open! Core
open Lwt.Infix
open Lwt.Syntax

type ssl = {
  key: Filename.t;
  cert: Filename.t;
  ca: Filename.t;
}
[@@deriving sexp]

type host =
  | IP     of {
      hostname: string;
      port: int;
    }
  | Socket of Filename.t
[@@deriving sexp]

type params = {
  host: host;
  dbname: string;
  user: string;
  password: string;
  max_pool_size: int;
  ssl: ssl option;
  logging: bool;
}
[@@deriving sexp]

module SCPT = Source_code_position.Table

let transaction_count scpt = SCPT.fold scpt ~init:0 ~f:(fun ~key:_ ~data acc -> acc + data)

type pool = {
  pool: (Caqti_lwt.connection, S.combined) Caqti_lwt.Pool.t;
  params: params;
  current_transactions: int Source_code_position.Table.t;
  log_statements: bool;
}

let load_config ?override_password path =
  let+ config =
    Lwt_io.with_file
      ~flags:Unix.[ O_RDONLY; O_NONBLOCK ]
      ~mode:Input path
      (fun ic ->
        let+ s = Lwt_io.read ic in
        Sexp.of_string_conv_exn s [%of_sexp: params])
  in
  Option.value_map override_password ~default:config ~f:(fun password -> { config with password })

let raise_caqti_error ?(it_is : [ `Safe_to_show_internal_error_details ] option) err =
  (if Option.is_some it_is then Caqti_error.show else Caqti_error_safe.show) err |> failwith

let exec conn (stmt : 'a S.stmt) =
  stmt conn >|= function
  | Ok res -> res
  | Error err -> raise_caqti_error ~it_is:`Safe_to_show_internal_error_details err

let regex_separator = Re.Perl.re {|-- \*\* --|} |> Re.compile

let exec_file conn path =
  let open Lwt.Syntax in
  let* contents = Lwt_io.with_file path ~flags:[ O_RDONLY; O_NONBLOCK ] ~mode:Input Lwt_io.read in
  let stmts = Re.split regex_separator contents in
  Lwt_list.iter_s
    (fun (stmt : string) ->
      let query (module C : Caqti_lwt.CONNECTION) =
        C.exec (Caqti_request.exec ~oneshot:true Caqti_type.unit stmt) ()
      in
      exec conn query)
    stmts

let disable_all_triggers conn =
  [%rapper execute {sql|
  SET session_replication_role = replica;
  |sql}] () |> exec conn

let enable_all_triggers conn =
  [%rapper execute {sql|
  SET session_replication_role = DEFAULT;
  |sql}] () |> exec conn

let with_no_triggers conn ~f =
  let open Lwt.Syntax in
  Lwt.finalize
    (fun () ->
      let* () = disable_all_triggers conn in
      f ())
    (fun () -> enable_all_triggers conn)

let enable_statement_logging : unit S.stmt =
  [%rapper execute {sql|
  SET LOCAL log_statement = 'all';
  |sql}] ()

let combined_to_error = function
| `Unexpected_exception exn -> Error.of_exn exn
| `User_error e -> e
| `Mixed_error ll -> Error.of_list ll
| `Caqti caqti
 |(`Connect_rejected _ as caqti)
 |(`Post_connect _ as caqti)
 |(`Connect_failed _ as caqti) ->
  Caqti_error.show caqti |> Error.of_string

let lift_user_error err = Lwt_result.map_err (fun x -> `User_error x) err

let lift_mixed_errors (ll : S.combined list) : ('a, S.combined) result =
  Error (`Mixed_error (ll |> List.map ~f:combined_to_error))

let lift_unexpected_exn exn = Lwt.return_error (`Unexpected_exception exn)

let lift_caqti_error err = Lwt_result.map_err (fun x -> `Caqti x) err

(*
User_error: business logic error as opposed to DB or system errors.
We want User_error to be Lwt_results, but all other errors to be exceptions.
Exceptions are caught by the router, and the server responds with a generic 500 Internal Server Error.
In the case of Lwt_results, the server responds with a 400 Bad Request and a detailed error message.
*)
let do_rollback ~rollback result error =
  rollback () |> lift_caqti_error >|= function
  (* Sucessful rollback: pass the original error... *)
  | Ok () -> result
  | Error err2 -> lift_mixed_errors [ error; err2 ]

let do_transaction { pool; log_statements; _ } ~f () =
  Caqti_lwt.Pool.use
    (fun conn ->
      let module M = (val conn : Caqti_lwt.CONNECTION) in
      (* BUSINESS LOGIC *)
      Lwt.catch
        (* Lift exceptions into Lwt_result of "combined error" so they rollback too *)
          (fun () ->
          let open Lwt_result.Syntax in
          let* () = M.start () |> lift_caqti_error in
          let* () =
            (* TODO: revisit once we have log levels *)
            if log_statements
            then enable_statement_logging conn |> lift_caqti_error
            else Lwt_result.return ()
          in
          f ~conn |> lift_user_error)
        lift_unexpected_exn
      >>= function
      (* BUSINESS LOGIC SUCCESS: COMMIT OR ROLLBACK *)
      | Ok _ as res -> (
        M.commit () |> lift_caqti_error >>= function
        | Ok () -> Lwt.return res
        | Error err1 as res -> do_rollback ~rollback:M.rollback res err1
      )
      (* ERROR: ROLLBACK *)
      | Error err1 as res -> do_rollback ~rollback:M.rollback res err1)
    pool
  |> Lwt_result.map_err (function
       | `User_error err -> err
       | `Unexpected_exception exn -> raise exn
       | x -> combined_to_error x)

let transaction ?no_queue ({ params = { max_pool_size; _ }; _ } as p) ~f =
  match no_queue with
  | None -> do_transaction p ~f ()
  | Some here when transaction_count p.current_transactions >= max_pool_size ->
    failwithf
      !"Reached max_pool_size (%d) due to a call at %{Source_code_position}. Other call sites: %{sexp: \
        int SCPT.t}"
      max_pool_size here p.current_transactions ()
  | Some here ->
    SCPT.incr p.current_transactions here;
    Lwt.finalize (do_transaction p ~f) (fun () ->
        SCPT.decr ~remove_if_zero:true p.current_transactions here;
        Lwt.return_unit)

let connection_uri { host; dbname = path; user; password; ssl; max_pool_size = _; logging = _ } =
  let host, port =
    match host with
    | IP { hostname; port } -> hostname, Some port
    | Socket sock -> sock, None
  in
  let query =
    Option.value_map ssl ~default:[] ~f:(fun { key; cert; ca } ->
        [ "sslmode", [ "verify-ca" ]; "sslkey", [ key ]; "sslcert", [ cert ]; "sslrootcert", [ ca ] ])
  in
  Uri.make ~scheme:"postgresql" ~userinfo:(sprintf "%s:%s" user password) ~host ?port ~path ~query ()

let connect params : pool =
  connection_uri params |> Caqti_lwt.connect_pool ~max_size:params.max_pool_size |> function
  | Ok pool -> { pool; params; current_transactions = SCPT.create (); log_statements = params.logging }
  | Error err -> raise_caqti_error ~it_is:`Safe_to_show_internal_error_details err
