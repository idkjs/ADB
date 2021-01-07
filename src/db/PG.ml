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
}
[@@deriving sexp]

let load_file ?override_password path =
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

let enable_statement_logging : unit S.stmt =
  [%rapper execute {sql|
  SET LOCAL log_statement = 'all';
  |sql}] ()

let start_transaction : unit S.stmt = [%rapper execute {sql|BEGIN;|sql}] ()

let revert_transaction : unit S.stmt = [%rapper execute {sql|ROLLBACK;|sql}] ()

let commit_transaction : unit S.stmt = [%rapper execute {sql|COMMIT;|sql}] ()

let lift_user_error err = Lwt_result.map_err (fun x -> `User_error x) err

let lift_unexpected_exn exn = Lwt.return_error (`Unexpected_exception exn)

let lift_caqti_error err = Lwt_result.map_err (fun x -> `Caqti x) err

let combined_to_error = function
| `Unexpected_exception exn -> Error.of_exn exn
| `User_error e -> e
| `Caqti caqti
 |(`Connect_rejected _ as caqti)
 |(`Post_connect _ as caqti)
 |(`Connect_failed _ as caqti) ->
  Caqti_error_safe.to_error caqti

(*
User_error: business logic error as opposed to DB or system errors.
We want User_error to be Lwt_results, but all other errors to be exceptions.
Exceptions are caught by the router, and the server responds with a generic 500 Internal Server Error.
In the case of Lwt_results, the server responds with a 400 Bad Request and a detailed error message.
*)
let transaction ?(log_statements = false) (pool : S.pool) ~f =
  Caqti_lwt.Pool.use
    (fun conn ->
      let* () = exec conn start_transaction in
      let* () = if log_statements then exec conn enable_statement_logging else Lwt.return_unit in
      (* BUSINESS LOGIC *)
      Lwt.catch
        (* Lift exceptions into Lwt_result of "combined error" so they rollback too *)
          (fun () -> f ~conn |> lift_user_error)
        lift_unexpected_exn
      >>= function
      (* BUSINESS LOGIC SUCCESS: COMMIT OR RAISE *)
      | Ok _ as res ->
        let+ () = exec conn commit_transaction in
        res
      (* BUSINESS LOGIC ERROR: ROLLBACK *)
      | Error (`User_error _ as err1) as res -> (
        revert_transaction conn |> lift_caqti_error >|= function
        (* Sucessful rollback: continue with the Lwt_result *)
        | Ok () -> res
        (* Failure to rollback: raise an exception *)
        | Error err2 -> List.map ~f:combined_to_error [ err1; err2 ] |> Error.of_list |> Error.raise
      )
      (* DB OR SYSTEM ERROR: ROLLBACK *)
      | Error err1 ->
        revert_transaction conn
        |> lift_caqti_error
        >|= (function
              (* Sucessful rollback: pass the original error... *)
              | Ok () -> combined_to_error err1
              | Error err2 ->
                (* Failure to rollback: append to the original error... *)
                List.map ~f:combined_to_error [ err1; err2 ] |> Error.of_list)
        (* ...raise exception! *)
        >|= Error.raise)
    pool
  |> Lwt_result.map_err combined_to_error

let connection_uri { host; dbname = path; user; password; ssl; max_pool_size = _ } =
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

let connect params : (Caqti_lwt.connection, S.combined) Caqti_lwt.Pool.t =
  connection_uri params |> Caqti_lwt.connect_pool ~max_size:params.max_pool_size |> function
  | Ok pool -> pool
  | Error err -> raise_caqti_error ~it_is:`Safe_to_show_internal_error_details err
