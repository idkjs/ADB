open! Core_kernel
open! Lwt.Syntax
open! Lwt.Infix

(* records the migration steps that have been completed *)
let create_migrations_table : unit -> unit S.stmt =
  [%rapper
    execute
      {sql|
      CREATE TABLE IF NOT EXISTS "migrations" (
        "id" SERIAL PRIMARY KEY ,
        "created_at" TIMESTAMP NOT NULL,
        "name" TEXT NOT NULL
      )|sql}]

let insert_migration ~created_at ~name : unit S.stmt =
  [%rapper
    execute
      {sql|
      INSERT INTO "migrations" ("created_at", "name") VALUES (%ptime{created_at}, %string{name})
    |sql}]
    ~created_at ~name

let last_step_timestamp : unit -> Ptime.t S.stmt =
  [%rapper
    get_one {sql|
    SELECT @ptime{COALESCE(MAX(created_at), TO_TIMESTAMP(0))} FROM migrations
  |sql}]

let run pool here migrations =
  let open Lwt.Syntax in
  PG.transaction ~no_queue:here pool ~f:(fun ~conn ->
      let* () = PG.exec conn @@ create_migrations_table () in
      let* time = PG.exec conn @@ last_step_timestamp () in
      List.fold migrations ~init:Lwt.return_unit ~f:(fun acc migration ->
          let module M = (val migration : S.Migration) in
          let* () = acc in
          Ptime.is_later M.created_at ~than:time |> function
          | true ->
            let* () = M.apply conn in
            let* () = PG.exec conn @@ insert_migration ~name:M.name ~created_at:M.created_at in
            Lwt.return_unit
          | false -> Lwt.return_unit)
      >|= Result.return)
