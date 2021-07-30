open! Core_kernel

(* open ADB *)
let created_at = Datatypes.Time.to_ptime_exn (Time.of_string "2020-01-07 10:53:00-06:00")

let name = "Initialize database tables"

let apply conn : unit Lwt.t =
  let open Lwt.Syntax in
  (* The pgcrypto extension is needed for GEN_RANDOM_UUID() *)
  let* () =
    ADB.PG.exec conn @@ [%rapper execute {sql|
CREATE EXTENSION IF NOT EXISTS pgcrypto;
|sql}] ()
  in
  (* Example table *)
  ADB.PG.exec conn
  @@ [%rapper
       execute
         {sql|
CREATE TABLE IF NOT EXISTS "testdates" (
  "id" UUID PRIMARY KEY DEFAULT GEN_RANDOM_UUID(),
  "foo" DATE NOT NULL
)
|sql}]
       ()
