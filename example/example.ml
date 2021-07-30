open! Core_kernel
open! Lwt_result.Syntax
open! Lwt.Infix
open ADB

let migrations = [ (module Initial_schema : S.Migration) ]

module Testdate = struct
  type t = {
    id: Datatypes.Uuid.t;
    foo: Datatypes.Date.t;
  }
  [@@deriving sexp, yojson, fields]
end

let from_json ~json of_yojson =
  Yojson.Safe.from_string json |> of_yojson |> Result.map_error ~f:Error.of_string

let run config_path =
  let* params = PG.load_config config_path >|= Result.return in
  let pool = PG.connect params in
  let* () = Migration.run pool [%here] migrations in

  let now = Datatypes.Time.now () |> Time.to_date ~zone:Time.Zone.utc in

  let* testdates =
    PG.transaction pool ~f:(fun ~conn ->
        let open Lwt.Syntax in
        (* Example 1: INSERT, manually listing every column *)
        let* () =
          PG.exec conn
          @@ [%rapper
               execute
                 {sql|
INSERT INTO "testdates" (id, foo) VALUES (%Datatypes.Uuid{key}, %Datatypes.Date{now})
        |sql}]
               ~now ~key:(Datatypes.Uuid.random_v4 ())
        in
        (* Example 2: INSERT, dynamically. This also allows bulk inserting a variable number of rows. *)
        let* () =
          let json =
            [
              Testdate.{ id = Datatypes.Uuid.random_v4 (); foo = now };
              Testdate.{ id = Datatypes.Uuid.random_v4 (); foo = now };
            ]
            |> [%to_yojson: Testdate.t list]
            |> Yojson.Safe.to_string
          in
          PG.exec conn
          @@ [%rapper
               execute
                 {sql|
INSERT INTO "testdates" (
            SELECT * FROM JSONB_POPULATE_RECORDSET(NULL::"testdates", %string{json}) X
            )
            |sql}]
               ~json
        in
        (* Example 3: SELECT, manually *)
        let* _testdates =
          PG.exec conn
          @@ [%rapper
               get_many
                 {sql|
SELECT @Datatypes.Date{foo}, @Datatypes.Uuid{id} FROM testdates
            |sql}
                 function_out]
               Testdate.Fields.create ()
        in
        (* Example 4: SELECT, dynamically *)
        let* testdates =
          PG.exec conn
          @@ [%rapper
               get_many
                 {sql|
SELECT ROW_TO_JSON(DATA) AS @string{json} FROM (
  SELECT id, NOW()::DATE AS foo FROM testdates
) DATA
|sql}
                 function_out]
               (from_json [%of_yojson: Testdate.t])
               ()
        in
        let oks, errors = List.partition_result testdates in
        match errors with
        | [] -> Lwt_result.return oks
        | errors -> Lwt_result.fail @@ Error.of_list errors
    )
  in
  Lwt_io.printl ([%sexp_of: Testdate.t list] testdates |> Sexp.to_string_hum) >|= Result.return
