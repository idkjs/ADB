# ADB
#### Asemio Database tools

## Install
Add to your OPAM file:
```
# You may need to add the pin-depends section:
pin-depends: [
  [ "ADB.1.0.0" "git+https://github.com/asemio/ADB.git#v1.0.0" ]
]

# Add to the depends section:
depends: [
  "ADB" { = "1.0.0" }
]
```

## Example

**initial_schema.ml**
```ocaml
open! Core_kernel

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

```

**initial_schema.mli**
```ocaml
include ADB.S.Migration
```

**example.ml**
```ocaml
open! Core_kernel
open! Lwt_result.Syntax
open! Lwt.Infix
open ADB

let migrations = [ (module Migrations.Initial_schema : S.Migration) ]

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
        | errors -> Lwt_result.fail @@ Error.of_list errors)
  in
  Lwt_io.printl ([%sexp_of: Testdate.t list] testdates |> Sexp.to_string_hum) >|= Result.return

```
## Example Reason

**initial_schema.re**
```reason
open! Core_kernel;

let created_at =
  Datatypes.Time.to_ptime_exn(Time.of_string("2020-01-07 10:53:00-06:00"));

let name = "Initialize database tables";

let apply = (conn): Lwt.t(unit) => {
  open Lwt.Syntax;
  /* The pgcrypto extension is needed for GEN_RANDOM_UUID() */
  let* () =
    ADB.PG.exec(conn) @@
    [%rapper execute({sql|
CREATE EXTENSION IF NOT EXISTS pgcrypto;
|sql})]();
  /* Example table */
  ADB.PG.exec(conn) @@
  [%rapper
    execute(
      {sql|
      CREATE TABLE IF NOT EXISTS "testdates" (
        "id" UUID PRIMARY KEY DEFAULT GEN_RANDOM_UUID(),
        "foo" DATE NOT NULL
      )
      |sql},
    )
  ]();
};
```

**initial_schema.rei**
```reason
include ADB.S.Migration;
```

**example.re**
```reason
open! Core_kernel;
open! Lwt_result.Syntax;
open! Lwt.Infix;
open ADB;

let migrations = [((module Migrations.Initial_schema): (module S.Migration))];

module Testdate = {
  [@deriving (sexp, yojson, fields)]
  type t = {
    id: Datatypes.Uuid.t,
    foo: Datatypes.Date.t,
  };
};

let from_json = (~json, of_yojson) =>
  Yojson.Safe.from_string(json)
  |> of_yojson
  |> Result.map_error(~f=Error.of_string);

let run = config_path => {
  let* params = PG.load_config(config_path) >|= Result.return;
  /* Example 1: INSERT, manually listing every column */
  /* Example 2: INSERT, dynamically. This also allows bulk inserting a variable number of rows. */
  /* Example 3: SELECT, manually */
  /* Example 4: SELECT, dynamically */
  let pool = PG.connect(params);
  let* () = Migration.run(pool, [%here], migrations);
  let now = Datatypes.Time.now() |> Time.to_date(~zone=Time.Zone.utc);

  let* testdates =
    PG.transaction(
      pool,
      ~f=(~conn) => {
        open Lwt.Syntax;
        let* () =
          PG.exec(conn) @@
          [%rapper
            execute(
              {sql|
INSERT INTO "testdates" (id, foo) VALUES (%Datatypes.Uuid{key}, %Datatypes.Date{now})
        |sql},
            )
          ](
            ~now,
            ~key=Datatypes.Uuid.random_v4(),
          );
        let* () = {
          let json =
            [
              Testdate.{id: Datatypes.Uuid.random_v4(), foo: now},
              Testdate.{id: Datatypes.Uuid.random_v4(), foo: now},
            ]
            |> [%to_yojson: list(Testdate.t)]
            |> Yojson.Safe.to_string;

          PG.exec(conn) @@
          [%rapper
            execute(
              {sql|
              INSERT INTO "testdates" (
              SELECT * FROM JSONB_POPULATE_RECORDSET(NULL::"testdates", %string{json}) X
              )
              |sql},
            )
          ](
            ~json,
          );
        };
        let* _testdates =
          PG.exec(conn) @@
          [%rapper
            get_many(
              {sql|
              SELECT @Datatypes.Date{foo}, @Datatypes.Uuid{id} FROM testdates
              |sql},
              function_out,
            )
          ](
            Testdate.Fields.create,
            (),
          );
        let* testdates =
          PG.exec(conn) @@
          [%rapper
            get_many(
              {sql|
              SELECT ROW_TO_JSON(DATA) AS @string{json} FROM (
                SELECT id, NOW()::DATE AS foo FROM testdates
              ) DATA
              |sql},
              function_out,
            )
          ](
            from_json([%of_yojson: Testdate.t]),
            (),
          );
        let (oks, errors) = List.partition_result(testdates);
        switch (errors) {
        | [] => Lwt_result.return(oks)
        | errors => Lwt_result.fail @@ Error.of_list(errors)
        };
      },
    );
  Lwt_io.printl(
    [%sexp_of: list(Testdate.t)](testdates) |> Sexp.to_string_hum,
  )
  >|= Result.return;
};
