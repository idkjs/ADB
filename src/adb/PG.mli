open! Core_kernel

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

val load_file : ?override_password:string -> Filename.t -> params Lwt.t

val connect : params -> (Caqti_lwt.connection, S.combined) Caqti_lwt.Pool.t

val transaction :
  ?log_statements:bool ->
  S.pool ->
  f:(conn:Caqti_lwt.connection -> 'a Or_error.t Lwt.t) ->
  'a Or_error.t Lwt.t

val exec : Caqti_lwt.connection -> (Caqti_lwt.connection -> ('b, Caqti_error.t) Lwt_result.t) -> 'b Lwt.t
