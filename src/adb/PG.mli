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
  logging: bool;
}
[@@deriving sexp]

type pool

val load_config : ?override_password:string -> Filename.t -> params Lwt.t

val connect : params -> pool

val exec_file : Caqti_lwt.connection -> Filename.t -> unit Lwt.t

val disable_all_triggers : Caqti_lwt.connection -> unit Lwt.t

val enable_all_triggers : Caqti_lwt.connection -> unit Lwt.t

val with_no_triggers : Caqti_lwt.connection -> f:(unit -> 'a Lwt.t) -> 'a Lwt.t

val transaction :
  ?no_queue:Source_code_position.t ->
  pool ->
  f:(conn:Caqti_lwt.connection -> 'a Or_error.t Lwt.t) ->
  'a Or_error.t Lwt.t

val exec : Caqti_lwt.connection -> (Caqti_lwt.connection -> ('b, Caqti_error.t) Lwt_result.t) -> 'b Lwt.t
