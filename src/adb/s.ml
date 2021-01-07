open! Core_kernel

type combined =
  [ `User_error of Error.t
  | `Unexpected_exception of Exn.t
  | `Caqti of Caqti_error.t
  | Caqti_error.connect
  ]

type pool = (Caqti_lwt.connection, combined) Caqti_lwt.Pool.t

type 'a stmt = Caqti_lwt.connection -> ('a, Caqti_error.t) Lwt_result.t

module type Migration = sig
  val created_at : Ptime.t

  val name : string

  val apply : Caqti_lwt.connection -> unit Lwt.t
end
