open! Core_kernel

(** Corresponds to a Postgresql TIMESTAMP column *)
type t = Time.t [@@deriving sexp, compare, equal, yojson]

include S.Storable with type t := Time.t and type encoding := Ptime.t

val now : unit -> t

val to_ptime_exn : t -> Ptime.t

val of_ptime_exn : Ptime.t -> t
