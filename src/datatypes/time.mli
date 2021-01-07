open! Core_kernel

type t = Time.t [@@deriving sexp, yojson]

include S.Storable with type t := Time.t and type encoding := Ptime.t
