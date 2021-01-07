open! Core_kernel

type t = Date.t [@@deriving sexp, yojson]

include S.Storable with type t := Date.t and type encoding := Ptime.t
