open! Core_kernel

type t = Uri.t [@@deriving sexp, compare, equal, yojson]

include S.Storable with type t := Uri.t and type encoding := string
