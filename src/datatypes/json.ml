open! Core_kernel

type t = Yojson.Safe.t [@@deriving equal, yojson]

let sexp_of_t x : Sexp.t = Atom (Yojson.Safe.to_string x)

let encode x = Ok (Yojson.Safe.to_string x)

let decode x = Ok (Yojson.Safe.from_string x)

let t = Caqti_type.(custom ~encode ~decode string)
