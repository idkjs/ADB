type t = Uuidm.t [@@deriving sexp, yojson]

include S.Storable with type t := Uuidm.t and type encoding := string
