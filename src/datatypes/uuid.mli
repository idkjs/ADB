type t = Uuidm.t [@@deriving sexp, yojson]

val random_v4 : unit -> t

include S.Storable with type t := Uuidm.t and type encoding := string
