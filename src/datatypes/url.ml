open! Core_kernel

type t = Uri.t

let sexp_of_t json = Sexp.Atom (Uri.to_string json)

let t_of_sexp = function
| Sexp.Atom s -> Uri.of_string s
| sexp -> failwithf "Invalid serialized Url: %s" (Sexp.to_string sexp) ()

let to_yojson url = `String (Uri.to_string url)

let of_yojson = function
| `String s -> Ok (Uri.of_string s)
| _ -> Error (Format.sprintf "Invalid JSON type for Url, expected String")

let encode x = Ok (Uri.to_string x)

let decode s = Ok (Uri.of_string s)

let t = Caqti_type.(custom ~encode ~decode string)
