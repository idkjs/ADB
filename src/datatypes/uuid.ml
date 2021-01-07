open! Core_kernel

type t = Uuidm.t

let caml_state = Caml.Random.State.make (Array.init 10 ~f:(fun _ -> Random.int Int.max_value))

let random_v4 () = Uuidm.v4_gen caml_state ()

let sexp_of_t x = Sexp.Atom (Uuidm.to_string x)

let t_of_sexp sexp =
  begin
    match sexp with
    | Sexp.Atom s -> Uuidm.of_string s
    | _ -> None
  end
  |> Option.value_exn ~message:(sprintf "Error: invalid Uuid sexp: %s" (Sexp.to_string_hum sexp))

let to_yojson x = `String (Uuidm.to_string x)

let of_yojson = function
| `String s -> Uuidm.of_string s |> Result.of_option ~error:(sprintf "Error: invalid Uuid string: %s" s)
| json -> Error (sprintf "Error: invalid Uuid json: %s" (Yojson.Safe.to_string json))

let encode x = Ok (Uuidm.to_string x)

let decode s = Uuidm.of_string s |> Result.of_option ~error:(sprintf "Error: invalid Uuid string: %s" s)

let t = Caqti_type.(custom ~encode ~decode string)
