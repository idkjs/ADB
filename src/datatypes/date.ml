open! Core_kernel

type t = Date.t

let sexp_of_t date = Sexp.Atom (Date.to_string date)

let t_of_sexp = function
| Sexp.Atom s -> Date.of_string s
| sexp -> failwithf "Invalid serialized Date: %s" (Sexp.to_string sexp) ()

let to_yojson date = `String (Date.to_string date)

let of_yojson = function
| `String s -> Ok (Date.of_string s)
| _ -> Error (sprintf "Invalid JSON for Date, expected String")

let encode date =
  let days_epoch = Date.diff date Date.unix_epoch in
  match Ptime.Span.of_d_ps (days_epoch, 0L) with
  | None -> Error "Ptime date encoding error"
  | Some span -> Ptime.of_span span |> Result.of_option ~error:"Ptime date span encoding error"

let decode x =
  let days_epoch, _picos_midnight = Ptime.to_span x |> Ptime.Span.to_d_ps in
  Result.return @@ Date.(add_days unix_epoch days_epoch)

let t = Caqti_type.(custom ~encode ~decode pdate)
