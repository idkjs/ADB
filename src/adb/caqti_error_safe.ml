open! Core_kernel
open Caqti_error

(*
This is a subset of the features provided by Caqti_error,
with the added guarantee that the error messages will not leak
information such as the connection URI or the query involved
*)

let pp_load_msg ppf fmt (err : load_error) =
  Format.fprintf ppf fmt;
  Format.pp_print_string ppf ": ";
  pp_msg ppf err.msg

let pp_connection_msg ppf fmt (err : connection_error) =
  Format.fprintf ppf fmt;
  Format.pp_print_string ppf ": ";
  pp_msg ppf err.msg

let pp_coding_error ppf fmt (err : coding_error) =
  Format.fprintf ppf fmt Caqti_type.pp_any err.typ;
  Format.pp_print_string ppf ": ";
  pp_msg ppf err.msg

let pp_query_msg ppf fmt (err : query_error) =
  Format.fprintf ppf fmt;
  Format.pp_print_string ppf ": ";
  pp_msg ppf err.msg

let rec pp : 'a. _ -> ([< Caqti_error.t ] as 'a) -> unit =
 fun ppf -> function
  | `Load_rejected err -> pp_load_msg ppf "Cannot load driver" err
  | `Load_failed err -> pp_load_msg ppf "Failed to load driver" err
  | `Connect_rejected err -> pp_connection_msg ppf "Cannot connect" err
  | `Connect_failed err -> pp_connection_msg ppf "Failed to connect" err
  | `Post_connect err ->
    Format.pp_print_string ppf "During post-connect: ";
    pp ppf err
  | `Encode_rejected err -> pp_coding_error ppf "Cannot encode %a" err
  | `Encode_failed err -> pp_coding_error ppf "Failed to bind %a" err
  | `Decode_rejected err -> pp_coding_error ppf "Cannot decode %a" err
  | `Request_rejected err -> pp_query_msg ppf "Request rejected" err
  | `Request_failed err -> pp_query_msg ppf "Request failed" err
  | `Response_failed err -> pp_query_msg ppf "Response failed" err
  | `Response_rejected err -> pp_query_msg ppf "Unexpected result" err

let show err =
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  pp ppf err;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let to_error err = show err |> Error.of_string
