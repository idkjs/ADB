open! Core_kernel

(** The second argument should look like [[(module Initial_schema : S.Migration)]] *)
val run : S.pool -> (module S.Migration) list -> unit Core_kernel.Or_error.t Lwt.t
