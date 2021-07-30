(lang dune 2.0)

(name ADB)

(version 1.4.2)

(generate_opam_files true)

(license MIT)

(authors "Asemio")

(maintainers "support@asemio.com")

(package
 (name ADB)
 (synopsis "Asemio's DB lib")
 (depends
  (lwt
   (>= 5.4.1))
  (core
   (>= 0.14.1))
  (core_kernel
   (>= 0.14.1))
  (caqti
   (>= 1.6.0))
  (caqti-lwt
   (>= 1.6.0))
  (caqti-driver-postgresql
   (>= 1.6.0))
  (ppx_deriving_yojson
   (>= 3.6.1))
  (re
   (>= 1.9.0))
  (ppx_jane
   (>= 0.14.0))
  (uri
   (>= 0.14.0))
  (ppx_rapper
   (>= 3.0.0))
  (ppx_rapper_lwt
   (>= 3.0.0))
  (uuidm
   (>= 0.9.7))
  ;; Dev dependencies
  (ocaml-lsp-server :dev)))
