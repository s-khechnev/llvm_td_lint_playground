(lang dune 2.8)

(cram enable)

(generate_opam_files true)

(name checker)

(maintainers Kakadu)

(package
 (name checker)
 (synopsis "TODO")
 (description "TODO")
 (depends
  ppx_deriving_yojson
  ppx_expect
  linenoise
  (ocamlformat
   (= 0.26.2))
  (sail
   (= 0.17.1))))
