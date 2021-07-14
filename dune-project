(lang dune 2.9)

(name typedppxlib)

(generate_opam_files true)

(package
 (name typedppxlib)
 (depends
  (ocaml
   (and
    (>= "4.08.0")
    (< "4.13.0")))
  ppxlib))
