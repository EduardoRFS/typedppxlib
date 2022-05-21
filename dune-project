(lang dune 2.9)

(name typedppxlib)

(generate_opam_files true)

(package
 (name typedppxlib)
 (depends ocaml ppxlib ppx_optcomp ocaml-migrate-types))

(package
 (name ppx_overload)
 (depends typedppxlib))

(package
 (name ppx_debug)
 (depends typedppxlib))

(package
 (name ppx_macro)
 (depends typedppxlib))
