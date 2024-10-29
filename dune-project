(lang dune 3.16)

(name guillmet)

(generate_opam_files true)

(source
 (github Chubek/Guillmet))

(authors "Chubak Bidpaa")

(maintainers "Chubak Bidpaa")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name guillmet)
 (synopsis "An implementation of the Scheme R7RS specifications, targeting x86-64")
 (description "Guillmet is yet another Scheme, that aims to produce correct, efficient and fast code in x86-64. It adds several new additions to the base R7RS specifications.")
 (depends ocaml dune)
 (tags
  (scheme plt fp "functional programming" "lisp" "s-expression" sexp x86-64)))

