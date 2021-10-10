(lang dune 2.9)
(name mlsub)
(source (github smimram/mlsub))
(license GPL-3.0)
(authors "Samuel Mimram <smimram@gmail.com>")
(maintainers "Samuel Mimram <smimram@gmail.com>")

(generate_opam_files true)

(package
 (name mlsub)
 (synopsis "A typechecker for MLsub")
 (description "A typechecker for a variant of ML with subtyping.")
 (depends
   menhir)
)
(using menhir 2.0)
