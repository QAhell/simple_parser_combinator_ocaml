(lang dune 3.4)

(name simple_parser_combinator)

(generate_opam_files true)

(source
 (github QAhell/simple_parser_combinator_ocaml))

(authors "Armin Heller")

(maintainers "Armin Heller")

(license "Apache-2.0")

(documentation https://github.com/QAhell/simple_parser_combinator_ocaml)

(implicit_transitive_deps false)

(package
 (name simple_parser_combinator)
 (version 0.1)
 (synopsis "Simple recursive descent parser combinators")
 (description "Simple recursive descent parser combinators")
 (depends ocaml dune)
 (tags (topics parser combinator)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
