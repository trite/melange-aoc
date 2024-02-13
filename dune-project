(lang dune 3.11)

(using melange 0.1)

(name melange-aoc)

(license MIT)

(generate_opam_files true)

(source
 (github trite/melange-aoc))

(documentation https://github.com/trite/melange-aoc)

(maintainers "Paul Brunner")

(authors "Paul Brunner")

(homepage https://github.com/trite/melange-aoc)

(bug_reports https://github.com/trite/melange-aoc/issues)

(package
 (allow_empty)
 (name melange-aoc)
 (synopsis "Advent of Code in ReasonML")
 (description "Advent of Code in ReasonML, compiled with Melange")
 (depends
  (dune
   (>= 3.11))
  (melange
   (>= 2.1.0))
  (ocaml
   (>= 5.1.0))
  opam-check-npm-deps
  dot-merlin-reader
  (ocamlformat
   (>= 0.26.1))
  ppx_deriving
  (reason
   (>= 3.10.0))
  (reason-react
   (>= 0.13.0))
  reason-react-ppx
  relude))
