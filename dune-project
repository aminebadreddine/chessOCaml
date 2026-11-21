(lang dune 3.24)

(name chessOCaml)

(generate_opam_files true)

(source
 (github aminebadreddine/chessOCaml))

(authors
 "Yedidia Alebachew"
 "Xavier Blake-Forbes"
 "Amine Badreddine"
 "Shehroze"
 "Jah")

(maintainers "Yedidia Alebachew")

(license MIT)

(package
 (name chessOCaml)
 (synopsis "A chess engine in OCaml")
 (description
  "A chess engine with an immutable board representation, pseudo-legal move generation, a legality layer, terminal and SVG rendering, and a rule-based opponent.")
 (depends ocaml)
 (tags
  (chess games ocaml)))
