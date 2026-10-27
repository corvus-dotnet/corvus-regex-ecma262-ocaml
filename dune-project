(lang dune 3.0)

(name corvus-regex-ecma262)

; The version of the package. Main keeps 0.0.0, and a release writes its own (.github/release.json).
(version 0.0.0)

(generate_opam_files true)

(license Apache-2.0)

(authors "Endjin Limited")

(maintainers "Endjin Limited <hello@endjin.com>")

(source
 (github corvus-dotnet/corvus-regex-ecma262-ocaml))

(package
 (name corvus-regex-ecma262)
 (synopsis "ECMA-262 (JavaScript) regular expressions: the grammar and the answers of RegExp, over UTF-8 and UTF-16")
 (description
  "Regular expressions with the grammar and the answers of ECMA-262, the RegExp of JavaScript: every flag, lookbehind, named groups, modifier groups, Unicode property escapes and the v flag's set notation, matched over UTF-8 or UTF-16 text with no copy, in time proportional to the pattern times the text for a pattern without backreferences. A port of the corvus-regex-ecma262 Rust crate.")
 (depends
  (ocaml
   (>= 4.14))
  (dune
   (>= 3.0))))
