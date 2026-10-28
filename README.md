# corvus-regex-ecma262-ocaml

Regular expressions with the grammar and the answers of ECMA-262, the `RegExp` of JavaScript, for OCaml (4.14 and
later, 64-bit).

- Every flag: `d`, `g`, `i`, `m`, `s`, `u`, `v` and `y`, with the grammar of each. Without `u` or `v` a pattern is
  read by the Annex B grammar and matched by UTF-16 code unit, as ECMA-262 matches it; with them, by code point. The
  `v` flag has nested classes, the difference and the intersection of classes, strings in a class and the seven
  properties of strings.
- ECMAScript 2025: lookbehind, named groups (and a name shared between alternatives), modifier groups such as
  `(?i:...)`, and every Unicode property ECMA-262 lists, from tables of Unicode 17 the library carries, so a pattern
  answers the same whichever compiler built it.
- The grammar of ECMAScript 2020, which JSON Schema 2020-12 names, and a choice of Unicode version, each independent
  of the other: `compile` takes an `?edition` (`Current`, the default, or `Es2020`) and a `?unicode_version` (`Default`
  for V8's version, which is 17 today, or `V13`, `V17`, `V18` or `Latest`, which is 18 today). Both are settled when a
  pattern is compiled and cost a match nothing. The 2020 grammar refuses what later editions added: the `d` and `v`
  flags, modifier groups, a group name used twice, and property names outside its tables. The tables of a version are
  made when a pattern first asks for them.
- Text as UTF-8 (a `string`, or a checked `utf8_text`, which may be a range of a string, checked once and held with no
  copy) or as UTF-16 (a `string` of UTF-16LE code units, lone surrogates and all). The function chooses the encoding,
  and nothing is transcoded. Text that is not UTF-8 is refused, never read as U+FFFD. One matcher serves both
  encodings: it is written once and compiled for each.
- The methods of a `RegExp`: `is_match`, `find` (a match with its groups, by number and by name), `matches` (a
  `Seq.t`), `count`, `replace` (`String.prototype.replace`, with every `$` form of `GetSubstitution`), `replace_all`
  and `split` (`RegExp.prototype[Symbol.split]`, which puts the groups between the pieces). Positions are 0-based
  offsets in the text's units, bytes or UTF-16 code units. `is_match`, `count` and a match into a `captures` allocate
  nothing once the working memory has grown.
- The searches of the Rust crate for where a match can start (literals by their rarest bytes, Teddy, the pair search,
  windows of starts, the anchored, forward and reverse automata), with their kernels in C: SSE2, SSSE3 and AVX2 on
  x86-64, NEON on arm64, and a scalar fallback on any other processor.
- `compile` returns a `result`, and takes a match timeout of its own, so a pattern from outside is tried without an
  exception and bounded without a global setting.
- A pattern without a backreference is matched in time proportional to its size times the text. A pattern can be given
  a match timeout, as a `Regex` of .NET can, which bounds the time and the backtracking memory of every match. With a
  timeout and the library's limits of pattern size, neither a pattern nor a text from outside can make a program hang.
- A compiled pattern is shared freely between threads (OCaml 4.14) and domains (OCaml 5).
- No dependencies beyond the compiler's distribution.

**Compliance.** Over the [ECMA-262 test data](https://github.com/corvus-dotnet/ecma262-regex-test-data) (what V8 says
about its patterns and texts, test262's patterns and `exec` calls, the global search, `replace` and `split` among them,
and texts of bytes that are not UTF-8, which it refuses), the library gives every answer right and none wrong, refuses
no valid pattern, accepts no invalid one and reaches no limit, over UTF-8 and over UTF-16. It does the same on the data
as the data's own scripts write it for Unicode 13 and 18 and for the grammar of ECMAScript 2020. CI runs the whole data
on every change, on OCaml 4.14 and 5.5.

It is a port of the Rust crate [corvus-regex-ecma262](https://github.com/corvus-dotnet/corvus-regex-ecma262-rs), and it
follows the design of the .NET engine [Corvus.Regex.Ecma262](https://github.com/corvus-dotnet/Corvus.Regex.Ecma262).

```ocaml
module R = Corvus_regex_ecma262

let () =
  let re = R.compile_exn ~flags:"u" {|(?<year>\d{4})-(?<month>\d{2})|} in
  assert (R.is_match re "on 2026-10-25");
  assert (R.replace re "2026-10-25" "$<month>/$<year>" = "10/2026-25");
  (match R.find re "on 2026-10-25" with
   | Some m -> assert (R.Match.named m "month" = Some "10" && R.Match.start m = 3)
   | None -> assert false);
  (* A pattern from outside: no exception, and a timeout of its own. *)
  (match R.compile ~flags:"u" ~match_timeout:0.1 "[a-z" with
   | Error e -> print_endline (R.error_message e)
   | Ok _ -> ());
  (* ECMAScript 2020 as its edition was published, with the Unicode of its year. *)
  let greek = R.compile_exn ~flags:"u" ~edition:R.Es2020 ~unicode_version:R.V13 {|^\p{Script=Greek}+$|} in
  assert (R.is_match greek "αβγ");
  assert (not (R.is_valid ~edition:R.Es2020 "(?i:a)"))
```

A match that reaches its limit raises `Match_timeout` or `Match_memory`; text that is not UTF-8 raises
`Invalid_argument`.

## Status

0.1.0 is the first release. The parser, the compiler, the matcher and the crate's search layer (where a match can
start, and the automata) are in, the whole test data passes, and the central benchmark site runs the harness each
night. `docs/port-checklist.md` lists every optimisation of the search layer and how this port has it, and the rows
still slower than V8.

## Installing

```sh
opam install corvus-regex-ecma262
```

and name the library in dune:

```
(executable
 (name main)
 (libraries corvus-regex-ecma262))
```

The package needs a C compiler, which `ocamlopt` needs anyway, for the search kernels.

## Benchmarks

The [central benchmark site](https://corvus-oss.org/corvus-regex-ecma262-benchmarks/) runs this repository's harness
image, `ghcr.io/corvus-dotnet/corvus-regex-ecma262-ocaml`, each night beside the engines of every other source: this
library over UTF-8 and UTF-16, `re` (ocaml-re) and `pcre2`, which do not claim ECMA-262, and V8, over the Industry
workloads of dotnet/performance, with each ECMA-262 engine's compliance over the test data. This repository publishes
no figures of its own. `tools/benchmarks/run.sh <workloads.json> <texts> <out.json>` runs the same harness anywhere
(see the script for what it needs).

## Building and testing

```
dune build
dune test
dune build --profile release ./tools/dataset/run.exe
./_build/default/tools/dataset/run.exe <ecma262-regex-test-data>/data wrong-cases.tsv
dune build --profile release ./tools/stackprobe/stackprobe.exe
./tools/stackprobe/run.sh
```

`dune test` runs the tests of the API, the tests of the searches (each finds what trying every position finds) and the
test that a warm match allocates nothing. The stack probe asks about patterns built to overflow the stack, on stacks of
4 MB and 2 MB (`docs/port-checklist.md`, which also lists every optimisation of the search layer).

`CORVUS_REGEX_MEMO_FROM_THE_START=1` makes every match take its memory of failed places from its first step, which CI
does for a second run of both. The Unicode tables of each version (`src/ucd_tables<major>.ml` and
`src/strings_tables<major>.ml`) and the property names of ECMAScript 2020 (`src/es2020.ml`) are written from the Rust
crate's by `tools/unicode`, and CI checks they are current.

## Releasing

Every pull request merged into main without the `no_release` label releases the library with the next patch version
(`.github/workflows/auto-release.yml`, `tools/release`): a `v<version>` tag on a commit of the merge with the version
written into `dune-project`, `corvus-regex-ecma262.opam` and `src/corvus_regex_ecma262.ml`, which main keeps at 0.0.0.
A pull request that needs a minor or major version sets `nextVersion` in `.github/release.json`.

## License

Apache 2.0.
