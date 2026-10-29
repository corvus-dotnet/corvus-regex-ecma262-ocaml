# corvus-regex-ecma262 for OCaml: design

An outline of the library as it is: what it offers, how the engine is built and why, and how it is tested and
measured. The [getting-started page](getting-started.md) shows how to use it.

## 1. What it is

An OCaml regular expression engine with ECMA-262's grammar and answers (ECMAScript 2025), over UTF-8 (`string`,
`utf8_text`) and UTF-16 (a `string` of UTF-16LE code units). It is a port of the corvus-regex-ecma262 Rust crate, with
its search kernels in C and no dependencies beyond the compiler's distribution.

**Why ECMA-262.** JSON Schema, OpenAPI and many wire formats specify their patterns as ECMAScript regular
expressions. `Str` and `Re` have their own grammars, and `Re` has no backreferences or lookaround, so a translation to
them can never be exact. An engine that reads ECMA-262 directly, held to V8's answers over a large data set
(section 6), needs no translation.

## 2. Public surface

**Names.** One module, `Corvus_regex_ecma262`: `compile` (a `result`) and `compile_exn`, `is_match`, `find`, `matches`
(a `Seq.t`), `count`, `captures` with `exec_into`, `replace`, `replace_all` and `split`, each with `_text` and `_utf16`
forms, and the position as `?pos` where JavaScript keeps `lastIndex`. A match is a `Match.t`, with its groups by number
and by name.

**Flags.** ECMA-262's, as the string `?flags`, and nothing else.

**Edition and Unicode version.** By default a pattern is read with today's grammar (`Current`) and V8's version of
Unicode (`Default`, 17 today). `?edition` and `?unicode_version` override either, independently: `Es2020` is the
grammar JSON Schema 2020-12 names, and the versions are `V13`, `V17`, `V18` and `Latest`. Both are resolved when the
pattern is compiled; `edition` and `unicode_version_of` read them back.

**Errors.** `compile` returns an `error`: `Invalid` (the reason), `Flags`, `Too_large` (with the limit) or
`Invalid_utf8`; `compile_exn` raises it as `Compile_error`. A match that reaches its limit raises `Match_timeout` or
`Match_memory`; text that is not UTF-8 raises `Invalid_argument`.

**ECMA-262 algorithms.** `replace` substitutes as `GetSubstitution` does, replacing every match only with `g`;
`replace_all` always does. `split` follows `RegExp.prototype[Symbol.split]`, with `None` for a group that did not take
part.

**Input.** A `string` is UTF-8, checked before it is read and refused when it is not, never read as U+FFFD;
`utf8_text` checks a text once, and may be a range of a string, held with no copy. The `_utf16` functions take
UTF-16LE code units, lone surrogates and all. Positions are 0-based, in the text's units. Nothing is transcoded.

**Allocation.** A warm `is_match`, `count` and `exec_into` (into a `captures` made once) allocate nothing: the working
memory is the domain's on OCaml 5 (in domain-local storage, taken by a compare-and-set so that two threads never share
it), with one kept by each pattern besides, its arrays grown to what a match needs and kept up to caps.

**Limits.** No pattern and no text can hang a program or overflow the stack.

- A match timeout per pattern (`?match_timeout`), or a process-wide default (`set_default_match_timeout`), bounds every
  match and caps its backtracking memory.
- Size limits on the pattern (`max_pattern_length`, `max_nesting`, `max_size`) are deterministic.
- The matcher keeps its choices on a stack of its own, and the parser's and compiler's recursion, bounded by
  `max_nesting`, fits a stack of 2 MB.

## 3. Engine

**One matcher, two encodings.** The matcher is written once (`vm.ml.in`) and compiled for each encoding by a dune
rule that puts the encoding's module before it, so the loop has no functor or closure call. Matching by UTF-16 code
unit over UTF-8 places the position between the halves of a four-byte character at its third byte.

**The matcher.** A backtracking matcher over a flat program, with V8's quick check of the next character at each
choice. A memo of the places that failed, taken into use once a match has made sixty-four steps a position, makes a
pattern without backreferences linear in the text. The clock is read every 4,096 steps.

**Where a match can start.** The search layer is the Rust crate's. OCaml has no vector intrinsics, so the kernels are C
(`src/cre_stubs.c`), called as externals marked `[@@noalloc]` with untagged ints, which costs a plain C call: SSE2 for
one to four bytes, up to four ranges of bytes, a pair of positions and a UTF-16 unit; SSSE3 for Teddy; AVX2, found when
the program starts, for a literal's two rarest bytes; and a NEON form of each on AArch64. Every kernel has a scalar
fallback, which any other processor takes. A literal at a fixed or bounded distance opens windows of starts.

**The automata.** For a pattern that needs no backtracking: an automaton over ASCII for a test from the start, an
anchored leftmost-first automaton for where a match from a start ends, a reverse automaton for the terms before a
window's anchors, and a forward automaton for a pattern with no literal. Each is built on first use, with its states
bounded.

## 4. Generated tables

The Unicode tables of each version (`ucd_tables<major>.ml`, `strings_tables<major>.ml`) and the property names of
ECMAScript 2020 (`es2020.ml`) are written from the Rust crate's by `tools/unicode`, and CI checks them against the
crate, so the engines carry the same tables.

## 5. Targets and layout

OCaml 4.14 and later, 64-bit, published on opam. CI builds and tests on OCaml 4.14 and 5.5 on Linux, 5.5 on Linux
arm64, macOS and Windows, the flambda and no-flat-float-array compilers opam-repository tests every package on, and
s390x under QEMU (a big-endian processor).

- `src/`: the library and its C kernels; `test/`: the tests, the README's example and the getting-started page's
  examples among them.
- `tools/dataset`: the test-data runner; `tools/stackprobe`: the stack probe; `tools/unicode`: the tables' writers;
  `tools/benchmarks`: the harness.

## 6. Testing

**The ECMA-262 test data.** [`ecma262-regex-test-data`](https://github.com/corvus-dotnet/ecma262-regex-test-data)
holds V8's answers: validity under each grammar, `test` answers, the positions of `exec`, the global search, `y`,
`replace` and `split`, and every `exec` call of test262. `tools/dataset` runs all of it over UTF-8 and UTF-16, with
none wrong, refused or at a limit: on every change, on OCaml 4.14 and 5.5, again with the memo from the first step, and
as the data's own scripts write it for Unicode 13 and 18 and for ECMAScript 2020.

**Other tests.** Tests over the API and over the searches (again with Teddy's byte shuffle and the 32-byte loads turned
off); a test that a warm match allocates nothing, counted by `Gc.counters`; a test of domains matching at once; the
README's and the getting-started page's examples; and the stack probe, which asks 964 deep, wide and long patterns
through every call on stacks of 4 MB and 2 MB, none overflowing.

## 7. Benchmarks

The harness image, `ghcr.io/corvus-dotnet/corvus-regex-ecma262-ocaml`, runs the Industry workloads of
`dotnet/performance` over this library (UTF-8 and UTF-16), `re`, `pcre2` and V8. The published figures are on
[the benchmark page](https://corvus-oss.org/corvus-regex-ecma262-benchmarks/), which runs every engine's image each
night; this repository publishes none of its own.

Three rows are not like for like with an engine that is not ECMA-262's, as the price of its rules under u: `.*` (`.`
excludes CR, so the CRLF text has more matches), `Sher[a-z]+|Hol[a-z]+` under i (the search must also find the long s
and the Kelvin sign), and `[a-q][^u-z]{13}x` (a supplementary character is two units, so the distance to the `x` is
not fixed).

## 8. Differences an OCaml user will meet

- The grammar is ECMA-262's, not `Str`'s or `Re`'s: backreferences and lookaround, and `(?i:...)` groups set flags
  inside a pattern.
- A compiled pattern holds no state, so it is shared between threads and domains, which `Str` is not.
- `Re` is linear for every pattern; here a pattern with a backreference is not, so a pattern from outside is given a
  timeout.

The [getting-started page](getting-started.md) has the full list.
