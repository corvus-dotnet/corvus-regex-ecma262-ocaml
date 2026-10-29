# Contributing

This guide is for someone who wants to build the library, run its tests and gates, and open a pull request. It covers
the tools, the build step by step, the gates every change must pass, and how a pull request is released. The
[design document](design.md) describes what is underneath.

## 1. Tools

| Tool | Needed for | Version |
|---|---|---|
| OCaml and opam | Everything | OCaml 4.14 or later, 64-bit; CI tests 4.14 and 5.5 |
| dune | The build and the tests | 3.0 or later |
| A C compiler | The search kernels (`ocamlopt` needs one anyway) | Any recent GCC, Clang or MSVC |
| Git | The repository and the test data | Any recent version |
| PowerShell 7 | The check of the Unicode tables against the Rust crate's, and the release scripts | 7.4 or later |
| Node.js | The test data for other versions of Unicode and for ECMAScript 2020, and the V8 rows of the harness | 24 (the V8 it carries is the oracle) |

opam installs OCaml and dune on every platform: on Linux and macOS from [opam.ocaml.org](https://opam.ocaml.org) (or
`brew install opam`), then `opam init` and `opam install dune`; on Windows `winget install OCaml.opam`, then the same.
A second switch (`opam switch create 4.14.2`) runs the tests on the oldest OCaml.

## 2. Getting the sources

```
git clone https://github.com/corvus-dotnet/corvus-regex-ecma262-ocaml.git
git clone https://github.com/corvus-dotnet/ecma262-regex-test-data.git   # beside it: the test data
git clone https://github.com/corvus-dotnet/corvus-regex-ecma262-rs.git   # beside it: for the table check
cd corvus-regex-ecma262-ocaml
```

The test data is the oracle for the engine (design, section 6). It is not vendored; the commands below assume it at
`../ecma262-regex-test-data`.

## 3. Building and testing, step by step

Every command runs from the repository root.

1. **Build.** `dune build` builds in the dev profile, in which a warning is an error, and writes
   `corvus-regex-ecma262.opam` from `dune-project` and the template; commit it when it changes.
2. **The tests.** `dune test --force` runs the tests, the allocation test, the README's example and the
   getting-started page's examples; again with `CORVUS_REGEX_MEMO_FROM_THE_START=1`, which takes the memo of failed
   places into use from the first step. Run them on OCaml 4.14 and 5.
3. **The README's example** is the file the tests run: `test/readme/readme_example.ml` must be the README's block of
   OCaml.
4. **The stack probe.** `dune build --profile release ./tools/stackprobe/stackprobe.exe`, then
   `./tools/stackprobe/run.sh` runs it on stacks of 4 MB and 2 MB; no probe may overflow.
5. **The test data.**

   ```
   dune build --profile release ./tools/dataset/run.exe
   ./_build/default/tools/dataset/run.exe ../ecma262-regex-test-data/data wrong-cases.tsv
   ```

   It must report 0 wrong, 0 refused and 0 at a limit, and again with `CORVUS_REGEX_MEMO_FROM_THE_START=1`. For the
   other versions of Unicode and for ECMAScript 2020, write the data with its own scripts and run it with the engine
   set to match:

   ```
   node ../ecma262-regex-test-data/unicode.js apply 13.0 /tmp/u13 ../ecma262-regex-test-data/data
   ./_build/default/tools/dataset/run.exe /tmp/u13 wrong-u13.tsv --unicode 13
   node ../ecma262-regex-test-data/edition.js apply /tmp/es2020 "" ../ecma262-regex-test-data/data
   ./_build/default/tools/dataset/run.exe /tmp/es2020 wrong-es2020.tsv --edition es2020
   ```

## 4. The Unicode tables and the C kernels

The tables of each version of Unicode (`ucd_tables<major>.ml`, `strings_tables<major>.ml`) and the property names of
ECMAScript 2020 (`es2020.ml`) are written from the Rust crate's, so that every engine carries the same tables. After
the crate's tables change:

```
$src = '../corvus-regex-ecma262-rs/corvus-regex-ecma262/src'
foreach ($major in 13, 17, 18) {
  ./tools/unicode/gen-ucd-tables.ps1 -CrateTables "$src/tables$major.rs"
  ./tools/unicode/gen-strings.ps1 -CrateStrings "$src/strings$major.rs"
}
./tools/unicode/gen-es2020-properties.ps1 -CrateEs2020 "$src/es2020.rs"
```

With `-Check`, each script fails when what is checked in differs, as CI runs them.

The C kernels (`src/cre_stubs.c`) are compiled with the library. Each has a scalar fallback, and a change to one is
tested on x86-64 and on AArch64 (CI's arm64 and macOS legs), and on a big-endian processor (CI's s390x leg).

## 5. Benchmarks

`tools/benchmarks/run.sh <workloads.json> <texts directory> <out.json>` runs this library, `re`, `pcre2` and V8 over
the Industry workloads; the script lists what it needs and its settings. The published figures are on
[the benchmark page](https://corvus-oss.org/corvus-regex-ecma262-benchmarks/), which runs this repository's harness
image each night beside every other engine; a run on another machine is comparable with itself, never with the page.

## 6. What a pull request needs

- **The gates of section 3** passing on the change as pushed, on OCaml 4.14 and 5.
- **The case in the test data first** for a change to the engine's semantics: add the pattern to the data repository's
  family it belongs to, regenerate with its `generate.js`, check with `verify.js`, and pin the new data commit in
  `ci.yml`. V8 is the oracle; a fault V8 has is marked in the data, never copied into the engine.
- **A failing test first** for a bug: the test that reproduces it, then the fix.
- **A measured run** for a change made for speed, with the rows it moved.
- **The design document kept current**, describing the library as it is.
- **Doc comments** on every value of the `.mli`, since the API reference on ocaml.org is generated from them.
- **The getting-started page** kept true: its examples are run by the tests.

### Conventions

- OCaml that builds warning-free in dune's dev profile, on 4.14 and 5; C99 for the kernels.
- Nothing on the match path allocates once warm, and the allocation test holds that line.
- A change to the matcher is made in `vm.ml.in`, which is compiled for each encoding.

## 7. Releases

Every pull request merged into main releases the library with the next patch version, unless it is labelled
`no_release`: `auto-release.yml` runs `tools/release` on the merge, which tags `v<version>` on a commit with the version
written into `dune-project`, `corvus-regex-ecma262.opam` and `src/corvus_regex_ecma262.ml` (main keeps 0.0.0). The
release is then submitted to opam-repository, whose CI builds it on every compiler and platform it tests. A change to
the benchmark harness alone is a harness release, which publishes the harness image. A pull request that needs a minor
or major version sets `nextVersion` in `.github/release.json`. The documentation site takes the published documents (the
getting-started page, the API reference, the design document and this guide) from `main`, and they describe the latest
release: a change to them alone is merged with `no_release`, and a change to the API carries its documents, which its
release brings into step.

Changes arrive by pull request, and the `CI gate` check must be green on the branch as it will merge.
