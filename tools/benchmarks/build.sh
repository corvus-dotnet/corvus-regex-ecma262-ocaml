#!/usr/bin/env bash
# Readies the harness: the harness program (tools/benchmarks/industry.ml) and the test data runner (tools/dataset/run.ml)
# built in the harness profile (release settings), and the versions of re and pcre2 written to
# tools/benchmarks/versions.env for the run's facts (the image has no opam to ask). run.sh calls it, and the image (Dockerfile) calls it when it is built.
set -euo pipefail
cd "$(dirname "$0")/../.."
opam exec -- dune build --profile harness ./tools/benchmarks/industry.exe ./tools/dataset/run.exe
{
  echo "RE_VERSION=$(opam show -f version re)"
  echo "PCRE2_OCAML_VERSION=$(opam show -f version pcre2)"
} > tools/benchmarks/versions.env
