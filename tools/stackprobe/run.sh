#!/usr/bin/env bash
# Runs the stack probe under two stacks: 4 MB and 2 MB, set by ulimit -s (the system stack, which OCaml 4.14 runs on)
# and by the stack limit of OCAMLRUNPARAM in words (OCaml 5, whose stacks grow on the heap up to it). Run from the root
# of the repository after dune build --profile release ./tools/stackprobe/stackprobe.exe.
set -u
exe=./_build/default/tools/stackprobe/stackprobe.exe
status=0
for kb in 4096 2048; do
  echo "== stack of $((kb / 1024)) MB"
  (ulimit -s "$kb" && OCAMLRUNPARAM="l=$((kb * 1024 / 8))" "$exe" "$@") || status=1
done
exit $status
