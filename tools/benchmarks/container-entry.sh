#!/usr/bin/env bash
# The image's entry point: tools/benchmarks/run.sh over the mounted workloads, texts and (when mounted) test data.
set -euo pipefail
# The versions of re and pcre2 that build.sh recorded, for the run's facts.
set -a
. /harness/tools/benchmarks/versions.env
set +a
data=()
if [ -d /data ]; then data=(/data); fi
mkdir -p /out
exec /harness/tools/benchmarks/run.sh /workloads.json /texts /out/industry.json "${data[@]}"
