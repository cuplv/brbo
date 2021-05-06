#!/bin/bash

set -x

today=$(date +%m%d-%H%M)
timeout=60
mostPrecise="mostPrecise"

mkdir -p output/

./scripts/run_with_deps_artifact.sh -d src/main/java/brbo/benchmarks/all -a all -t $timeout > output/"containers-$today-$timeout-$mostPrecise.txt" 2>&1