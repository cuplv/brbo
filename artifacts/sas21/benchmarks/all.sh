#!/bin/bash

set -x

today=$(date +%m%d-%H%M)
timeout=60
mostPrecise="mostPrecise"

mkdir -p output/

./scripts/run_with_deps_artifact.sh -d src/main/java/brbo/benchmarks/containers -a all -t $timeout > output/"containers-$today-$timeout-$mostPrecise.txt" 2>&1

./scripts/run_with_deps_artifact.sh -d src/main/java/brbo/benchmarks/string -a all -t $timeout > output/"string-$today-$timeout-$mostPrecise.txt" 2>&1

./scripts/run_with_deps_artifact.sh -d src/main/java/brbo/benchmarks/synthetic -a all -t $timeout > output/"synthetic-$today-$timeout-$mostPrecise.txt" 2>&1
