#!/bin/bash

set -x

today=$(date +%m%d-%H%M)
timeout=60
lessPrecise="lessPrecise"

./scripts/run_with_deps_artifact.sh -d src/main/java/brbo/benchmarks/containers -a all -t $timeout --less-precise > output/"containers-$today-$timeout-$lessPrecise.txt" 2>&1

./scripts/run_with_deps_artifact.sh -d src/main/java/brbo/benchmarks/string -a all -t $timeout --less-precise > output/"string-$today-$timeout-$lessPrecise.txt" 2>&1

./scripts/run_with_deps_artifact.sh -d src/main/java/brbo/benchmarks/synthetic -a all -t $timeout --less-precise > output/"synthetic-$today-$timeout-$lessPrecise.txt" 2>&1