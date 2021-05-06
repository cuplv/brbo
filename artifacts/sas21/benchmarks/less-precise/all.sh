#!/bin/bash

set -x

today=$(date +%m%d-%H%M)
timeout=60
lessPrecise="lessPrecise"

mkdir -p output/

./scripts/run_with_deps_artifact.sh -d src/main/java/brbo/benchmarks/ -a all -t $timeout --less-precise > output/"containers-$today-$timeout-$lessPrecise.txt" 2>&1