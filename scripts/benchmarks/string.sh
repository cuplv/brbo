#!/bin/bash

set -x

today=$(date +%m%d-%H%M)
timeout=60
mostPrecise="mostPrecise"
./scripts/run_with_deps.sh -d src/main/java/brbo/benchmarks/string -a all -t $timeout > output/"string-$today-$timeout-$mostPrecise.txt" 2>&1