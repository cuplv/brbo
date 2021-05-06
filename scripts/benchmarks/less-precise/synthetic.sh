#!/bin/bash

set -x

today=$(date +%m%d-%H%M)
timeout=60
lessPrecise="lessPrecise"
./scripts/run_with_deps.sh -d src/main/java/brbo/benchmarks/synthetic -a all -t $timeout --less-precise > output/"synthetic-$today-$timeout-$lessPrecise.txt" 2>&1