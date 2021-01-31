#!/bin/bash

set -x

today=$(date +%m%d-%H%M) # today=$(date +%Y%m%d-%H%M)
timeout=60
mostPrecise="mostPrecise"
./scripts/run.sh -d src/main/java/brbo/benchmarks/basic -a all -t $timeout > output/"basic-$today-$timeout-$mostPrecise.txt" 2>&1