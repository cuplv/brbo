#!/bin/bash

set -x

today=$(date +%m%d-%H%M)
timeout="60s"
mostPrecise="mostPrecise"
./scripts/run.sh -d src/main/java/brbo/benchmarks/synthetic -a all -t 60 > output/"synthetic-$today-$timeout-$mostPrecise.txt" 2>&1