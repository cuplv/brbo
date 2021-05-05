#!/bin/bash

set -x

today=$(date +%m%d-%H%M)
timeout=60
lessPrecise="lessPrecise"
./scripts/run.sh -d src/main/java/brbo/benchmarks/string -a all -t $timeout --less-precise > output/"string-$today-$timeout-$lessPrecise.txt" 2>&1