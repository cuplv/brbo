#!/bin/bash

set -x

today=$(date +%m%d-%H%M)
timeout="60s"
lessPrecise="lessPrecise"
./scripts/run.sh -d src/main/java/brbo/benchmarks/containers -a all -t 60 --less-precise > output/"containers-$today-$timeout-$lessPrecise.txt" 2>&1