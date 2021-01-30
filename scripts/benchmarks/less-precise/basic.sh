#!/bin/bash

set -x

today=$(date +%m%d-%H%M) # today=$(date +%Y%m%d-%H%M)
./scripts/run.sh -d src/main/java/brbo/benchmarks/basic -a all -t 60 --less-precise > output/"basic-$today.txt" 2>&1