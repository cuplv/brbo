#!/bin/bash

set -x

today=$(date +%m%d-%H%M)
./scripts/run.sh -d src/main/java/brbo/benchmarks/synthetic -a all -t 60 > output/"synthetic-$today.txt" 2>&1