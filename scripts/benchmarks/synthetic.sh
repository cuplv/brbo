#!/bin/bash

set -x

./scripts/run.sh -d src/main/java/brbo/benchmarks/synthetic/ -a all -t 60 > output/synthetic.txt 2>&1