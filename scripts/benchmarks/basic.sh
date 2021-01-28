#!/bin/bash

set -x

./scripts/run.sh -d src/main/java/brbo/benchmarks/basic/ -a all -t 60 > output/basic.txt 2>&1