#!/bin/bash

set -x

./scripts/run.sh -d src/main/java/brbo/benchmarks/string/ -a all -t 60 > output/string.txt 2>&1