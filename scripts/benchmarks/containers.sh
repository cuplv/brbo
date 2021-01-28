#!/bin/bash

set -x

./scripts/run.sh -d src/main/java/brbo/benchmarks/containers/ -a all -t 60 > output/containers.txt 2>&1