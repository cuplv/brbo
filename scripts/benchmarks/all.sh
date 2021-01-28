#!/bin/bash

set -x

./scripts/benchmarks/string.sh
./scripts/benchmarks/containers.sh
./scripts/benchmarks/synthetic.sh
./scripts/benchmarks/basic.sh