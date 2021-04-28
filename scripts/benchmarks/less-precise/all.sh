#!/bin/bash

set -x

./scripts/benchmarks/less-precise/string.sh
./scripts/benchmarks/less-precise/containers.sh
./scripts/benchmarks/less-precise/synthetic.sh
# ./scripts/benchmarks/less-precise/basic.sh