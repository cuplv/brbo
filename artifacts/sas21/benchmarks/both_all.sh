#!/bin/bash

set -x

mkdir -p output/

./artifacts/sas21/benchmarks/all.sh ; ./artifacts/sas21/benchmarks/less-precise/all.sh
