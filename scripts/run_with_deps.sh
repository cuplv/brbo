#!/bin/bash

# This script should be executed from the root directory of this project via `./scripts/run.sh`

# Create output directory
mkdir -p output/
mkdir -p output/cfg

# Machine-dependent path configurations
tools_jar="/usr/lib/jvm/jdk1.8.0_271/lib/tools.jar"
brbo_jar="./target/scala-2.12/brbo-impl_2.12-0.1.jar" # Assume this jar file only contains the code of brbo

# Jars dependency
lib_jars="lib/deps/*"

# Set up paths for Z3
lib="$(pwd)/lib"
z3lib="$lib/z3"
export LD_LIBRARY_PATH=$z3lib:$LD_LIBRARY_PATH
export DYLD_LIBRARY_PATH=$z3lib:$DYLD_LIBRARY_PATH

classpath=".:$brbo_jar:$tools_jar:$lib_jars"

set -x
time java -Dlog4j.configurationFile="src/resources/log4j2.properties" -cp $classpath brbo.BrboMain "${@:1}"
# time java -jar target/scala-2.12/brbo-impl-assembly-0.1.jar $javafiles

# Clean up
