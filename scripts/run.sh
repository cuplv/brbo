#!/bin/bash

# This script should be executed from the root directory of this project via `./scripts/run.sh`
# Before running this script, please make sure that using javac to compile the target project will not have any compiler errors

# Shell arguments
src_dir="$1" # relative path
lib_dir="$2"

# Create output directory
mkdir -p output/
mkdir -p output/cfg
mkdir -p output/c_files
mkdir -p output/class_files

# Machine-dependent path configurations
tools_jar="/usr/lib/jvm/jdk1.8.0_271/lib/tools.jar"
brbo_jar="$HOME/win_c/Desktop/brbo-fat.jar"

# Set up paths for Z3
lib="$(pwd)/lib"
z3lib="$lib/z3"
export LD_LIBRARY_PATH=$z3lib:$LD_LIBRARY_PATH
export DYLD_LIBRARY_PATH=$z3lib:$DYLD_LIBRARY_PATH

# Find all jar files in directory $lib_dir
target_project_lib=`find "$lib_dir" -name "*.jar" | tr '\n' ':'`
classpath=".:$brbo_jar:$tools_jar:$target_project_lib"

# Find all source files
javafiles="java_src_files.txt"
find "$src_dir" -name "*.java" > $javafiles

echo "Step 1: Numerically abstract"

printf  "\n\n\n"

echo "Step 2: Infer bounds"
time java -cp $classpath brbo.BrboMain $javafiles

# Clean up
rm $javafiles
