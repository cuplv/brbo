#!/bin/bash

# This script should be executed from the root directory of this project via `./scripts/run.sh`
# Before running this script, please make sure that using javac to compile the target project will not have any compiler errors

# Shell arguments
src_dir="$1" # relative path
lib_dir="$2"

# Machine-dependent path configurations
scala_lib="$HOME/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.12.12.jar"
tool_jar="$HOME/win_c/Desktop/brbo.jar"
log4j_api_jar="$HOME/.ivy2/cache/org.apache.logging.log4j/log4j-api/jars/log4j-api-2.11.2.jar"
log4j_core_jar="$HOME/.ivy2/cache/org.apache.logging.log4j/log4j-core/jars/log4j-core-2.11.2.jar"

# Set up the environment for external tools
lib="$(pwd)/lib"

# Set up paths for Checker Framework
checker_framework_bin="$lib/checker-framework-3.7.1/checker/bin"
export PATH=$checker_framework_bin:$PATH # To override the default `javac` with file `javac` in the above directory

# Set up paths for Z3
z3lib="$lib/z3"
z3jar="$lib/z3/com.microsoft.z3.jar"
export LD_LIBRARY_PATH=$z3lib:$LD_LIBRARY_PATH
export DYLD_LIBRARY_PATH=$z3lib:$DYLD_LIBRARY_PATH

# Find all jar files in directory $lib_dir
target_project_lib=`find "$lib_dir" -name "*.jar" | tr '\n' ':'`
classpath=".:$z3jar:$scala_lib:$tool_jar:$log4j_api_jar:$log4j_core_jar:$target_project_lib"

# Find all source files
javafiles="java_src_files.txt"
find "$src_dir" -name "*.java" > $javafiles

echo "Run the bound inference checker"
time javac -proc:only -Xmaxwarns 10000 -Xmaxerrs 10000 -cp $classpath -processor bndinfchecker.BndinfChecker @$javafiles -d . $3

printf  "\n\n\n"

echo "Run the numeric abstraction checker"
time javac -proc:only -Xmaxwarns 10000 -Xmaxerrs 10000 -cp $classpath -processor numabschecker.NumabsChecker @$javafiles -d . $3

# Clean up
rm $javafiles
