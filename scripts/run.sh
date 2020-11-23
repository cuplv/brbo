#!/bin/sh

# Pre-condition before running this script
# - Using javac to compile the target project will not have compiler errors
# - This script is executed from the root directory of this project via `./scripts/run.sh`

PWD=$(pwd)

# ==========================Please configure the following paths============================
scala_lib="$HOME/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.12.12.jar"
tool_jar="$HOME/Desktop/brbo.jar"
# ==========================================================================================

# Shell arguments
src_dir="$1" # relative path
lib_dir="$2"

# Set up the environment for external tools
lib="$PWD/lib"

# Set up paths for Checker Framework
checker_framework_bin="$lib/checker-framework-3.7.1/checker/bin"
export PATH=$checker_framework_bin:$PATH # To override the default `javac` with file `javac` in the above directory

# Set up paths for Z3
z3lib="$lib/z3"
z3jar="$lib/z3/com.microsoft.z3.jar"
export LD_LIBRARY_PATH=z3lib:$LD_LIBRARY_PATH
export DYLD_LIBRARY_PATH=z3lib:$DYLD_LIBRARY_PATH

# set -x
# Find all jar files in directory $lib_dir
target_project_lib=`find "$lib_dir" -name "*.jar" | tr '\n' ':'`
classpath=".:$z3jar:$scala_lib:$tool_jar:$target_project_lib"
# echo $classpath
javafiles="java_src_files.txt"
find "$src_dir" -name "*.java" > $javafiles
time javac -proc:only -Xmaxwarns 10000 -Xmaxerrs 10000 -cp $classpath -processor bndinfchecker.BndinfChecker @$javafiles -d output/ $3
# time javac -proc:only -Xmaxwarns 10000 -Xmaxerrs 10000 -cp $classpath -processor numabschecker.NumabsChecker @$javafiles -d output/ $3
rm $javafiles
