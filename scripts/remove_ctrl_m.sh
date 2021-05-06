#!/bin/bash

# Run this script each time before running `sbt test` such that it won't complain the difference between CRLF and LF
# Source code files end with CRLF because we edit them from Windows
# However, the strings generated from running Brbo end with LF

find ./src/test/ -type f -print0 | xargs -0 dos2unix
find ./scripts -type f -print0 | xargs -0 dos2unix