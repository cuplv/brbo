#!/bin/bash

set -x

rsync -rtv ~/win_c/Documents/workspace/academic/brbo-impl/ ~/Documents/workspace/brbo-impl/
# diff -qr ~/win_c/Documents/workspace/academic/brbo-impl/ ~/Documents/workspace/brbo-impl/
