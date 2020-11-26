#!/bin/bash

set -x

rsync -rtv ~/Documents/workspace/brbo-impl/ ~/win_c/Documents/workspace/academic/brbo-impl/ 
# diff -qr ~/win_c/Documents/workspace/academic/brbo-impl/ ~/Documents/workspace/brbo-impl/
