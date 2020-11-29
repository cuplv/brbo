#!/bin/bash

find ./src/test/ -type f -print0 | xargs -0 dos2unix