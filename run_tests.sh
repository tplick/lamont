#!/bin/bash
set -e

mkdir $1

time ./exe.opt -single 315 | tee $1/single-315.txt
time ./exe.opt -show-runs -bench 100 | tee $1/bench-100.txt
time ./exe.opt -show-runs -bench 1000 | tee $1/bench-1000.txt
time ./exe.opt -show-runs -bench 10000 | tee $1/bench-10000.txt

