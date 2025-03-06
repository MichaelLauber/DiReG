#!/bin/bash

input=$1
out=$2

/usr/bin/sort -k1,1 -k2,2n  ${input} > ${out}
