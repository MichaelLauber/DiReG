#!/bin/bash

input=$1
out=$2

awk '$NF33 {print $0}' ${input} > ${out}
