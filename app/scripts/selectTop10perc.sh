#!/bin/bash

input=$1
out=$2
length=$(wc -l ${input} | cut -f 1 -d' ')
top10=$(( ${length} /10 ))

sort -k 9nr ${input} | head -n ${top10} > ${out}
