#!/bin/bash

startFile=$1
targetFile=$2
outFile=$3

#/usr/bin/bedtools intersect -a ${startFile} -b ${targetFile} -v  > ${outFile}
#scripts/bedtools intersect -a ${startFile} -b ${targetFile} -v  > ${outFile}
bedtools intersect -a ${startFile} -b ${targetFile} -v  > ${outFile}
