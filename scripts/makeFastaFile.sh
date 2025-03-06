#!/bin/bash

inputFile=$1
motifFile=$2
outFile=$3

#/usr/bin/bedtools getfasta -fi ${motifFile} -bed ${inputFile} > ${outFile}
scripts/bedtools getfasta -fi ${motifFile} -bed ${inputFile} > ${outFile}


