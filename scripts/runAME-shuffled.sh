#!/bin/bash

input=$1
motifFile=$2
outDir=$3


#/nfs/home/users/michaell/meme/bin/ame --control --shuffle-- -oc ${outDir}  ${input} ${motifFile}
scripts/ame --control --shuffle-- -oc ${outDir}  ${input} ${motifFile}
