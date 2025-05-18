#!/bin/bash


input=$1
motifFile=$2
homer_background=$3
outDir=$4


#/nfs/home/users/michaell/meme/bin/ame --control ${homer_background} -oc ${outDir}  ${input} ${motifFile}
#scripts/ame --control ${homer_background} -oc ${outDir}  ${input} ${motifFile}
ame --control ${homer_background} -oc ${outDir}  ${input} ${motifFile}
