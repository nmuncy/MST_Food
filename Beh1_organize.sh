#!/bin/bash


parDir=/Volumes/Vedder/FoodMST
refDir=${parDir}/Participants
outDir=${parDir}/Analyses/behAnalysis/all_data
timDir=${parDir}/Analyses/behAnalysis/timing_files


mkdir -p $outDir $timDir


cd $refDir

for i in p*; do
	if [ -f ${i}/FoodMST*.txt ]; then
		cp ${i}/FoodMST*.txt $outDir
	fi
done
