#!/bin/bash


parDir=/Volumes/Vedder/FoodMST
refDir=${parDir}/Participants
outDir=${parDir}/Analyses/behAnalysis/all_data
timDir=${parDir}/Analyses/behAnalysis/timing_files


mkdir -p $outDir $timDir


cd $refDir

for i in s*; do
	if [ -f ${i}/${i}_FoodMST-${i/s}-1.txt ]; then
		cp ${i}/${i}_FoodMST-${i/s}-1.txt $outDir
	fi
done
