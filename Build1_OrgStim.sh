#!/bin/bash



### Written by Nathan Muncy on 4/4/19
#
# A script to build the stimulus dir and set up for the R script.


workDir=$(pwd)/..
foodDir=${workDir}/FinalImages
objDir=${workDir}/FinalObjects
stimDir=${workDir}/Stimuli


 ## cp food stimuli
 cd $foodDir

 for i in {High,Low}{,Single}; do

 	HL=${i:0:1}
 	hold=$((${#i}-1))
 	last=`echo ${i:$hold:1}`

 	cd $i

 	for j in *jpg; do

 		if [ $last == e ]; then
 			end=${j/.j/s.j}
 		else
 			end=$j
 		fi

 		if [ ! -f ${stimDir}/F_${HL}_$end ]; then
 			cp $j ${stimDir}/F_${HL}_$end
 		fi
 	done
 	cd $foodDir
 done


 ## cp object stimuli
 cd $objDir

 for i in Pairs Singles; do

 	F=${i:0:1}
 	cd $i

 	for j in *.jpg; do
 		if [ $F == S ]; then
 			end=${j/.j/s.j}
 		else
 			end=$j
 		fi

 		if [ ! -f ${stimDir}/O_$end ]; then
 			cp $j ${stimDir}/O_$end
 		fi
 	done
 	cd $objDir
 done


## clean up T stimuli names
cd $stimDir

for i in F_?_T*; do

	tmp=$((${#i}-5))
	hold=`echo ${i:$tmp:1}`

	if [ $hold != s ]; then
		name=${i%_*}
		mv $i ${name}.jpg
	fi
done



# make lists - only pull "a" stim for pairs
cd $stimDir

for a in {FH,FL,O}_{pair,single}; do
	>${a}.txt
done

for i in *jpg; do

	categ=${i%%_*}

	tmp1=$((${#i}-5))
	stim=`echo ${i:$tmp1:1}`

	if [ $categ == O ]; then
		if [ $stim == a ]; then
			echo $i >> O_pair.txt
		elif [ $stim == s ]; then
			echo $i >> O_single.txt
		fi

	elif [ $categ == F ]; then

		tmp2=${i#*_}
		HL=${tmp2%%_*}

		if [ $HL == H ]; then
			if [ $stim == a ]; then
				echo $i >> FH_pair.txt
			elif [ $stim == s ]; then
				echo $i >> FH_single.txt
			fi
		elif [ $HL == L ]; then
			if [ $stim == a ]; then
				echo $i >> FL_pair.txt
			elif [ $stim == s ]; then
				echo $i >> FL_single.txt
			fi
		fi
	fi
done
