#!/bin/bash

#SBATCH --time=40:00:00   # walltime
#SBATCH --ntasks=10   # number of processor cores (i.e. tasks)
#SBATCH --nodes=1   # number of nodes
#SBATCH --mem-per-cpu=4gb   # memory per CPU core
#SBATCH -J "TS4"   # job name

# Compatibility variables for PBS. Delete if not needed.
export PBS_NODEFILE=`/fslapps/fslutils/generate_pbs_nodefile`
export PBS_JOBID=$SLURM_JOB_ID
export PBS_O_WORKDIR="$SLURM_SUBMIT_DIR"
export PBS_QUEUE=batch

# Set the max number of threads to use for programs using OpenMP. Should be <= ppn. Does nothing if the program doesn't use OpenMP.
export OMP_NUM_THREADS=$SLURM_CPUS_ON_NODE





# Written by Nathan Muncy on 12/9/19



### --- Set up --- ###										###??? update variables/arrays
#
# This is where the script will orient itself.
# Notes are supplied, and is the only section
# that really needs to be changed for each
# experiment.


# General variables
parDir=~/compute/FoodMST
workDir=${parDir}/derivatives								# par dir of data
outDir=${parDir}/Analyses/grpAnalysis						# where output will be written (should match step3)
refFile=${workDir}/sub-s300/run-1_Food_scale+tlrc			# reference file, for finding dimensions etc

tempDir=~/bin/Templates/vold2_mni							# desired template
priorDir=${tempDir}/priors_ACT								# location of atropos priors
mask=Intersection_GM_mask+tlrc								# this will be made, just specify name for the interesection gray matter mask


# grpAnalysis
thr=0.3														# thresh value for Group_EPI_mask, ref Group_EPI_mean
blurM=2														# blur multiplier, float/int

compList=(Response Encoding)								# matches decon prefixes, and will be prefix of output files

brickResponse=(3 5 7 13 15 17 23 25 27)
nameResponse=({H,L,O}_{Hit,LCR,LFA})

brickEncoding=(1 3 5 9 11 13 17 19 21)
nameEncoding=(Rp{H,L,O}_{Hit,LCR,LFA})




### --- Functions --- ###

# search array for string
MatchString () {
	local e match="$1"
	shift
	for e; do
		[[ "$e" == "$match" ]] && return 0
	done
	return 1
}




### --- Create Masks --- ###
#
# This section will create a group mean intersection mask
# then threshold it at $thr to create a binary intersection mask.
# A gray matter mask will be constructed, and then the GM mask
# will be multiplied with the intersection mask to create a
# single GM intersection mask

# make lists
cd $outDir

unset subjList list
arrRem=(`cat info_rmSubj_Encoding.txt`)		# only one session of experiment, so Encoding=Response

for i in ${workDir}/s*; do
	subj=${i##*\/}
	MatchString "$subj" "${arrRem[@]}"
	if [ $? == 1 ]; then
		list+="${i}/mask_epi_anat+tlrc "
		subjList+=("$i ")
	fi
done


# intersection mask
if [ ! -f Group_epi_mask.nii.gz ]; then
	3dMean -prefix ${outDir}/Group_epi_mean.nii.gz $list
	3dmask_tool -input $list -frac $thr -prefix ${outDir}/Group_epi_mask.nii.gz
fi


# make $mask
if [ ! -f ${mask}.HEAD ]; then

	# GM mask
	c3d ${priorDir}/Prior2.nii.gz ${priorDir}/Prior4.nii.gz -add -o tmp_Prior_GM.nii.gz
	3dresample -master $refFile -rmode NN -input tmp_Prior_GM.nii.gz -prefix tmp_Template_GM_mask.nii.gz

	# combine GM and intersection mask
	c3d tmp_Template_GM_mask.nii.gz Group_epi_mask.nii.gz -multiply -o tmp_Intersection_GM_prob_mask.nii.gz
	c3d tmp_Intersection_GM_prob_mask.nii.gz -thresh 0.1 1 1 0 -o tmp_Intersection_GM_mask.nii.gz
	3dcopy tmp_Intersection_GM_mask.nii.gz $mask
	rm tmp*
fi


# check
if [ ! -f ${mask}.HEAD ]; then
	echo >&2
	echo "Could not construct $mask. Exit 1" >&2
	echo >&2; exit 1
fi


# get template
if [ ! -f vold2_mni_brain+tlrc.HEAD ]; then
	cp ${tempDir}/vold2_mni_brain+tlrc* .
fi




### --- MVM --- ###
#
# Run noise simulations using the updated
# ACF method on blurred data. Construct
# MVM scripts, and run them.

# blur, determine parameter estimate
gridSize=`3dinfo -dk $refFile`
blurH=`echo $gridSize*$blurM | bc`
blurInt=`printf "%.0f" $blurH`


for i in ${compList[@]}; do

	# get estimates from e/subj
	if [ ! -s ACF_MC_${i}.txt ]; then

		print=ACF_raw_${i}.txt
		> $print

		for k in ${subjList[@]}; do
			for m in stats errts; do

				# blur
				hold=${k}/${i}_${m}_REML
				if [ ! -f ${hold}_blur${blurInt}+tlrc.HEAD ]; then
					3dmerge -prefix ${hold}_blur${blurInt} -1blur_fwhm $blurInt -doall ${hold}+tlrc
				fi
			done

			# parameter estimate
			file=${k}/${i}_errts_REML_blur${blurInt}+tlrc
			3dFWHMx -mask $mask -input $file -acf >> $print
		done

		# cacluate averages
		sed '/ 0  0  0    0/d' $print > tmp

		xA=`awk '{ total += $1 } END { print total/NR }' tmp`
		xB=`awk '{ total += $2 } END { print total/NR }' tmp`
		xC=`awk '{ total += $3 } END { print total/NR }' tmp`

		# simulate noise, determine thresholds
		3dClustSim -mask $mask -LOTS -iter 10000 -acf $xA $xB $xC > ACF_MC_${i}.txt
		rm tmp
	fi


	## build input for dataTable, set to dynamic variable
	brickArr=($(eval echo \${brick${i}[@]}))
	nameArr=($(eval echo \${name${i}[@]}))

	if [ ${#brickArr[@]} != ${#nameArr[@]} ]; then
		echo >&2
		echo "Brick and Name arrays are unequal for $i. Exit 2" >&2
		echo >&2; exit 2
	fi

	unset dataFrame
	for m in ${subjList[@]}; do
		c=0; while [ $c -lt ${#brickArr[@]} ]; do

			hold=${nameArr[$c]}
			type=${hold%_*}
			beh=${hold#*_}

			scan=${i}_stats_REML_blur${blurInt}+tlrc
			dataFrame+="${m##*\/} $type $beh ${m}/'${scan}[${brickArr[$c]}]' "
			let c+=1
		done
	done

	if [ -z $dataFrame ]; then
		echo >&2
		echo "Building 3dMVM dataTable failed for $i. Exit 3." >&2
		echo >&2; exit 3
	fi

	declare $(eval echo data$i)="$dataFrame"
done


## write scripts
# each is written individually so they can be fine tuned,
# particularly the post-hoc comparisons

# Response
inputResponse=$(eval echo \$dataResponse)

cat > MVM_Response.sh << EOF
module load r/3.6

3dMVM -prefix MVM_Response \
-jobs 10 \
-mask $mask \
-bsVars 1 \
-wsVars 'StimType*Beh' \
-dataTable \
Subj StimType Beh InputFile \
$inputResponse
EOF


# Encoding
inputEncoding=$(eval echo \$dataEncoding)

cat > MVM_Encoding.sh << EOF
module load r/3.6

3dMVM -prefix MVM_Encoding \
-jobs 10 \
-mask $mask \
-bsVars 1 \
-wsVars 'StimType*Beh' \
-dataTable \
Subj StimType Beh InputFile \
$inputEncoding
EOF


## Run, check scripts
for i in MVM*.sh; do
	file=${i%.*}+tlrc.HEAD
	if [ ! -f $file ]; then
		source $i
	fi
	if [ ! -f $file ]; then
		echo >&2
		echo "3dMVM failed on $i. Exit 4" >&2
		echo >&2; exit 4
	fi
done
