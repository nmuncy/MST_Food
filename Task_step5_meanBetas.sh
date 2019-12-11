#!/bin/bash

#SBATCH --time=10:00:00   # walltime
#SBATCH --ntasks=6   # number of processor cores (i.e. tasks)
#SBATCH --nodes=1   # number of nodes
#SBATCH --mem-per-cpu=6gb   # memory per CPU core
#SBATCH -J "TS5"   # job name

# Compatibility variables for PBS. Delete if not needed.
export PBS_NODEFILE=`/fslapps/fslutils/generate_pbs_nodefile`
export PBS_JOBID=$SLURM_JOB_ID
export PBS_O_WORKDIR="$SLURM_SUBMIT_DIR"
export PBS_QUEUE=batch

# Set the max number of threads to use for programs using OpenMP. Should be <= ppn. Does nothing if the program doesn't use OpenMP.
export OMP_NUM_THREADS=$SLURM_CPUS_ON_NODE






# Written by Nathan Muncy on 11/28/18


###--- Notes, in no particular order
#
# 1) the script will split the cluster file into multiple masks, and pull betas from each participant.
#
# 2) assumes clusters from step4 output have been saved in Clust_$fileArr format
#		will use the comparisonString portion to keep different group analyses straight
#		comparisonString should match the decon prefix (step4 $etacList)
#
# 3) assumes that decon files exist locally (bring them back from the supercomputer)
#
# 4) Written for the output of ETAC - will pull betas for each appropriate blur from eaach cluster



# Variables
parDir=~/compute/FoodMST
workDir=${parDir}/derivatives										###??? Update this section
grpDir=${parDir}/Analyses/grpAnalysis
clustDir=${grpDir}/mvm_clusters
outDir=${grpDir}/mvm_betas


compList=(Response Encoding)								# matches decon prefixes, and will be prefix of output files

brickResponse=(3 5 7 13 15 17 23 25 27)
brickEncoding=(1 3 5 9 11 13 17 19 21)

compStat=(MEstim MEbeh Intx)
compThresh=(7.557 7.557 4.868)

clustName_EncodingMEstim=(LDMPFC LOCC ROCC LIPS LOFC ROCC2 RIPS LIPS2 RIFS LPHC LINS RINS LOCC2 RIPS2 LAG RPHC ROCC3 RDMPFC ROCC4 RCun LOCC3 RLOCC RIPS3 RINS2 RIFS2 LIFS RINS3 RPHC2 LIFS2 LVS RDMPFC2 LIPS3 RVS LVS2 LDMPFC2)
clustName_EncodingMEbeh=(RAG RMFG RTPJ RACC)
clustName_EncodingIntx=(RDMPFC)

clustName_ResponseMEstim=(RLOCC RPHC LPHC LAG LLOCC RIFS LOFC LIPS ROCC RVS LRS ROCC2 LOCC ROCC3 LIPS2 RIPS LOCC2 ROCC4 RIPS2 RRS LOCC3 ROCC5 LSTS ROCC6)
clustName_ResponseMEbeh=(LIFG LDMPFC LVS LIPS RVS RIFS LPCC LOper RAG LOFC RDMPFC RINS ROper LSFS LCN RTPJ LCN2 LSFG RIPS RIPar RVS2 LPCG LPCC2 LRS RSFG)
clustName_ResponseIntx=(LDMPFC RVS LSTS LOper RSFS)




# function - search array for string
MatchString (){
	local e match="$1"
	shift
	for e; do [[ "$e" == "$match" ]] && return 0; done
	return 1
}




### make clusters, tables
mkdir $outDir $clustDir
cd $grpDir


for i in ${compList[@]}; do

	# get MC thresh, always round up to int
	mc=`grep "0.0010" ACF_MC_${i}.txt | sed '4q;d' | awk '{print $7}' | awk '{print ($0-int($0)>0)?int($0)+1:int($0)}'`

	# save clusters and table from each desired comparison
	for j in ${!compThresh[@]}; do

		pos=$(($j+1))
		val=${compThresh[$j]}
		str=${compStat[$j]}

		if [ ! -f ${clustDir}/Clust_${i}_${str}_mask+tlrc.HEAD ]; then

			3dclust -1Dformat -nosum -1dindex $pos \
			-1tindex $pos -2thresh -$val $val -dxyz=1 \
			-savemask Clust_${i}_${str}_mask \
			1.01 $mc MVM_${i}+tlrc.HEAD > Clust_${i}_${str}_table.txt

			mv Clust* $clustDir
		fi

		# check for output
		if [ ! -f ${clustDir}/Clust_${i}_${str}_mask+tlrc.HEAD ]; then
			echo >&2
			echo "Extracting clusters failed for $i $str. Exit 1" >&2
			echo >&2; exit 1
		fi
	done
done




### pull mean betas for e/cluster from e/comparison from e/subject
cd $clustDir

for i in ${compList[@]}; do

	# determine bricks, exclusion list
	brickList=$(eval echo \${brick${i}[@]})
	bricks=${brickList// /,}
	arrRem=(`cat ${grpDir}/info_rmSubj_${i}.txt`)

	for j in ${compStat[@]}; do

		# determine number of clusters
		num=`3dinfo Clust_${i}_${j}_mask+tlrc | grep "At sub-brick #0 '#0' datum type is short" | sed 's/[^0-9]*//g' | sed 's/^...//'`
		nameArr=($(eval echo \${clustName_${i}${j}[@]}))

		# start output
		print=${outDir}/Betas_${i}_${j}.txt
		> $print

		# check that planned names == number of clusters
		if [ $num != ${#nameArr[@]} ]; then
			echo >&2
			echo "Number of clusters in Clust_${i}_${j}_mask+tlrc does not equal length of clustName_${i}${j}. Exit 2" >&2
			echo >&2; exit 2
		fi

		for k in ${!nameArr[@]}; do

			clust=${nameArr[$k]}
			echo "Clust_$clust" >> $print
			mask=Clust_${i}_${j}_clust-${clust}+tlrc

			# split parent cluster file into indivdiual cluster files, rename
			if [ ! -f ${mask}.HEAD ]; then
				c=$(($k+1))
				3dcopy Clust_${i}_${j}_mask+tlrc.HEAD tmp_${i}_${j}.nii.gz
				c3d tmp_${i}_${j}.nii.gz -thresh $c $c 1 0 -o tmp_${i}_${j}_${clust}.nii.gz
				3dcopy tmp_${i}_${j}_${clust}.nii.gz $mask
				rm tmp_*
			fi

			# pull betas from apporpriate subjs
			for m in ${workDir}/sub-*; do
				subj=${m##*\/}
				MatchString "$subj" "${arrRem[@]}"
				if [ $? == 1 ]; then
					file=${m}/${i}_stats_REML+tlrc
					stats=`3dROIstats -mask $mask "${file}[${bricks}]"`
					echo "$subj $stats" >> $print
				fi
			done
			echo "" >> $print
		done
	done
done
