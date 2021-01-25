#!/bin/bash
#SBATCH -J sim_inf
#SBATCH -N 1
#SBATCH -n 7             #use site recommended # of cores
#SBATCH -p normal
#SBATCH -o o.%j
#SBATCH -e e.%j
#SBATCH -t 24:00:00
##SBATCH -A spielman   #uncomment and insert acct name if necessary

module load intel/18.0.2  
module load python3/3.7.0

SITE=$1
THREADS=7
SIZE=1000000

TOPPATH=$WORK/aa_model_bl
SIMPATH_SEQ=${TOPPATH}/simulations_sequences
SIMPATH_COUNT=${TOPPATH}/simulations_counts
BLPATH=${TOPPATH}/branch_length_inference

mkdir -p $SIMPATH_SEQ
mkdir -p $SIMPATH_COUNT
mkdir -p $BLPATH

cd $TOPPATH

######################## Simulate #################################
## Define input information
SIM_TREEFILE=${TOPPATH}/unrooted_simulation_tree.tre
PREFFILE=${TOPPATH}/preferences/NP_prefs.csv

## Define the output files that will come out of simulation: an AA alignment in FASTA (we don't need to save codon version) and a CSV of counts
SIM_SEQ_OUTPUT=${SIMPATH_SEQ}/site${SITE}.fasta 
SIM_COUNT_OUTPUT=${SIMPATH_COUNT}/site${SITE}.csv

## Arguments should be all input files or information as well as all output files - increases portability of the script
## The python script itself should have NO ACTUAL PATHS OR DIRECT FILE NAMES!!!!
if [ ! -f ${SIM_SEQ_OUTPUT} ]; then ## only simulate if there is no simulation already
    python3 simulate.py $SITE $SIZE $PREFFILE ${SIM_TREEFILE} ${SIM_SEQ_OUTPUT} ${SIM_COUNT_OUTPUT}   ## note about first arg: will need to say site - 1 in python for indexing purposes
fi

########################### Branch lengths #######################
for MODEL in Poisson+F WAG+F JTT+F LG+F FLU+F; do
    for GAMMA in "" +G; do
               
        FULLMODEL=${MODEL}${INV}${GAMMA}
        TREEFILE=${BLPATH}/site${SITE}_${FULLMODEL}.treefile
        INFOFILE=${BLPATH}/site${SITE}_${FULLMODEL}.iqtree
        
        if [ ! -f ${TREEFILE} ]; then ## only run if not already run
          #  $HOME/bin/iqtree -st AA -s ${SIM_SEQ_OUTPUT} -m ${FULLMODEL} -te ${SIM_TREEFILE} -nt AUTO -ntmax ${THREADS}
         $HOME/bin/iqtree -st AA -s ${SIM_SEQ_OUTPUT} -m ${FULLMODEL} -te ${SIM_TREEFILE} -nt 7

            # keep files we need, renamed
            mv ${SIM_SEQ_OUTPUT}.treefile $TREEFILE
            mv ${SIM_SEQ_OUTPUT}.iqtree $INFOFILE
          
            # remove files we don't need
            rm ${SIM_SEQ_OUTPUT}.ckp.gz
            rm ${SIM_SEQ_OUTPUT}.log
        fi
    done
done

