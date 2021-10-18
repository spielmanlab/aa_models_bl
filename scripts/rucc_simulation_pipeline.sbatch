#!/bin/bash
#SBATCH --job-name=simulate_mutsel_infer_bl
#SBATCH -N 1
#SBATCH --ntasks-per-node=5
#SBATCH --mem-per-cpu=1000
#SBATCH -p csm
#SBATCH --array=2-100

##### The variable ${SLURM_ARRAY_TASK_ID} automatically will be defined 1-5 (array line). Eventually this will be *bigger*
THREADS=5  ## see above: --ntasks-per-node

TOPPATH=/csm_data/spielman_lab/aa_model_bl
SIMPATH_SEQ=${TOPPATH}/simulations_sequences
SIMPATH_COUNT=${TOPPATH}/simulations_counts
BLPATH=${TOPPATH}/branch_length_inference

mkdir -p $SIMPATH_SEQ
mkdir -p $SIMPATH_COUNT
mkdir -p $BLPATH


cd $TOPPATH

######################## Simulate #################################
## Define input information
SITE=${SLURM_ARRAY_TASK_ID}
PREFFILE=${TOPPATH}/preferences/NP_prefs.csv
SIZE=10000


for BL in 0.01 0.05 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0 1.1 1.2 1.3 1.4 1.5 1.6 1.7 1.8 1.9 2.0 2.1 2.2 2.3 2.4 2.5 2.6 2.7 2.8 2.9 3.0; do

    
    ## Define the output files that will come out of simulation: an AA alignment in FASTA (we don't need to save codon version) and a CSV of counts
    SIM_SEQ_OUTPUT=${SIMPATH_SEQ}/site${SITE}_bl${BL}.fasta 
    SIM_COUNT_OUTPUT=${SIMPATH_COUNT}/site${SITE}_bl${BL}.csv

    ## Arguments should be all input files or information as well as all output files - increases portability of the script
    ## The python script itself should have NO ACTUAL PATHS OR DIRECT FILE NAMES!!!!
    if [ ! -f ${SIM_SEQ_OUTPUT} ]; then ## only simulate if there is no simulation already
        python3 simulate.py $SITE $SIZE $PREFFILE ${BL} ${SIM_SEQ_OUTPUT} ${SIM_COUNT_OUTPUT}   ## note about first arg: will need to say site - 1 in python for indexing purposes
    fi

    ########################### Branch lengths #######################
    ### 5 * 2 * 2 = 20 models for each simulation ###
    for MODEL in Poisson+F WAG+F JTT+F LG+F FLU+F; do
        for GAMMA in "" +G; do
            
            FULLMODEL=${MODEL}${INV}${GAMMA}
            TREEFILE=${BLPATH}/site${SITE}_bl${BL}_${FULLMODEL}.treefile
            INFOFILE=${BLPATH}/site${SITE}_bl${BL}_${FULLMODEL}.iqtree
            LOGLFILE=${BLPATH}/site${SITE}_bl${BL}_${MODELNAME}.sitelogl
        
            if [ ! -f ${TREEFILE} ]; then ## only run if not already run
                iqtree -st AA -s ${SIM_SEQ_OUTPUT} -m ${FULLMODEL} -nt ${THREADS}
        
                # keep files we need renamed
                mv ${SIM_SEQ_OUTPUT}.treefile $TREEFILE
                mv ${SIM_SEQ_OUTPUT}.iqtree $INFOFILE
                mv ${SIM_SEQ_OUTPUT}.sitelogl $LOGLFILE
          
                # remove files we don't need
                rm ${SIM_SEQ_OUTPUT}.*
            fi
        done
    done
done

