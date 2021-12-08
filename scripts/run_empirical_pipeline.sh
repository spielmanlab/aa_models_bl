#!/bin/bash 

THREADS=6

# y'all BUCKLE UP for some HARDCODED PATHS!!! 
TOP_DATA_DIR=/Users/spielman/Desktop/empirical_alignments
NAME=$1
DATADIR=${TOP_DATA_DIR}/alignments_trees/${NAME}/
BLDIR=${TOP_DATA_DIR}/optimized_bl/${NAME}/
OUTFILE=../results/${NAME}_optimized_branch_lengths.csv
 
mkdir -p ${BLDIR}

# Infer branch lengths across models
python3 infer_empirical_branch_lengths.py $DATADIR $BLDIR $NAME $THREADS

# Get branch lengths into CSV
Rscript --vanilla parse_empirical_bl.R $NAME $BLDIR $OUTFILE