#!/bin/bash 

THREADS=6

# Making some assumptions y'all BUCKLE UP
TOP_DATA_DIR=/Users/spielman/googledrive/research_data/data_aa_models_bl/empirical_alignments
NAME=$1
DATADIR=${TOP_DATA_DIR}/${NAME}_alignments_trees 
BLDIR=${TOP_DATA_DIR}/${NAME}_optimized_bl
OUTFILE=${TOP_DATA_DIR}/${NAME}_optimized_branch_lengths.csv
 
# Infer branch lengths across models
python3 infer_empirical_branch_lengths.py $DATADIR $BLDIR $NAME $THREADS

# Get branch lengths into CSV
Rscript --vanilla parse_empirical_bl.R $NAME $BLDIR $OUTFILE