PATH_TO_COUNTS=$1
PATH_TO_BL=$2

# IC ranks for each branch length
python3 parse_ic.py ${PATH_TO_BL}
Rscript --vanilla rank_ic.R

# Simulation counts vs branch lengths
Rscript --vanilla parse_simulations_count_bl.R ${PATH_TO_COUNTS} ${PATH_TO_BL}