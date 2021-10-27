Scripts to establish result files 
----------------------------------------

> Scripts in `..analysis/` analyze given result files.

## Simulation study

+ Miscellaneous
    + `calc_site_dnds_entropy.py` calculates dNdS (Spielman and Wilke 2015) and AA entropy for each site simulation parameters to `../results/np_site_dnds_entropy.csv`
    + `compute_dnds_from_mutsel.py` is imported by `calc_site_dnds_entropy.py`

+ `rucc_simulation_pipeline.sbatch` performed simulation/bl inference on RUCC and calls in order:
    + `simulate.py` to simulate alignments in pyvolve using Gillespie. Saves a substitution file and fasta file for each.
    + Calls IQTREE directly to optimize all the branch lengths. Saves the .sitelogl, .treefile, and .iqtree files
    + In the end, data is stored in drive (massive amount of data)

+ `parse_simulation_results.sh` parses the results of the simulation/inference pipeline and calls...
    + `parse_ic.py` obtains AIC, BIC, AICc for each inference and saves in `../results/simulation_ic_scores.csv`
    + `rank_ic.R` then processes data from `../results/simulation_ic_scores.csv` to obtain ranks per bl, saved in `../results/simulation_ic_ranks_per_bl.csv`
    + `parse_simulations_count_bl.R` processes the massive drive data to make `simulation_branch_lengths_counts.csv` which contains BOTH the aa simulation counts and the inferred branch lengths 


## Empirical study

+ `run_empirical_pipeline.sh` infers all branch lengths for given $NAME (using some hard-coded data paths!!) and calls in order:
    + `infer_empirical_branch_lengths.py` to call IQTREE and optimize bl's per model
    + `parse_empirical_bl.R` to parse the output and create the massive data files that are in Drive: