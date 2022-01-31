## Contents:


### Other 

+ `np_site_dnds_entropy.csv`: The evolutionary properties of each NP site

### Simulation result files

+ `inferred_branch_lengths.csv`: The inferred treelengths with each model for each simulation
    + Made with `../scripts/parse_treelength_simcounts.R`
+ `merged_branch_lengths_counts.csv`: 
    + Made with `../scripts/parse_treelength_simcounts.R`
+ `simulation_aa_counts.csv`: The number of amino-acid substitutions per simulation
    + Made with `../scripts/parse_treelength_simcounts.R`
+ `simulation_ic_scores.csv`: The AIC, AICc, and BIC for each model for each simulation
    + Made with `../scripts/parse_ic.py`
+ `simulation_bias_slope.csv`: Contains results of LM's to assess slope and bias of the results
    + Made with `../scripts/calculate_simulation_bias.R`
