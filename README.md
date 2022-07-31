# aa_models_bl

> Fleshed out description coming soon!!!

### Project goal
Assess the reliability of protein empirical models for inferring divergence. Due to existing empirical rates in matrices, we may expect them to be biased estimators.

### Contributors
+ Stephanie Spielman ([@sjspielman](https://github.com/sjspielman)): Lead investigator
+ Frankie Picone ([@piconef8](https://github.com/piconef8)): Undergraduate research assistant
	+ Summer 2019 - Spring 2020
+ Jake Mihalecz ([@jakemihalecz](https://github.com/jakemihalecz)): Undergraduate research assistant
	+ Fall 2020 - Summer 2021
+ Courtney Charlesworth ([@cjcharlesworth](https://github.com/cjcharlesworth)): Undergraduate research assistant
	+ Spring 2022
	+ [Courtney's Website](https://cjcharlesworth.github.io/)

### Project pipeline: Simulations

1. Obtain amino acid preference data from DMS experiments. Here, we use Jesse's nucleoprotein (NP) preferences, as in simulations performed for [Spielman and Wilke 2015](https://github.com/sjspielman/publications/blob/master/2015_SpielmanWilke_MBE.pdf).
2. Simulate as follows:
	+ Each individual simulation is a three-taxon tree with a total tree length of `X` where $X \in {0.01, 0.05, seq(0.1, 3, 0.1)}$
	+ Each individual simulation uses a single site parameterization from NP and 1e4 sites. Total of 498 sites.
	+ Use Gillespie algorithm to count # of changes as the simulation proceeds
3. Infer branch lengths as follows:
	+ Use IQ-TREE under Poisson, LG, WAG, JTT, FLU, with(out) +4G 
4. In total, we have:
	+ 32 branch length conditions
	+ 10 model conditions
	+ 498 NP models
	+ ===> 159360 results.
5. We then wish to compare inferred tree length with total number of *amino acid* substitutions that occurred. 
	+ Treelength * 1e4 = *expected* number of substitutions
	+ In count file    = *true* number of substitutions


### Project pipeline: Empirical

1. Obtain a bunch of protein alignments: birds, mammals, PANDIT, and enzymes
2. For a given tree topology for each empircal alignment, optimize BLs under Poisson, LG, WAG, JTT, FLU, with(out) +4G 
3. We then wish to assess whether there are differences in branch length distributions or summaries (e.g. tree length) across models
