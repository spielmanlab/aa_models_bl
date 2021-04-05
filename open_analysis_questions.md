+ Is there a difference in evolutionary properties (entropy or dnds) between sites without much and with a lot of bias?
+ How does the bias relate to model preference? Are the bias rankings consistent with model rankings?
+ Make linear models of specific branch lengths. I guess this is a lot of one-sample t-tests, one t-test for each branch length and then divide all p-values by the number of branch lengths in the sims (something like 30?
+ Can we figure out at which branch lengths the relationship starts to get borked? Can do like a systematic exclusion of higher bl to find when the bias is p>0.05?

+ Empirical data analysis:
	+ **Do branch length summary statistics differ across protein models?**
	  + One lm model per summary statistic as `statistic ~ protein_model+ASRV`
	+ **Does literal branch lengths differ?**
	  + One lm per pair of protein models, per dataset, as `protein_model_bl ~ protein_model_bl`
	  + Spielman TODO list: Once able to run analyses on workstation (currently slammed with stuff for another couple weeks), will run more *high divergence* datasets here. So need to make sure code is *flexible enough* to handle new information, assuming same format. 