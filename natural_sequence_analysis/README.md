## Goals

The primary goals for this project moving forward are to answer the following questions: 
 <br><br>
	1.) **Do branch length summary statistics differ across protein models?**
	 <br><br>
	   -Will include one lm model per summary statistic as `statistic ~ protein_model+ASRV`
	   <br><br>
	2.) **Does literal branch lengths differ?**
	   One lm per pair of protein models, per dataset, as `protein_model_bl ~ protein_model_bl`

## Notes
-would be interesting to run the analysis without the enzyme data to see if lower evolutionary change does anything to the models' estimates of branch length summary stats (perhaps not so much under/over estimation).
-still need to clean up the utils file
-use the gt package to make nice tables 
-





