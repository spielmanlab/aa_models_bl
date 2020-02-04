import os
import sys
import csv

TEXT_AIC  = "Akaike information criterion (AIC) score:"
TEXT_AICC = "Corrected Akaike information criterion (AICc) score:"
TEXT_BIC  = "Bayesian information criterion (BIC) score:"

def find_score(text, line):
    if text in line: 
		score = line.split(" ")[-1].strip()
    return score
    
    
input_path = "../branch_length_inference/"
output_path = "../results/"


iq_files = os.listdir(input_path)

out_file = open(output_path + "parsed_aic.csv", "w")
header = "sites,bls,models,aic,aicc,bic\n"
out_file.write(header)

for file in iq_files:
    if not file.endswith(".iqtree"):
        continue
    open_iqfile = open(input_path + file, "r")
	file_info = file.split("_")
	
	for line in open_iqfile:
	    aic_score  = find_score(TEXT_AIC, line)
	    aicc_score = find_score(TEXT_AICC, line)
	    bic_score  = find_score(TEXT_BIC, line)

        file_info.append(aic_score + "," + aicc_score + "," + bic_score)
		#print(",".join(split_files)+"\n")
		out_file.write(",".join(file_info) + "\n")

	open_iqfile.close()

out_file.close()



















                    
