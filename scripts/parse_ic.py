### SJS and FJP. Parse all IQTREE files for information theoretic criteria scores.

import os
import sys

TEXT_AIC  = "Akaike information criterion (AIC) score:"
TEXT_AICC = "Corrected Akaike information criterion (AICc) score:"
TEXT_BIC  = "Bayesian information criterion (BIC) score:"

def find_score(text, line):
    score = None
    if text in line: 
        score = line.split(" ")[-1].strip()
    return score


assert(len(sys.argv) == 2), "\nUSAGE: python3 parse_ic.py <path_to_input_files>"
input_path = sys.argv[1]
out_file = "../results/simulation_ic_scores.csv"


iq_files = [x for x in os.listdir(input_path) if x.endswith("iqtree")]

out_file = open(out_file, "w")
header = "site,bl,model,AIC,AICc,BIC\n"
out_file.write(header)

for file in iq_files:
    print(file)
    #if not file.endswith(".iqtree"):
    #    continue
    open_iqfile = open(input_path + file, "r")
    file_info = file.replace("bl","").replace("site","").replace(".iqtree","").split("_")
    aic_score  = None
    aicc_score = None
    bic_score  = None
    for line in open_iqfile:
        if aic_score is None:
            aic_score  = find_score(TEXT_AIC, line)
        if aicc_score is None:
            aicc_score = find_score(TEXT_AICC, line)
        if bic_score is None:
            bic_score  = find_score(TEXT_BIC, line)

    file_info.append(aic_score + "," + aicc_score + "," + bic_score)
    out_file.write(",".join(file_info) + "\n")
    open_iqfile.close()

out_file.close()



















                    
