"""
Only reason this isn't a shell script is `ls *fasta` too long arg :(
"""
import os
import sys


path_to_data = sys.argv[1]
if not path_to_data.endswith("/"):
    path_to_data += "/"
dataset_type = sys.argv[2]
threads      = sys.argv[3]

if dataset_type == "mammal":
    treefile = path_to_data + "mammal.tre"

outpath = path_to_data + "../" + dataset_type + "_optimized_bl/"
   

q_options = ["WAG", "JTT", "LG", "FLU", "Poisson"]
g_options = ["+F", "+F+G"]

fastas = [x for x in os.listdir(path_to_data) if x.endswith("fasta")]

x=1
for fasta in fastas:
    fasfile = path_to_data + fasta
    if dataset_type != "mammal":
        treefile = fasfile.replace(".fasta", ".tre")
    print(x) ## 9861 total
    x += 1
    for matrix in q_options:
        for gamma in g_options:
            model = matrix + gamma
            # --tree-fix is borked, filed an issue: https://github.com/iqtree/iqtree2/issues/17
            # used -te instead for now ^^
            #cmd = "iqtree2 -T " + threads + " -s " + fasfile + " -t " + treefile + " --tree-fix --redo"
            cmd = "iqtree2 -T " + threads + " -s " + fasfile + " -m " + model + " -te " + treefile + " --redo --quiet"
            #print(cmd) 
            cmdcode = os.system(cmd)
            assert(cmdcode == 0)   
            
            # Move files to final resting place 
            os.system("rm " + fasfile + "*gz " + fasfile + ".iqtree" )
            os.system("mv " + fasfile + ".* " + outpath)
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
        