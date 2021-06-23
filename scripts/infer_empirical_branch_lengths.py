"""
Only reason this isn't a shell script is `ls *fasta` too long arg :(
"""
import os
import sys


path_to_data = sys.argv[1]
if not path_to_data.endswith("/"):
    path_to_data += "/"
outpath = sys.argv[2]
dataset_type = sys.argv[3]
threads      = sys.argv[4]

if dataset_type == "mammal":
    treefile = path_to_data + "mammal.tre"

   

q_options = ["WAG", "JTT", "LG", "FLU", "Poisson"]
g_options = ["+F", "+F+G"]

# some are phys, it's cool
fastas = [x for x in os.listdir(path_to_data) if x.endswith(".fasta")]

x=1
for fasta in fastas:
    print(x)
    x+=1
    
    fasfile = path_to_data + fasta
    if dataset_type != "mammal":
        treefile = fasfile.replace(".fasta", ".tre")
    if dataset_type == "bird" or dataset_type == "pandit":
        # Infer a quick fasttree. Does not need to be the best tree, just the SAME tree for all model bl optimizations
        cmd = "FastTree -quiet -fastest -noml " + fasfile + " > " + treefile
        cmdcode = os.system(cmd)
        if cmdcode != 0:
            print("BAD FASTREE:", fasta)
            continue
    for matrix in q_options:
        for gamma in g_options:
            model = matrix + gamma
            final_log = outpath + fasta.replace(".fasta", "") + "-" + model + ".log" 
            final_tree = outpath + fasta.replace(".fasta", "") + "-" + model + ".tre" 
            
            if (os.path.exists(final_log) and os.path.exists(final_tree)):
                continue
            # --tree-fix is borked, filed an issue: https://github.com/iqtree/iqtree2/issues/17
            # used -te instead for now ^^
            #cmd = "iqtree2 -T " + threads + " -s " + fasfile + " -t " + treefile + " --tree-fix --redo"
            cmd = "iqtree2 -T " + threads + " -s " + fasfile + " -m " + model + " -te " + treefile + " --redo --quiet"
            #print(cmd) 
            #assert 1==3
            cmdcode = os.system(cmd)
            if cmdcode != 0:
                print("---------------------------------------------------------")
                print("error: ", fasfile, model)  
                print(cmd) 
                print("---------------------------------------------------------")
                continue
            

            # Move files to final resting place 
            os.system("mv " + fasfile + ".log " + final_log)
            os.system("mv " + fasfile + ".treefile " + final_tree)
            os.system("rm " + fasfile + ".*")            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
        