import os
import sys


TL_STRING = "Total tree length:"

def extract_tree_length(string):
    return string.replace(TL_STRING, "").strip()



path_to_logs = sys.argv[1]
datatype     = sys.argv[2] # mammal, enzyme
if not path_to_logs.endswith("/"):
    path_to_logs += "/"
outfile = "../results/empirical_treelengths_" + datatype + ".csv"
outstring = "name,model,tree_length\n"

logs = [x for x in os.listdir(path_to_logs) if x.endswith(".log")]

x=1 # just so I can see the numbers go by. Soothing?
for logfile in logs:
    print(x)
    x+=1 
    tree_length = None
    model = logfile.split("-")[1].replace(".log", "")
    name = logfile.split("-")[0]
    with open(path_to_logs + logfile, "r") as f:
        for line in f:
            if line.startswith(TL_STRING):
                tree_length = extract_tree_length(line)
    assert(tree_length is not None), "BAD TL PARSE"
    outstring += ",".join([name, model, tree_length]) + "\n"
    #print(outstring)
    #assert 1==3
    
with open(outfile, "w") as f:
    f.write(outstring.strip())
                
    