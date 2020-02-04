import numpy as np
import pyvolve
import sys
from Bio import SeqIO
from Bio.Seq import Seq
from Bio.Alphabet import IUPAC
g = pyvolve.Genetics()


## Using mutation rates from NP experiment
mutation_rates = {'AG':2.4e-5, 'TC':2.4e-5, 'GA':2.3e-5, 'CT':2.3e-5, 'AC':9.0e-6, 'TG':9.0e-6, 'CA':9.4e-6, 'GT':9.4e-6, 'AT':3.0e-6, 'TA':3.0e-6, 'GC':1.9e-6, 'CG':1.9e-6}

### Input arguments (all get read in as STRINGS!):
"""
1. The site to simulate (but index needs to be -1)
1. The length to simulate (for trial runs we want to _not_ do 1e6...)
3. The input file containing preferences, aka _simulation parameters_
4. The input branch length (will be divided by 3 so total inferred tree length = simulated branch length)
5. The output amino acid sequences
6. The output counts csv
"""
assert(len(sys.argv) == 7), "\nInput arguments not properly specified" ## length should be SIX: arg 1 is always name of script, then come the command line arguments of which we have 5.
site              = int(sys.argv[1]) - 1 ### Python indexing starts from 0, but the argument passed in will start at *1*
size_to_simulate  = int(sys.argv[2])
preference_file   = sys.argv[3]
branch_length     = float(sys.argv[4])
output_seq_file   = sys.argv[5]
output_count_file = sys.argv[6]



### Load preferences
prefs_all = np.loadtxt(preference_file, delimiter=",")
prefs = prefs_all[site] 
sitefit = np.log(  prefs/np.sum(prefs)   ) ## Take log of "preferences" to obtain "fitnesses"
assert len(sitefit) == 20 ## Before we proceed, make sure ("assert") that there are 20 fitness values in `sitefit`

## Simulate
treestring = "(t1:" + str(branch_length/3) + ", t2:" + str(branch_length/3) + ", t3:" + str(branch_length/3)+ ");"
print(treestring)
my_tree = pyvolve.read_tree(tree = treestring)
my_model = pyvolve.Model("mutsel", {"fitness": sitefit, "mu": mutation_rates})
my_partition = pyvolve.Partition(model = my_model, size = size_to_simulate)
evolve = pyvolve.Evolver(tree = my_tree, partitions = my_partition)
evolve(algorithm = 1, seqfile = False, ratefile = False, infofile = False, countfile = output_count_file)

### Translate sequences to amino acid
nucseqs = evolve.get_sequences()
aaseqs = {}
for id in nucseqs:
    coding_dna = Seq(str(nucseqs[id]), IUPAC.unambiguous_dna)
    protein = str(coding_dna.translate())
    aaseqs[id] = protein


### Save AA sequences to file
with open(output_seq_file, "w") as f:
    for entry in aaseqs:
        f.write(">" + entry + "\n" + aaseqs[entry] + "\n")




