## Written by SJS to compute mutsel-based dN/dS and entropy from NP preferences


from compute_dnds_from_mutsel import *
import pyvolve

def calculate_entropy(f):
    return -1. * np.sum ( f * np.log(f) )


mutation_rates = {'AG':2.4e-5, 'TC':2.4e-5, 'GA':2.3e-5, 'CT':2.3e-5, 'AC':9.0e-6, 'TG':9.0e-6, 'CA':9.4e-6, 'GT':9.4e-6, 'AT':3.0e-6, 'TA':3.0e-6, 'GC':1.9e-6, 'CG':1.9e-6}
preference_file = "../preferences/NP_prefs.csv"

outfile = "../results/np_site_dnds_entropy.csv"
outstring = "site,dnds,entropy\n"

prefs_all = np.loadtxt(preference_file, delimiter=",")
for i in range(len(prefs_all)):
    print(i)
    
    site_prefs = prefs_all[i]
    sitefit = np.log(  site_prefs/np.sum(site_prefs)   )
    assert len(sitefit) == 20 ## Before we proceed, make sure ("assert") that there are 20 fitness values in `sitefit`
    model = pyvolve.Model("mutsel", {"fitness": sitefit, "mu": mutation_rates})
    state_freqs = model.extract_state_freqs()
    #print(np.sum(state_freqs))
    c = dNdS_from_MutSel(state_freqs, mutation_rates)
    site_dnds = c.compute_dnds()
    h = calculate_entropy(state_freqs)
    
    outstring += str(i+1) + "," + str(site_dnds) + "," + str(h) + "\n"

with open(outfile, "w") as f:
    f.write(outstring.strip())