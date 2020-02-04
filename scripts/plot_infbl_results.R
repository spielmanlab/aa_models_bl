library(ape)
library(tidytree)
library(tidyverse)
       


# To get started, this code reads in the true tree 
true_tree  <- read.tree("tree_for_simulation_unrooted_highbl.tre")
true_tree_tibble <- as_tibble(true_tree) %>%
                    select(label, branch.length) %>%
                    mutate(branch.length = branch.length * 0.1) %>%
                    rename(true.branch.length = branch.length) ### Makes these branch 

infbl_path_out <- "infbl2_plots/"
infbl_path_read <- "infbl2_simulations/"
infbl_path_read_counts <- "simulations_fasta_counts_mutationrates2/"
sim_list <- c(1, 2, "null")
bias_list <- c("0.2", "0.4", "0.6", "0.8")
bias_type_list <- c("GCbias", "ATbias")
model_list <- c("Poisson+F", "Poisson+F+G4", "GTR20", "GTR20+G4", "JTT+F", "JTT+F+G4", "LG+F", "LG+F+G4", "WAG+F", "WAG+F+G4")

complete_tibble <- tibble(label = as.character(), 
                          branch.length.as.counts = as.numeric(),
                          branch.length = as.numeric(),
                          model = as.character(), 
                          bias_type = as.character(),
                          bias = as.character())
count_tibble <-    tibble(label = as.character(), 
                          subtype = as.character(), 
                          count = as.numeric(), 
                          site = as.character())
     
for (sim in sim_list){
  for (bias in bias_list){
    for(bias_type in bias_type_list){
      counts <- read_csv(paste0(infbl_path_read_counts, "simulation_", sim, "_", bias, "_", bias_type, "_substitution_branch_counts.csv")) %>%              
                gather(subtype, count, codon_subs:nuc_subs) %>% 
                mutate(site = sim, bias  = bias, bias_type = bias_type)
    count_tibble <- bind_rows(count_tibble, counts)
    #print(count_tibble)
      for (model in model_list){
  
    ## step 2.2
    ### Setting up the code to read in the different simulations
        file <- paste0(infbl_path_read, "simulation_", sim, "_", bias, "_", bias_type, "_AA.fasta.treefile-", model)
        tree <- read.tree(file) 
        this_tibble <- as_tibble(tree) %>% select(label, branch.length) ### What code can you add to be CLEAR that this tibble refers to model = JTT? Hint: remember mutate()?
        m_tibble <- dplyr::mutate(this_tibble, model = model, bias = bias, bias_type = bias_type ,branch.length.as.counts = branch.length * 1000000, site = sim)
        #print(m_tibble)
        
        
        
        bind_rows(complete_tibble, m_tibble) -> complete_tibble #everything except for counts in tibble
        #print(complete_tibble)
        #stop("~~~~~~~~~~~~~~~~~~~~~~~~")
        #print(completed_tibble)
        #left_join(count_tibble, completed_tibble) -> final_tibble

        #print(final_tibble)
        #print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      }
    }
  }
}
print(complete_tibble, n = 500)
print(count_tibble, n = 500)
#print(head(complete_tibble))

left_join(count_tibble, complete_tibble) %>% 
  na.omit() %>% write_csv("threebranchlength.csv") %>%


filter(subtype == "nonsyn_subs")-> final_tibble
#print(final_tibble)

final_tibble %>%
  filter(site == "2") %>%
  ggplot(aes(x = count, y = branch.length.as.counts, color = model)) + geom_point() + geom_smooth(method = "lm", se = FALSE, color = "black") +
    facet_grid(bias_type+bias~model) + geom_abline(color = "red") + theme(legend.position = "none") + theme_bw() -> p
ggsave("plot_2.pdf", p, width = 20, height=16) -> my_plot


ggsave(paste0(infbl_path_out, "plot_simulation_null.pdf"), width=8, height=10, my_plot)


