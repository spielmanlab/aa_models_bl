args = commandArgs(trailingOnly=TRUE)

library(tidyverse)
library(ape)
library(tidytree)


SIM_LENGTH    <- 1e4
count_path <- args[1] #"../simulations_counts/"
tree_path  <- args[2] # "../branch_length_inference/"
out_path   <- file.path(here::here(), "results/")

##### Read in all count csv files into a single tibble ########
count_files <- dir(path = count_path, pattern = "*.csv")
tibble(filename = count_files) %>%
  mutate(file_contents = map(filename,
           ~ read_csv(file.path(count_path, .))))  -> nested_count_files

nested_count_files %>%
  unnest(cols = c(file_contents)) %>%
  separate(filename, c("info", "bye"), sep="\\.csv") %>% #site1_bl0.001.csv 
  separate(info, c("sitenumber", "blnumber"), sep = "_") %>%
  mutate(site = str_replace(sitenumber, "site", ""),
         site = as.numeric(site),
         sim_branch_length = str_replace(blnumber, "bl", ""), 
         sim_branch_length = as.numeric(sim_branch_length)) %>%
  filter(substitution_type == "amino_acid") %>% 
  mutate(persite_count = count/SIM_LENGTH) %>%
  group_by(site, sim_branch_length) %>%
  mutate(count = sum(count),
         persite_count = sum(persite_count)) %>%
  ungroup() %>%
  dplyr::select(-bye, -branch_name, -sitenumber, -blnumber, -substitution_type) %>%
  distinct() -> site_aa_counts
#write_csv(site_aa_counts, paste0(out_path,"simulation_aa_counts.csv"))

######### Read in all branch length (tree) files into a single tibble
tree_files <- dir(path = tree_path, pattern = "*.treefile")
tibble(filename = tree_files) %>%
  mutate(file_contents = map(filename,
           ~ as_tibble(read.tree(file.path(tree_path, .))))) -> nested_tree_files

nested_tree_files %>%
  unnest(cols = c(file_contents)) %>%
  separate(filename, c("need", "bye"), sep="\\.treefile") %>%
  separate(need, c("site", "sim_branch_length", "model"), sep = "_") %>%
  mutate(site = str_replace(site, "site", ""),
         site = as.numeric(site),
         sim_branch_length = str_replace(sim_branch_length, "bl", ""), 
         sim_branch_length = as.numeric(sim_branch_length), 
         model = str_replace(model, "\\+F", "")) %>%
  rename(branch_length = branch.length) %>%
  na.omit() %>%
  group_by(site, sim_branch_length, model) %>%
  mutate(branch_length = sum(branch_length)) %>%
  dplyr::select(-bye, -parent, -node, -label) %>%
  ungroup() %>%
  distinct() -> branch_lengths
#write_csv(branch_lengths, paste0(out_path, "inferred_branch_lengths.csv"))


### Join site_aa_counts and branch_lengths by site and branch_name to obtain single full tibble for all data ####
### In addition, create three new columns to represent model for its matrix and its +G "status" so they can all be treated as separate variables in linear models
left_join(site_aa_counts, branch_lengths, by = c("site", "sim_branch_length")) %>% 
    na.omit() %>%
    mutate(ASRV = ifelse(str_detect(model,"\\+G"), TRUE, FALSE)) %>%
    rowwise() %>%
    mutate(model = str_replace(model, "\\+G", "")) -> full_data

write_csv(full_data, paste0(out_path,"simulation_branch_lengths_counts.csv"))
