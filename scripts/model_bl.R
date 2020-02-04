library(tidyverse)
library(magrittr)
library(modelr)
library(ape)
library(tidytree)


SIM_LENGTH    <- 1e4
SIG_THRESHOLD <- 0.01
count_path <- "../simulations_counts/"
tree_path  <- "../branch_length_inference/"
out_path   <- "../results/"

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
write_csv(site_aa_counts, paste0(out_path,"simulation_aa_counts.csv"))

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
write_csv(branch_lengths, paste0(out_path, "inferred_branch_lengths.csv"))


### Join site_aa_counts and branch_lengths by site and branch_name to obtain single full tibble for all data ####
### In addition, create three new columns to represent model for its matrix and its +G "status" so they can all be treated as separate variables in linear models
left_join(site_aa_counts, branch_lengths, by = c("site", "sim_branch_length")) %>% 
    na.omit() %>%
    mutate(ASRV = ifelse(str_detect(model,"\\+G"), TRUE, FALSE)) %>%
    rowwise() %>%
    mutate(model = str_replace(model, "\\+G", "")) -> full_data

write_csv(full_data, paste0(out_path,"merged_branch_lengths_counts.csv"))

###### Fixes slope to 1 for an estimate of bias ########
lm_bl_count_offset <- function(df) {
    lm(branch_length ~ offset(persite_count), data = df) ## formula fixes slope = 1 to give us the bias of the model   
}         


####### Fixes intercept to 0 for an estimate of slope ########
lm_bl_count <- function(df) {
  fitted_model <- lm(branch_length ~ 0 + persite_count, data = df)
  result <- confint(fitted_model, "persite_count", level=1-SIG_THRESHOLD)
  ## within 1
  lb <- result[[1]]
  ub <- result[[2]]
  if (1 >= lb & 1 <= ub) {  ## lb <= 1 <= ub
    return (FALSE) ## NOT SIGNIFICANT
  } else {
    return(TRUE) ## SIGNIFICANTLY DIFFERENT FROM 1
  }
} 



full_data %>%
    group_by(site, ASRV, model) %>%
    nest() %>%
    mutate(slope_differs_from_1 = map_lgl(data, lm_bl_count),
           lm_bias = map(data, lm_bl_count_offset),
           lm_bias_tidy = map(lm_bias, broom::tidy)) %>%
    unnest(lm_bias_tidy) %>%
    rename(bias = estimate, bias_se = std.error, bias_pvalue = p.value) %>%
    ungroup() %>%
    mutate(n = n(),
           bias_pvalue = bias_pvalue * n,  ## eh some bonf stuff?
           bias_pvalue = ifelse(bias_pvalue >= 1, 1, bias_pvalue)) %>%
        dplyr::select(-lm_bias, -data, -term, -statistic, -n) -> modeled_data


modeled_data %>% 
    ungroup() %>%
    filter(!str_detect(model, "model")) %>%
    mutate(sig_bias = bias_pvalue <= SIG_THRESHOLD) %>%
    count(sig_bias, ASRV, model) %>%
    ungroup() %>%
    group_by(model, ASRV) %>%
    spread(sig_bias, n) %>%
    replace_na(list(`TRUE`=0, `FALSE`=0)) %>%
    mutate(prop_biased = `TRUE`/(`FALSE` + `TRUE`)) %>%
    dplyr::select(-`TRUE`, -`FALSE`) -> final_bias
    
modeled_data %>% 
    ungroup() %>%
    filter(!str_detect(model, "model")) %>%
    count(slope_differs_from_1, ASRV, model) %>%
    ungroup() %>%
    group_by(model, ASRV) %>%
    spread(slope_differs_from_1, n) %>%
    replace_na(list(`TRUE`=0, `FALSE`=0)) %>%
    mutate(prop_slope_not_1 = `TRUE`/(`FALSE` + `TRUE`)) %>%
    dplyr::select(-`TRUE`, -`FALSE`) %>%
    left_join(final_bias) -> final_results

write_csv(final_results, paste0(out_path, "final_modeled_slope_bias.csv"))

