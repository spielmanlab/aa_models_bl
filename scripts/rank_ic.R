## Takes `simulation_ic_scores.csv` and cleans it up to rank and weight for each BL based on AIC, AICc, BIC
library(tidyverse)


data_path <- file.path(here::here(), "results")
input_file <- file.path(data_path, "simulation_ic_scores.csv")
output_file <- file.path(data_path, "simulation_ic_ranks_weights_per_bl.csv")

raw_data <- read_csv(input_file)

# For each site and sim bl, rank models using each of the three ICs
raw_data %>%
  mutate(site = as.factor(site), 
         bl = as.factor(bl), 
         model = as.factor(model)) %>%
  pivot_longer(AIC:BIC, names_to = "ic_type", values_to = "ic_value") %>%
  group_by(site, bl, ic_type) %>%
  mutate(ic_rank = rank(ic_value, tie = "average")) %>%
  ungroup() %>%
  group_by(bl, ic_type) %>%
  arrange(site) %>%
  ungroup() %>%
  mutate(ASRV = if_else(str_detect(model,"\\+G"), TRUE, FALSE),
         model = str_replace(model, "\\+G", ""), 
         model = str_replace(model, "\\+F", "")) %>%
  rename(sim_branch_length = bl) -> ic_ranks

# Calculate ic weights
ic_ranks %>%
  filter(ic_rank == 1) %>%
  select(-ic_rank) %>%
  rename(ic_best = ic_value) %>%
  select(-model, -ASRV) -> best_models

ic_ranks %>%
  full_join(best_models) %>%
  group_by(site, sim_branch_length, ic_type) %>%
  mutate(weight = exp(-0.5*(ic_value - ic_best)))  %>%
  mutate(ic_weight =weight/sum(weight)) %>%
  select(site, sim_branch_length, model, ASRV, ic_type, ic_rank, ic_weight) -> ic_ranks_weights

write_csv(ic_ranks_weights, output_file)
