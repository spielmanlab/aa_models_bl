## Takes `simulation_ic_scores.csv` and cleans it up to rank for each BL based on AIC, AICc, BIC
library(tidyverse)


data_path <- file.path(here::here(), "results")
input_file <- file.path(data_path, "simulation_ic_scores.csv")
output_file <- file.path(data_path, "simulation_ic_ranks_per_bl.csv")

raw_data <- read_csv(input_file)

raw_data %>%
  mutate(site = as.factor(site), 
         bl = as.factor(bl), 
         model = as.factor(model)) %>%
  pivot_longer(AIC:BIC, names_to = "ic_type", values_to = "ic_value") %>%
  group_by(site, bl, ic_type) %>%
  mutate(ic_rank = rank(ic_value)) %>%
  ungroup() %>%
  group_by(bl, ic_type) %>%
  arrange(site) -> ic_ranks

write_csv(ic_ranks, output_file)
