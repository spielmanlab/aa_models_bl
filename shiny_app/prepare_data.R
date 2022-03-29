#Data preparation file
model_levels <- c("FLU", "LG", "JTT", "WAG", "Poisson")

#path to data
path_to_sbl <- file.path(here::here(), "results_now", "simulation_branch_lengths_counts.csv")
path_to_de <- file.path(here::here(), "results_now", "np_site_dnds_entropy.csv")
path_to_bs <- file.path(here::here(), "results_now", "simulation_bias_slope.csv")
path_to_ic <- file.path(here::here(), "results_now", "simulation_ic_ranks_weights_per_bl.csv")

#read in data
sbl_data <- read_csv(path_to_sbl)
de_data <- read_csv(path_to_de) 
bs_data <-read_csv(path_to_bs) %>% select(-slope_p_value, -bias_p_value)
ic_data <-read_csv(path_to_ic)

#sbl_data %<>% # assignment pipe. runs and assigns at the same time (see introverse)
 # mutate(model = factor(model, levels = c("FLU", "LG", "JTT", "WAG", "Poisson")))

#join data
combined_data <- sbl_data %>%
  left_join(de_data) %>%
  left_join(bs_data) %>% 
  left_join(ic_data) %>%
  #avoid getting confused with poor use of "site" term all over the place
  rename(np_sim_model = site,
         `+G4` = ASRV) %>% #which ASRV
  #right order for models
  mutate(model = factor(model, levels = model_levels)#,
         #`+G4`= as.factor(`+G4`), #no work
         #`+G4` = fct_relevel(`+G4`, TRUE)#TRUE is first, this column is a logical vector column not factor so fct_relevel or fct_reorder doesn't work?
         ) 
