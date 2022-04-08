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

#join data ---------------------------------
combined_data <- sbl_data %>%
  left_join(de_data) %>%
  left_join(bs_data) %>% 
  left_join(ic_data) %>%
  #avoid getting confused with poor use of "site" term all over the place
  rename(np_sim_model = site, 
         `+G4`        = ASRV) %>% #which ASRV
  #right order for models
  mutate(model = factor(model, levels = model_levels),
         `+G4` = ifelse(`+G4` == FALSE, "No", "Yes"), # get away from logical
         `+G4` = fct_relevel(`+G4`, "Yes")
         ) 

#data for the ic tables ----------------------------------------
data_for_ic_tables <- combined_data %>%
  #have to select otherwise gt shows every single column
  select(np_sim_model, sim_branch_length, model, `+G4`, ic_type, ic_rank) %>%
  #don't want to filter so that function works
  group_by(ic_type) %>%
  #model is column names
  pivot_wider(names_from = "model",
              values_from = "ic_rank") %>%
  ungroup()

#make a tibble instead of function for coloring cells???? --------------------
 color_best_cell <- combined_data %>%
  group_by(ic_type) %>%
  #model is column names
  pivot_wider(names_from = "model",
              values_from = "ic_rank") %>%
   #no LG with ic_rank of 1
  select(FLU:WAG) %>%
  print()
