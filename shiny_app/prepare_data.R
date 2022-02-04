#path to data
path_to_sbl <- file.path(here::here(), "results", "simulation_branch_lengths_counts.csv")
path_to_de <- file.path(here::here(), "results", "np_site_dnds_entropy.csv")
path_to_bs <- file.path(here::here(), "results", "simulation_bias_slope.csv")

#read in data
sbl_data <- read_csv(path_to_sbl)
de_data <- read_csv(path_to_de) 
bs_data <-read_csv(path_to_bs) %>% select(-slope_p_value, -bias_p_value)

#sbl_data %<>% # assignment pipe. runs and assigns at the same time (see introverse)
 # mutate(model = factor(model, levels = c("FLU", "LG", "JTT", "WAG", "Poisson")))

#join data
sbl_de_bs_data <- sbl_data %>%
  left_join(de_data) %>%
  left_join(bs_data) %>% 
  #avoid getting confused with poor use of "site" term all over the place
  rename(np_sim_model = site) %>%
  mutate(model = factor(model, levels = c("FLU", "LG", "JTT", "WAG", "Poisson")))

# UI Variables ----------------------------------
min_np_model <- 1
max_np_model <- 498
yes_string <- "Yes"
no_string <- "No"
choices_line_of_best_fit <- c(yes_string, no_string)
choices_de <- names(sbl_de_bs_data)[names(sbl_de_bs_data) %in% c("dnds", "entropy")]
choices_bs <- names(sbl_de_bs_data)[names(sbl_de_bs_data) %in% c("bias", "slope_when_yint0")]