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
  rename(np_sim_model = site) %>%
  mutate(model = factor(model, levels = c("FLU", "LG", "JTT", "WAG", "Poisson")))

# UI Variables ----------------------------------
min_np_model <- 1
max_np_model <- 498
yes_string <- "Yes"
no_string <- "No"
choices_line_of_best_fit <- c(yes_string, no_string)
choices_de <-  c("dnds", "entropy")
choices_bs <-  c("bias", "slope_when_yint0")

# Functions ----------------------------------------------
de_bs_plot_function <- function(x_axis, y_axis) {
  x_axis <- as.symbol(x_axis)
  y_axis <- as.symbol(y_axis)
  combined_data %>%
    ggplot() +
    aes(x = {{x_axis}}, 
        y = {{y_axis}}) +
    geom_hex(bins = 25) + #because SO many points
    scale_fill_viridis_c(option = 'viridis') +
    facet_grid(cols = vars(model),
               rows = vars(ASRV),
               scales = "free_y")
} 

