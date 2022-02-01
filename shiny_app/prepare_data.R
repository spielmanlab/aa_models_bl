
path_to_data <- file.path(here::here(), "results", "simulation_branch_lengths_counts.csv")
path_to_info <- file.path(here::here(), "results", "np_site_dnds_entropy.csv")

#read in data
#avoid getting confused with poor use of "site" term all over the place
data <- read_csv(path_to_data) %>% rename(np_sim_model = site)
info <- read_csv(path_to_info) %>% rename(np_sim_model = site)

data %<>% # assignment pipe. runs and assigns at the same time (see introverse)
  mutate(model = factor(model, levels = c("FLU", "LG", "JTT", "WAG", "Poisson")))

#table to have something for tab subsection
filler_table <- c("hello", "shiny", "world", "!!!!!!!!!!!!!")


# UI Variables ----------------------------------
yes_string <- "Yes"
no_string <- "No"
choices_line_of_best_fit <- c(yes_string, no_string)