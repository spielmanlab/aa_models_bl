### SJS

## SCRIPT NEEDS REVIVING AND IT IS NOT READY YET
# WHAT I THINK IT DID:
### This script runs some modeling on ../results/merged_branch_lengths_counts.csv
### Produces `modeled_slope_bias.csv`

## Libraries ------------------------------
library(tidyverse)
library(broom)
library(ggforce)

## Functions -------------------------------
estimate_bias <- function(df) {
  # Fixes slope = 1
  lm(branch_length ~ offset(persite_count), data = df) 
}   

estimate_slope <- function(df) {
  # Fixes intercept = 1
  lm(branch_length ~ 0 + persite_count, data = df)
}   



## Paths -----------------------------------

csv_path <- file.path(here::here(),  "results/")
input_file  <- file.path(csv_path, "simulation_branch_lengths_counts.csv")
output_file <- file.path(csv_path, "simulation_bias_slope.csv")


## Analysis ----------------------------------
data <- read_csv(input_file)

data %>%
    group_by(site, ASRV, model) %>%
    nest() %>%
    mutate(lm_slope = map(data, estimate_slope),
           lm_bias = map(data, estimate_bias)) -> data_with_lm


# Process slopes
data_with_lm %>%
    select(site, ASRV, model, lm_slope) %>%
    mutate(lm_slope_tidy = map(lm_slope, broom::tidy)) %>%
    unnest(cols = c(lm_slope_tidy)) %>%
    ungroup() %>%
    dplyr::select(-lm_slope, -term, -std.error, -statistic) %>%
    rename(slope_when_yint0 = estimate, slope_p_value = p.value) %>%
    mutate(slope_p_value_corrected = slope_p_value * n(), # correct p-values based on number of tests (one row per test)
           slope_p_value_corrected = ifelse(slope_p_value_corrected >= 1, 1, slope_p_value_corrected)) -> modeled_slope

# Process bias
data_with_lm %>%
    dplyr::select(site, ASRV, model, lm_bias) %>%
    mutate(lm_bias_tidy = map(lm_bias, broom::tidy)) %>%
    unnest(cols = c(lm_bias_tidy)) %>%
    ungroup() %>%
    dplyr::select(-lm_bias, -term, -std.error, -statistic) %>%
    rename(bias = estimate, bias_p_value = p.value) %>%
    mutate(bias_p_value_corrected = bias_p_value * n(),  # correct p-values based on number of tests (one row per test) 
           bias_p_value_corrected = ifelse(bias_p_value_corrected >= 1, 1, bias_p_value_corrected)) -> modeled_bias

# Combine datasets
left_join(modeled_slope, modeled_bias) -> modeled_slope_bias

# Save
write_csv(modeled_slope_bias, output_file)



    
    
    
    
    
    
    
    
    
    
    