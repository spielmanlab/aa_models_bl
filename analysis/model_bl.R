### SJS

## SCRIPT NEEDS REVIVING AND IT IS NOT READY YET
# WHAT I THINK IT DID:
### This script runs some modeling on ../results/merged_branch_lengths_counts.csv
### Produces `modeled_slope_bias.csv`

library(tidyverse)
library(broom)
library(ggforce)

csv_path <- "../results/"
input_file  <- "merged_branch_lengths_counts.csv"
output_file <- "modeled_slope_bias.csv"

######## Fixes slope to 1 for an estimate of bias ############
lm_bl_count_bias <- function(df) {
    lm(branch_length ~ offset(persite_count), data = df) ## formula fixes slope = 1 to give us the bias of the model   
}         
##############################################################

####### Fixes intercept to 0 for an estimate of slope ########
lm_bl_count_slope <- function(df) {
  lm(branch_length ~ 0 + persite_count, data = df) #%>%
    #broom::tidy() %>%
    #pull(estimate)
  #result <- confint(fitted_model, "persite_count", level=1-SIG_THRESHOLD)
  ## within 1
  #lb <- result[[1]]
  #ub <- result[[2]]
  #if (1 >= lb & 1 <= ub) {  ## TODO: make this `dplyr::between`
  #  return (FALSE) ## NOT SIGNIFICANT
  #} else {
  #  return(TRUE) ## SIGNIFICANTLY DIFFERENT FROM 1
  #}
}
###############################################################

full_data <- read_csv(paste0(csv_path, input_file))


full_data %>%
    group_by(site, ASRV, model) %>%
    nest() %>%
    mutate(lm_slope = map(data, lm_bl_count_slope),
           lm_bias = map(data, lm_bl_count_bias)) -> full_data_models


### All slopes are significant
full_data_models %>%
    dplyr::select(site, ASRV, model, lm_slope) %>%
    mutate(lm_slope_tidy = map(lm_slope, broom::tidy)) %>%
    unnest(cols = c(lm_slope_tidy)) %>%
    ungroup() %>%
    dplyr::select(-lm_slope, -term, -std.error, -statistic) %>%
    rename(slope_of_yint0 = estimate, slope_p_value = p.value) %>%
    mutate(slope_p_value_corrected = slope_p_value * n(),
           slope_p_value_corrected = ifelse(slope_p_value_corrected >= 1, 1, slope_p_value_corrected)) -> modeled_slope

### ~ 70% of bias significant
full_data_models %>%
    dplyr::select(site, ASRV, model, lm_bias) %>%
    mutate(lm_bias_tidy = map(lm_bias, broom::tidy)) %>%
    unnest(cols = c(lm_bias_tidy)) %>%
    ungroup() %>%
    dplyr::select(-lm_bias, -term, -std.error, -statistic) %>%
    rename(bias = estimate, bias_p_value = p.value) %>%
    mutate(bias_p_value_corrected = bias_p_value * n(),
           bias_p_value_corrected = ifelse(bias_p_value_corrected >= 1, 1, bias_p_value_corrected)) -> modeled_bias

left_join(modeled_slope, modeled_bias) -> modeled_slope_bias

write_csv(modeled_slope_bias, paste0(csv_path, output_file))



## plotting the slope a little?

model_levels <- c("Poisson", "JTT", "WAG", "LG", "FLU")


ggplot(modeled_slope_bias, 
  aes(x = fct_relevel(model, model_levels), 
      color = ASRV, 
      y = slope_of_yint0)) + 
    geom_violin(alpha=0, position = position_dodge()) +
    geom_sina(position = position_dodge(),  size = 0.7) +
    # doesn't work need to wrangle blech.
    #stat_summary(position = "dodge", pch = 21, color = "black", aes(group = ASRV, fill=ASRV)) +
    scale_color_brewer(palette = "Dark2") + 
    geom_hline(yintercept=1) +
    ggtitle("Estimate of slope when y-intercept is fixed to 0 in `lm(bl ~ persite_sim_count)`")

    

ggplot(modeled_slope_bias, 
  aes(x = fct_relevel(model, model_levels), 
      color = ASRV, 
      y = bias)) + 
    geom_violin(alpha=0, position = position_dodge()) +
    geom_sina(position = position_dodge(),  size = 0.7) +
    scale_color_brewer(palette = "Dark2") + 
    geom_hline(yintercept=0) + 
    ggtitle("Estimate of y-intercept when slope is fixed to 1 in `lm(bl ~ persite_sim_count)`")
    
    



    
    
    
    
    
    
    
    
    
    
    