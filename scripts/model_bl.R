### SJS
### This script runs some modeling on ../results/merged_branch_lengths_counts.csv

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
    rename(slope = estimate, slope_p_value = p.value) %>%
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



### Some plotting?!
ggplot(modeled_slope_bias, aes(x = model, color = ASRV, y = slope)) + 
    geom_jitter(position = position_jitterdodge(jitter.width=0.2), alpha=0.6) +
    scale_color_brewer(palette = "Dark2")
    
### Some plotting?!
ggplot(modeled_slope_bias, aes(x = model, fill = ASRV, y = slope)) + 
    geom_boxplot() +
    scale_fill_brewer(palette = "Dark2") +
    scale_y_continuous(limits = c(0,5))
    
    
ggplot(modeled_slope_bias, aes(x = slope, y = model, fill = ASRV)) + 
    geom_density_ridges(alpha = 0.4) +
    scale_fill_brewer(palette = "Dark2")

    
#### save for later, dealing with aic file. column names need changing too
aic <- read_csv("../results/parsed_aic.csv")
aic %>% 
    group_by(site, bl) %>% mutate(aicrank = rank(AIC)) %>% 
    filter(sites == 10) %>% 
    ggplot(aes(x = bl, y = aicrank, fill=models)) + 
    geom_col(position = position_dodge()) + 
    geom_hline(yintercept = 1:9) 
    
    
    
    
    
    
    
    
    
    
    
    