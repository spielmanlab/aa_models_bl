## THIS IS NOT A SHINY APP, WE DON'T NEED SHINY LIBRARIES
#library(shiny)
#library(shinythemes)


library(tidyverse)

# Define file paths -------------------------------------------
infbl_path_out  <- "../results/"
infbl_path_read <- file.path(infbl_path_out, "merged_branch_lengths_counts.csv")
slope_bias_read <- file.path(infbl_path_out, "modeled_slope_bias.csv")
entropy_read    <- file.path(infbl_path_out, "np_site_dnds_entropy.csv")
aic_read        <- file.path(infbl_path_out, "clean_parsed_aic.csv")

# Read input files --------------------------------------------
bl_tibble       <- read_csv(infbl_path_read)
slope_tibble    <- read_csv(slope_bias_read)
entropy_tibble  <- read_csv(entropy_read)
aic_tibble      <- read_csv(aic_read)


# Join input --------------------------------------
## THE LINES BELOW DON'T WORK PROPERLY - YOU NEED TO SAVE THE OUTPUT FROM LINES 24/25 TO GET 26 TO WORK...
bl_tibble %>%
  unite(model_ASRV, model, ASRV, remove=FALSE)
bl_tibble$model_ASRV <- factor(bl_tibble$model_ASRV)
####################################################

left_join(bl_tibble, slope_tibble) %>%
left_join(entropy_tibble) %>%
left_join(aic_tibble) ->   final_tibble

print(final_tibble)

### THE LINE BELOW REMAINS UNUSED IN THIS SCRIPT???
entropy_value <- pull(final_tibble, entropy)
#print(entropy_value)
 

final_tibble %>%  
  #dplyr::filter(site == "54") %>%   ## WE NEED A GENERAL RANKING, NOT ONLY FOR THIS SITE
  group_by(site, model, ASRV ) %>% 
  mutate(rank_AIC = rank(AIC, na.last = FALSE)) %>% left_join(final_tibble) -> final_tibble
      
 