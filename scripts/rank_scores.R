library(shiny)
library(tidyverse)
library(shinythemes)

infbl_path_out  <- "../results/"
infbl_path_read <- "../results/merged_branch_lengths_counts.csv"
#     /home/piconef8/aa_models_bl/results/merged_branch_lengths_counts.csv
slope_bias_read <- "../results/modeled_slope_bias.csv"
entropy_read    <- "../results/np_site_dnds_entropy.csv"
aic_read        <- "../results/clean_parsed_aic.csv"
bl_tibble       <- read_csv(infbl_path_read)
slope_tibble    <- read_csv(slope_bias_read)
entropy_tibble  <- read_csv(entropy_read)
aic_tibble      <- read_csv(aic_read)


bl_tibble %>%
  unite(model_ASRV, model, ASRV, remove=FALSE)
bl_tibble$model_ASRV <- factor(bl_tibble$model_ASRV)
left_join(bl_tibble, slope_tibble) %>%
left_join(entropy_tibble) %>%
left_join(aic_tibble) ->   final_tibble

print(final_tibble)
entropy_value <- pull(final_tibble, entropy)
#print(entropy_value)
 
      final_tibble %>%  
      dplyr::filter(site == "54") %>% 
      group_by(site, model, ASRV ) %>% 
      mutate(rank_AIC = rank(AIC, na.last = FALSE)) %>% left_join(final_tibble) -> final_tibble
      
 
      
    #  ggplot(aes(x = model, y = branch_length, color = model)) +
    #  geom_point()  + 
    #  geom_smooth(method = "lm") +  #, se = FALSE) +
    #  facet_grid(ASRV~model) +#, scales="free") + 
      #facet_wrap(model~ASRV, nrow=5) + 
     # geom_abline(color = "black") + 
     # labs(title = "Count vs Branch Length", x = "Persite Count", y = "Branch Length") +
    #  theme_bw(base_size = 18) +
     # theme(plot.background = element_rect(fill = "white"),
      #      legend.position = "none", 
      #      strip.background = element_rect(color = "black", fill = "white")) +
     # geom_text( x = 0.6, y = 3, size=5, aes(label = round(bias, 4)),color = "black")
