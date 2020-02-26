library(tidyverse)
library(magrittr)

infbl_path_out  <- "../results/"
infbl_path_read <- "../results/merged_branch_lengths_counts.csv"
#     /home/piconef8/aa_models_bl/results/merged_branch_lengths_counts.csv
slope_bias_read <- "../results/modeled_slope_bias.csv"
entropy_read    <- "../results/np_site_dnds_entropy.csv"
bl_tibble       <- read_csv(infbl_path_read)
slope_tibble    <- read_csv(slope_bias_read)
entropy_tibble  <- read_csv(entropy_read)


bl_tibble %<>%
  unite(model_ASRV, model, ASRV, remove=FALSE)
bl_tibble$model_ASRV <- factor(bl_tibble$model_ASRV)
left_join(bl_tibble, slope_tibble) -> bl_slope_tibble
left_join(bl_slope_tibble, entropy_tibble) -> final_tibble
print(final_tibble)
entropy_value <- pull(final_tibble, entropy)
print(entropy_value[2])
# > levels(final_tibble$model_ASRV)
# [1] "FLU_FALSE"     "FLU_TRUE"      "JTT_FALSE"     "JTT_TRUE"      "LG_FALSE"      "LG_TRUE"      
# [7] "Poisson_FALSE" "Poisson_TRUE"  "WAG_FALSE"     "WAG_TRUE"     
#my_colors <- c() ## should be 10 long

final_tibble %>%
  dplyr::filter(site == "5") %>% dplyr::pull(entropy) %>% print()


final_tibble %>%
  dplyr::filter(site == "54") %>% ##### instead of "1" input$_____
  ggplot(aes(x = persite_count, y = branch_length, color = model)) +
  geom_point()  + 
  geom_smooth(method = "lm") +  #, se = FALSE) +
  facet_grid(ASRV~model) +#, scales="free") + 
  #facet_wrap(model~ASRV, nrow=5) + 
  geom_abline(color = "black") + 
  labs(title = "Count vs Branch Length", x = "Persite Count", y = "Branch Length") +
  theme_bw(base_size = 18) +
  theme(plot.background = element_rect(fill = "gray"),
        legend.position = "none", 
        strip.background = element_rect(color = "black", fill = "white")) -> p
 

print(p)

write_csv(final_tibble, "../results/final_tibble.csv")