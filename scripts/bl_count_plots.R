library(tidyverse)
library(magrittr)

infbl_path_out  <- "/home/piconef8/Downloads"
infbl_path_read <- "/home/piconef8/Downloads/merged_branch_lengths_counts.csv"

final_tibble <- read_csv(infbl_path_read)


final_tibble %<>%
  unite(model_ASRV, model, ASRV, remove=FALSE)
final_tibble$model_ASRV <- factor(final_tibble$model_ASRV)
# > levels(final_tibble$model_ASRV)
# [1] "FLU_FALSE"     "FLU_TRUE"      "JTT_FALSE"     "JTT_TRUE"      "LG_FALSE"      "LG_TRUE"      
# [7] "Poisson_FALSE" "Poisson_TRUE"  "WAG_FALSE"     "WAG_TRUE"     
#my_colors <- c() ## should be 10 long

final_tibble %>%
  dplyr::filter(site == "55") %>% ##### instead of "1" input$_____
  ggplot(aes(x = persite_count, y = branch_length, color = model_ASRV)) +
    geom_point()  + 
    geom_smooth(method = "lm") +  #, se = FALSE) +
    facet_grid(model~ASRV) +#, scales="free") + 
    #facet_wrap(model~ASRV, nrow=5) + 
    geom_abline(color = "red") + 
    theme_bw() + 
    theme(legend.position = "none") -> p
print(p)
