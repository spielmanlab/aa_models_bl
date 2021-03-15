library(tidyverse)



summarize_branch_lengths <- function(input_df) 
{
  input_df %>%
    group_by(model, ASRV, id)%>%
    summarize(treelength = sum(branch_length),
              mean_bl    = mean(branch_length),
              median_bl  = median(branch_length),
              sd_bl      = sd(branch_length),
              cov        = sd_bl / mean_bl, 
              min_bl     = min(branch_length),
              max_bl     = max(branch_length)) %>%
    ungroup()
}




add_asrv_function<-function(input_df_3)
{
  input_df_3%>%
    mutate(ASRV_modified= if_else(ASRV==TRUE, "Yes", "No"))
}







function_for_modeling<-function(input_df_2)
{
  lm(treelength ~ model+ ASRV_modified, data = input_df_2)%>% 
    aov() %>%
    TukeyHSD()
}



function_for_model_outputs<-function(input_df_4)
{
  input_df_4$model %>% 
    as_tibble(rownames = "comparison")
  
  
}


function_for_graphing<- function(input_df_5)
{
  input_df_5%>%
    select(model,ASRV,id,treelength)%>%
    filter(treelength<5)%>%
    pivot_wider(names_from = model, values_from = treelength)%>%
    pivot_longer(cols=c("FLU", "JTT" , "Poisson", "WAG"), 
                 names_to = "other_models", 
                 values_to = "treelength")%>%
    ggplot(aes(x=LG, y=treelength))+
    geom_point()+
    theme_classic()+
    facet_grid(vars(ASRV), vars(other_models))+
    geom_smooth(method = "lm")+
    geom_abline(color = "red") + 
    cowplot::panel_border() + 
    theme(strip.background =element_rect(fill="cornflowerblue"))+
    labs(title= "LG vs. Other model's Treelengths", x="Treelength obtained from LG", y="Treelength obtained from other models")
  
}



