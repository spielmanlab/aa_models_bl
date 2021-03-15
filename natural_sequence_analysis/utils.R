#library(tidyverse)


summarize_branch_lengths <- function(input_df) 
{
  input_df %>%
    group_by(model, ASRV, id)%>%
    summarize(treelength = sum(branch_length),
              mean_bl    = mean(branch_length),
              median_bl  = median(branch_length),
              sd_bl      = sd(branch_length),
              cov_bl     = sd_bl / mean_bl, 
              min_bl     = min(branch_length),
              max_bl     = max(branch_length)) %>%
    ungroup()
}

# This function will take a dataframe of treelengths, protein model, ASRV and lm treelength
fit_model_ASRV <- function(input_df, response_variable)
{
  input_df %>%
    mutate(ASRV_modified= if_else(ASRV==TRUE, "Yes", "No")) -> df_for_modeling
  
  # string works as solution to column argument b/c *IT'S A FORMULA*
  formula <- paste(
    response_variable, 
    "~", 
    "model + ASRV_modified") %>% as.formula()
  
  #lm(response_variable ~ model + ASRV_modified, data = df_for_modeling) %>% 
  lm(formula, data = df_for_modeling) %>%
    aov() %>%
    TukeyHSD() -> fitted_model
  
  fitted_model$model %>% 
    as_tibble(rownames = "comparison")
}

  

plot_compare_treelengths<- function(input_df)
{
  input_df%>%
    select(model,ASRV,id,treelength)%>%
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



