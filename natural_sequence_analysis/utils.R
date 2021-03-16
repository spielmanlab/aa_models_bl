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

#################################################################





# This function will take a dataframe of branchlengths and a response variable and output is the lm as a tibble

#fit_model_ASRV <- function(input_df, response_variable)
#{
 # input_df %>%
   # mutate(ASRV_modified= if_else(ASRV==TRUE, "Yes", "No")) -> df_for_modeling
  
  # string works as solution to column argument b/c *IT'S A FORMULA*
 # formula <- paste(
  #  response_variable, 
  #  "~", 
  #  "model + ASRV_modified") %>% as.formula()
  
  #lm(response_variable ~ model + ASRV_modified, data = df_for_modeling) %>% 
 # lm(formula, data = df_for_modeling) %>%
  #  aov() %>%
  #  TukeyHSD() -> fitted_model
  
  #fitted_model$model %>% 
  #  as_tibble(rownames = "comparison")
#}






# Another way to accomplish the previous function: This function will take a dataframe of branchlengths and a response variable and the output is the lm as a tibble.

linear_model_function_with_curlies<-function(input_branchlength_df, dependent_variable_column)
{
  input_branchlength_df %>%
    mutate(ASRV_modified= if_else(ASRV==TRUE, "Yes", "No")) -> df_for_modeling
  
  lm({{dependent_variable_column}} ~ model+ ASRV_modified, data= df_for_modeling) %>%
    aov() %>%
    TukeyHSD()->fitted_model
  
  fitted_model$model %>% 
    as_tibble(rownames = "comparison")
}
  




#This function is useful for plotting any one of the columns in the branchlength dataframes and comparing each model's output to Poisson's output

plot_compare_function <- function(input_df, column_to_plot, plot_title, x_label, y_label)
{
  model_order <- c("JTT", "WAG", "LG", "FLU")
  input_df%>%
    select(model,ASRV,id,{{column_to_plot}})%>%
    pivot_wider(names_from = model, values_from = {{column_to_plot}})%>%
    pivot_longer(cols=all_of(model_order), # sshhhhh 
                 names_to = "other_models", 
                 values_to = "yaxis")%>%
    ggplot(aes(x=Poisson, yaxis))+
    geom_point()+
    theme_classic()+
    facet_grid(vars(ASRV), 
               vars(fct_relevel(other_models, model_order)))+
    geom_smooth(method = "lm")+
    geom_abline(color = "red") + 
    cowplot::panel_border() + 
    theme(strip.background =element_rect(fill="cornflowerblue"))+
    labs(title= plot_title, 
         x=x_label, y=y_label)
}
