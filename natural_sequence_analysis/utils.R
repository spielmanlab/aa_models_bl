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




##############################################################################################################

# Another way to accomplish the previous function: This function will take a dataframe of branchlengths and a dependent variable column and the output is the lm as a tibble.

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


#lets try to purr-------------------------------------------------------------------------------
#want a lm for each bird ID. The output will be id, Intercept, Slope, pvalue (of intercept or slope), rsquared, and rsquared pvalue)
#end up with NA's but I like the wider version better since Intercept and slope are their own columns




 #Create a mega dataset with birds, mammals, and enzymes 
birds$id=as.character(birds$id)#had to do this to bind the rows


bind_rows(enzymes, mammals, birds)->mega_empirical_dataset



lm_function_with_purr<-function(df)
{
  lm(FLU~Poisson, data=df)
  
}


#This function takes the bird, enzyme, mammal, or mega dataframe and creates an lm output for FLU~Poisson 
function_for_Poisson_FLU_lm <-function (input_df)
{ 
  input_df%>%
    filter(ASRV == TRUE) %>% 
    select(-ASRV) %>% 
    group_by(id, dataset) %>% 
    pivot_wider(names_from = model, values_from = branch_length)%>%
    select(Poisson, FLU)%>%
    group_by(dataset, id)%>%
    nest()%>%
    mutate(lm_fit=map(data, lm_function_with_purr))%>%
    select(-data)%>%
    mutate(tidied=map(lm_fit,broom::tidy), glanced=map(lm_fit, broom::glance))%>%
    select(-lm_fit)%>%
    unnest(glanced)%>%
    select(id, tidied, r.squared, rsq.pvalue = p.value)%>%
    unnest(tidied)%>%
    select(-std.error)%>%
    pivot_wider(names_from = term, values_from = estimate)%>%
    rename(Slope=Poisson, Intercept=`(Intercept)`)%>%
    select(id, Intercept, Slope, p.value, r.squared, rsq.pvalue, -statistic)
  
}


#lets up the game again, make a function that has which models we choose to lm as variables----------------

lm_function_with_purr_curlies<-function(model1, model2, df)
{
  lm({{model1}}~{{model2}}, data=df)
}



Function_for_lm_for_any_dataset_and_any_two_models<- function (input_df, ASRV_T_F, model1, model2)
{
    input_df%>%
    filter(ASRV == ASRV_T_F) %>% 
    select(-ASRV) %>% 
    group_by(id, dataset) %>% 
    pivot_wider(names_from = model, values_from = branch_length)%>%
    select({{model1}}, {{model2}})%>%
    group_by(dataset, id)%>%
    nest()%>%
    mutate(lm_fit=map(data,lm_function_with_purr_curlies))%>%
    select(-data)%>%
    mutate(tidied=map(lm_fit,broom::tidy), glanced=map(lm_fit, broom::glance))%>%
    select(-lm_fit)%>%
    unnest(glanced)%>%
    select(id, tidied, r.squared, rsq.pvalue = p.value)%>%
    unnest(tidied)%>%
    select(-std.error)%>%
    pivot_wider(names_from = term, values_from = estimate)%>%
    rename(Slope=Poisson, Intercept=`(Intercept)`)%>%
    select(id, Intercept, Slope, p.value, r.squared, rsq.pvalue, -statistic)
  
  
  
  }
    








