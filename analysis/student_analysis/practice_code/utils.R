#Takes one of the original empirical datasets and creates a tibble with several branchlength-related measurements.
#param input_df: the empirical dataset of interest
#returns: a tibble with several branchlength-related measurements
summarize_branch_lengths <- function(input_df) 
{
  input_df %>% #dataframe of interest
    group_by(model, ASRV, id)%>% #need to group before summarizing
    summarize(treelength = sum(branch_length),
              mean_bl    = mean(branch_length),
              median_bl  = median(branch_length),
              sd_bl      = sd(branch_length),
              cov_bl     = sd_bl / mean_bl, 
              min_bl     = min(branch_length),
              max_bl     = max(branch_length)) %>% #these are all the measurements we want
    ungroup()
}






#This function takes one of the branchlength dataframes obtained from the previous function and runs a lm of a column of choice ~ model + ASRV.
#param input_branchlength_df: the branchlength dataframe of interest
#param dependent_variable_column: which column to use as the dependent variable
#returns: a tibble with tukey comparisons between each of the models (JTT, FLU, WAG, Poisson)
linear_model_function_curlies<-function(input_branchlength_df, dependent_variable_column)
{
  input_branchlength_df %>%
    mutate(ASRV_modified= if_else(ASRV==TRUE, "Yes", "No")) -> df_for_modeling #need to make ASRV non-boolean
  
  lm({{dependent_variable_column}} ~ model+ ASRV_modified, data= df_for_modeling) %>% #since we are referring to a column,we need to use curlies
    aov() %>%
    TukeyHSD()->fitted_model
  
  fitted_model$model %>% 
    as_tibble(rownames = "comparison") #this will make the output a little nicer
}
  




#This function is useful for plotting any one of the columns in the branchlength dataframes and comparing each model's output to Poisson's output
#param input_df: Any one of the branchlength dataframes
#param column_to_plot: which column from the branchlength dataframe to plot
#param plot_title: plot title
#param x_label: x axis label
#param y_label: y-axis label
#returns: a plot that is faceted by ASRV and and shows Poisson's estimate along the X axis and every other model's estimate along the Y axis.
plot_compare_function <- function(input_df, column_to_plot, plot_title, x_label, y_label)
{
  model_order <- c("JTT", "WAG", "LG", "FLU")
  input_df%>%
    select(model,ASRV,id,{{column_to_plot}})%>% #use curlies since we are referring to a specific column
    pivot_wider(names_from = model, values_from = {{column_to_plot}})%>%
    pivot_longer(cols=all_of(model_order), 
                 names_to = "other_models", 
                 values_to = "yaxis")%>% #this pivoting sequence allows us to isolate Poisson from the rest of the models
    ggplot(aes(x=Poisson, yaxis))+
    geom_point()+
    theme_classic()+
    facet_grid(vars(ASRV), 
               vars(fct_relevel(other_models, model_order)))+ #want a 4 column by two row output with columns representing models and rows representing ASRV status (T/F)
    geom_smooth(method = "lm")+
    geom_abline(color = "red") + #add in the y=x line for easy analysis of the output
    cowplot::panel_border() + 
    theme(strip.background =element_rect(fill="cornflowerblue"))+
    labs(title= plot_title, 
         x=x_label, y=y_label)
}



#This function is simply a lm of FLU~Poisson that will be utilized as a piece of a later function
#param df: a dataframe that contains FLU and Poisson branchlengths
#returns: a lm of FLU ~ Poisson
lm_with_purr<-function(df)
{
  lm(FLU~Poisson, data=df)
  
}

#This function takes the bird, enzyme, mammal, or mega dataframe and creates an lm output for FLU~Poisson branchlength estimates
#param input_df: the dataframe of interest (birds, mammals, enzymes, or mega_empirical_dataset)
#returns: A tibble with the intercept and slope of the lm along with corresponding p values and r squared values. Every unique id has 2 rows with one dedicated to slope and the other to intercept
Poisson_FLU_lm <-function (input_df)
{ 
  input_df%>%
    filter(ASRV == TRUE) %>% 
    select(-ASRV) %>% 
    group_by(id, dataset) %>% 
    pivot_wider(names_from = model, values_from = branch_length)%>% #pivot to get branchlength values in columns
    select(Poisson, FLU)%>% #only want these two models
    group_by(dataset, id)%>%
    nest()%>% #this makes a sort of mini-tibble under each column (list)
    mutate(lm_fit=map(data, lm_with_purr))%>% #run the lm_with_purr function with the 'data' column as the argument
    select(-data)%>%
    mutate(tidied=map(lm_fit,broom::tidy), glanced=map(lm_fit, broom::glance))%>% #this will help with the output being easier to look at
    select(-lm_fit)%>%
    unnest(glanced)%>%
    select(id, tidied, r.squared, rsq.pvalue = p.value)%>%
    unnest(tidied)%>%
    select(-std.error)%>%
    pivot_wider(names_from = term, values_from = estimate)%>% #pivoting wider here makes one row for each id dedicated to intercept, and the other to slope
    rename(Slope=Poisson, Intercept=`(Intercept)`)%>%
    select(id, Intercept, Slope, p.value, r.squared, rsq.pvalue, -statistic)
  
}





#This function is meant to be used as part of a later function (and I am unsure if it is even usable) but it is meant to be an lm of (model~model) where possible models are (JTT, FLU, WAG, Poisson, LG) 
#param model 1: the first model of interest
#param model 2: the second model of interest
#param df: a dataframe that contains branchlength estimates from the models of interest
#returns: lm of (model~model)
lm_any_model<-function(model1, model2, df)
{
  lm({{model1}}~{{model2}}, data=df)
}


#this function is a work in progress but the goal is to perform an lm utilizing branchlength estimates for any two models (model1~model2) where models are (JTT, FLU, LG, WAG, Poisson).
#param input_df: dataframe (birds, mammals, enzymes, or mega_empirical_dataset)
#param ASRV_T_F: True or false for ASRV
#param model1: first model (will serve as dependent var)
#param model2: second model (will serve as independent var)
#returns: A tibble with the intercept and slope of the lm along with corresponding p values and r squared values. Every unique id has 2 rows with one dedicated to slope and the other to intercept
lm_two_models<- function (input_df, ASRV_T_F, model1, model2)
{
    input_df%>%
    filter(ASRV == ASRV_T_F) %>% 
    select(-ASRV) %>% 
    group_by(id, dataset) %>% 
    pivot_wider(names_from = model, values_from = branch_length)%>%
    select({{model1}}, {{model2}})%>%
    group_by(dataset, id)%>%
    nest()%>%
    mutate(lm_fit=map(data,lm_any_model))%>%
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
    








