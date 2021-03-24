# Load libraries ------------------------------------------------------------------------------------------------
library(tidyverse)
library(lme4) # for random effects linear model
library(cowplot)

source("utils.R")

# Define important variables ------------------------------------------------------------------------------------
conditions_per_dataset <- 10  # 5 models * 2 ASRV = 10


# Load datasets --------------------------------------------------------------------------------------------------

# relative path! goal for all scripts = MEANT to run from directory where script lives 
path_to_data <- "./"


birds_file <- file.path(path_to_data, "bird_empirical_branch_lengths.csv")
mammals_file <- file.path(path_to_data, "mammal_empirical_branch_lengths.csv")
enzymes_file <- file.path(path_to_data, "enzyme_empirical_branch_lengths.csv")


mammals <- read_csv(mammals_file)
enzymes <- read_csv(enzymes_file)
birds   <- read_csv(birds_file)

# Wrangle into branch lengths tibbles using functions ------------------------------------------------------------


bird_branch_lengths <- summarize_branch_lengths(birds)
mammal_branch_lengths <- summarize_branch_lengths(mammals)
enzyme_branch_lengths <- summarize_branch_lengths(enzymes)


# DEPRECATED:  How does model+ASRV affect treelength? -----------------------------------------------------------------------------------------------------------------
#fit_treelength_model_ASRV(mammal_branch_lengths) -> fitted_mammals_treelengths
#fit_treelength_model_ASRV(bird_branch_lengths)   -> fitted_bird_treelengths
#fit_treelength_model_ASRV(enzyme_branch_lengths) -> fitted_enzyme_treelengths



# DEPRECATED: Scatterplot treelengths among models----------------------------------------------------------------
#plot_compare_treelengths(mammal_branch_lengths) -> mammal_treelength_figure
#plot_compare_treelengths(enzyme_branch_lengths) -> enzyme_treelength_figure
#bird_branch_lengths %>%
#  filter(treelength < 5) %>%
#  plot_compare_treelengths() -> bird_treelength_figure




#How does model+ASRV affect any of the parameters in the branchlength dataframes?---------------------------------

#note:you have to use the money sign to specify the column in the argument:

linear_model_function_with_curlies(enzyme_branch_lengths, enzyme_branch_lengths$mean_bl)
linear_model_function_with_curlies(enzyme_branch_lengths, enzyme_branch_lengths$max_bl)
linear_model_function_with_curlies(enzyme_branch_lengths, enzyme_branch_lengths$treelength)


#Now, lets plot with the plotting function------------------------------------------------------------------------
#plot_compare_function <- function(input_df, column_to_plot, plot_title, x_label, y_label)
#Note: do not have to use $ in the argument for this function...

plot_compare_function(enzyme_branch_lengths, mean_bl, "Mean bl of Other Models vs. Poisson", "Poisson bl", "Other model bl")
plot_compare_function(enzyme_branch_lengths, treelength, "Treelength of Other Models vs. Poisson", "Poisson Treelength", "Other model Treelengths")

#Now, let's use map() to allow us to run several models at once--------------------------------------------

function_for_Poisson_FLU_lm (birds)->lm_bird_output_tibble
function_for_Poisson_FLU_lm (mega_empirical_dataset)->lm_mega_output_tibble



#visualize the lm output from the map() function--------------------------


lm_mega_output_tibble%>%
  group_by(dataset,id)%>%
  ggplot(aes(x=dataset, y=r.squared))+
  geom_jitter()+
  labs(title = "Lm output for FLU~Poisson For Each Empirical Dataset", x="Dataset", y="R-Squared Value From lm")+
  theme_classic()
 
 

#Let's try to use one huge function to make a lm for any dataset for any two models---------

#Function_for_lm_for_any_dataset_and_any_two_models<- function (input_df, ASRV_T_F, model1, model2)

Function_for_lm_for_any_dataset_and_any_two_models(birds, TRUE, Poisson, WAG)




  