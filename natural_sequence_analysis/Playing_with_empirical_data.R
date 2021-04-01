# Load libraries ------------------------------------------------------------------------------------------------
library(tidyverse)
library(lme4) # for random effects linear model
library(cowplot)

source("utils.R")

# Define important variables and make important column adjustments---------------------------------------------------
conditions_per_dataset <- 10  # 5 models * 2 ASRV = 10
birds$id=as.character(birds$id)



# Load datasets --------------------------------------------------------------------------------------------------

# relative path! goal for all scripts = MEANT to run from directory where script lives 
path_to_data <- "./"


birds_file <- file.path(path_to_data, "bird_empirical_branch_lengths.csv")
mammals_file <- file.path(path_to_data, "mammal_empirical_branch_lengths.csv")
enzymes_file <- file.path(path_to_data, "enzyme_empirical_branch_lengths.csv")


mammals <- read_csv(mammals_file)
enzymes <- read_csv(enzymes_file)
birds   <- read_csv(birds_file)
mega_empirical_dataset<-bind_rows(enzymes, mammals, birds)
# Wrangle into branch lengths tibbles using functions ------------------------------------------------------------


bird_branch_lengths <- summarize_branch_lengths(birds)
mammal_branch_lengths <- summarize_branch_lengths(mammals)
enzyme_branch_lengths <- summarize_branch_lengths(enzymes)

#How does model+ASRV affect any of the parameters in the branchlength dataframes?---------------------------------


linear_model_function_curlies(enzyme_branch_lengths, enzyme_branch_lengths$mean_bl)
linear_model_function_curlies(enzyme_branch_lengths, enzyme_branch_lengths$max_bl)
linear_model_function_curlies(enzyme_branch_lengths, enzyme_branch_lengths$treelength)


#Now, lets plot with the plotting function------------------------------------------------------------------------

plot_compare_function(enzyme_branch_lengths, mean_bl, "Mean bl of Other Models vs. Poisson", "Poisson bl", "Other model bl")
plot_compare_function(enzyme_branch_lengths, treelength, "Treelength of Other Models vs. Poisson", "Poisson Treelength", "Other model Treelengths")

#Now, let's use map() to allow us to run several models at once--------------------------------------------

Poisson_FLU_lm (birds)->lm_bird_output_tibble
Poisson_FLU_lm (mega_empirical_dataset)->lm_mega_output_tibble



#visualize the lm output from the map() function--------------------------


lm_mega_output_tibble%>%
  group_by(dataset,id)%>%
  ggplot(aes(x=dataset, y=r.squared))+
  geom_jitter()+
  labs(title = "Lm output for FLU~Poisson For Each Empirical Dataset", x="Dataset", y="R-Squared Value From lm")+
  theme_classic()
 
 

#Let's try to use one huge function to make a lm of branchlength estimates for any dataset and any two models---------

#lm_two_models (birds, TRUE, "FLU", "JTT")






  