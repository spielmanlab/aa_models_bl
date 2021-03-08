# Load libraries -------------------------------------
library(tidyverse)
library(lme4) # for random effects linear model
library(cowplot)

source("utils.R")

# Define important variables ----------------------------
conditions_per_dataset <- 10  # 5 models * 2 ASRV = 10


# Load datasets -------------------------------------

# relative path! goal for all scripts = MEANT to run from directory where script lives 
path_to_data <- "./"


birds_file <- file.path(path_to_data, "bird_empirical_branch_lengths.csv")
mammals_file <- file.path(path_to_data, "mammal_empirical_branch_lengths.csv")
enzymes_file <- file.path(path_to_data, "enzyme_empirical_branch_lengths.csv")


mammals <- read_csv(mammals_file)
enzymes <- read_csv(enzymes_file)
birds   <- read_csv(birds_file)

# Wrangle into branch lengths tibbles using functions ------------------------------


bird_branch_lengths <- summarize_branch_lengths(birds)
mammal_branch_lengths <- summarize_branch_lengths(mammals)
enzyme_branch_lengths <- summarize_branch_lengths(enzymes)



# Begin exploring (no specific goal for this section) -----------------------------------------------------------------
birds%>% 
  filter(id==100, model== "WAG", ASRV=="FALSE")%>%
  summarize(treelength=sum(branch_length)) #checking one individual treelength

#### Practice Assertions 
bird_branch_lengths %>% 
  count(id) %>% 
  filter(n != conditions_per_dataset) %>%
  nrow() -> number_bad
stopifnot( number_bad == 0 ) # if this isn't true, STOP!!!
# inside parentheses put what SHOULD BE TRUE


mammals %>%
  group_by(model, ASRV, id)%>%
  summarize(treelength=sum(branch_length)) %>%
  ungroup() %>%
  ggplot(aes(x=model, y=treelength, fill=model))+ 
    geom_violin() + stat_summary() + 
    facet_wrap(vars(ASRV), nrow=2) #quick plot to visualize treelengths of mammal data across models with +G/-G


mammals%>%
  group_by(id)%>%
  count(id) #looks like there are about 10,000 unique genes in the mammals data


birds%>%
  group_by(id)%>%
  count(id)%>%
  arrange(id)#and over 11,000 bird genes

enzymes%>%
  group_by(id)%>%
  count(id) #and about 500 bacterial enzyme-coding genes    

birds%>%
  group_by(ASRV)%>%
  summarize(avg_branch_length=mean(branch_length)) #as expected, when gamma is present, the branch length is higher because there can be different evolutionary rates between sites in models that include gamma


enzymes%>%
  group_by(id, model, ASRV)%>%
  summarize(treelength=sum(branch_length))%>%
  filter(id=="13PK_A", ASRV=="TRUE")%>%
  ggplot(aes(x=model, y=treelength))+
  geom_col()+
  theme_classic() #visualizing the different model outputs with G for each model for a specific enzyme

#########################################################################################################













#  How does model+ASRV affect treelength? ----------------------------------------------------------------
fit_treelength_model_ASRV(mammal_branch_lengths) -> fitted_mammals_treelengths
fit_treelength_model_ASRV(bird_branch_lengths)   -> fitted_bird_treelengths
fit_treelength_model_ASRV(enzyme_branch_lengths) -> fitted_enzyme_treelengths



# Scatterplot treelengths among models-----------------------------------
plot_compare_treelengths(mammal_branch_lengths) -> mammal_treelength_figure
plot_compare_treelengths(enzyme_branch_lengths) -> enzyme_treelength_figure

bird_branch_lengths %>%
  filter(treelength < 5) %>%
  plot_compare_treelengths() -> bird_treelength_figure


  