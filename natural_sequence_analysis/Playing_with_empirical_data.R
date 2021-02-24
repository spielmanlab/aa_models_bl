# Load libraries ----------------------------
library(tidyverse)
library(lme4) # random effects lm
# Define important variables ----------------------------
conditions_per_dataset <- 10  # 5 models * 2 ASRV = 10


# Load datasets -----------------------------
path_to_data <- "/Users/jakemihalecz/Junior_year/Research/aa_models_bl/natural_sequence_analysis"

birds_file <- file.path(path_to_data, "bird_empirical_branch_lengths.csv")
mammals_file <- file.path(path_to_data, "mammal_empirical_branch_lengths.csv")
enzymes_file <- file.path(path_to_data, "enzyme_empirical_branch_lengths.csv")


# optional strategy for saving time/sanity
#if (!(exists("birds"))) {
#  birds<- read_csv(birds_file)
#} 

mammals<- read_csv(mammals_file)
enzymes<- read_csv(enzymes_file)
birds<- read_csv(birds_file)
# Begin exploring -----------------------------------------
birds%>%
  filter(id==100, model== "WAG", ASRV=="FALSE")%>%
  summarize(treelength=sum(branch_length)) #checking one individual treelength


birds%>%
  group_by(model, ASRV, id)%>%
  summarize(treelength=sum(branch_length)) %>%
  ungroup() -> bird_treelengths #show all of the individual treelengths 

#### Assertions 
bird_treelengths %>% 
  count(id) %>% 
  filter(n != conditions_per_dataset) %>%
  nrow() -> number_bad
stopifnot( number_bad == 0 ) # if this isn't true, STOP!!!
# inside parentheses put what SHOULD BE TRUE

bird_treelengths%>%
  group_by(model, ASRV)%>%
  summarize(avg_tree_length= mean(treelength))%>%
  ggplot(aes(x=model, y=avg_tree_length, fill=model))+
  geom_col() #shows which model gives the highest avg tree length for the bird data (FLU)

mammals %>%
  group_by(model, ASRV, id)%>%
  summarize(treelength=sum(branch_length)) %>%
  ungroup() %>%
  ggplot(aes(x=model, y=treelength, fill=model))+ 
    geom_violin() + stat_summary() + 
    facet_wrap(vars(ASRV), nrow=2)


# similar to above but with something more numeric-y
bird_treelengths%>%
  ggplot(aes(x=model, y=treelength, color=model))+ 
    geom_jitter(width = 0.2) + 
    facet_wrap(vars(ASRV), nrow=2)


mammals%>%
  group_by(model, ASRV, id)%>%
  summarize(treelength=sum(branch_length)) %>%
  ungroup() ->mammal_treelengths # all treelengths for the mammal data

mammal_treelengths%>%
  group_by(model)%>%
  summarize(avg_tree_length= mean(treelength))%>%
  ggplot(aes(x=model, y=avg_tree_length, fill=model))+
  geom_col() #FLU once again gives highest avg treelength amongst the models!


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

birds%>%
  filter(id==10004, model=="FLU", ASRV=="FALSE")->single_bird_tree

  
  single_bird_tree%>%
    summarize(tree_length=sum(branch_length)) #find the treelength of a single tree

birds%>%
  group_by(id)%>%
 summarize(sum_of_all_branchlengths=sum(branch_length))%>%
  arrange(desc(sum_of_all_branchlengths)) #this (I think)is a very crude way to show which genes are more conserved than others where larger branch lengths suggest higher tolerance to change. Interesting that some literally come out to be 0

birds%>%
  group_by(id, ASRV)%>%
  summarize(avg_branch_length=mean(branch_length)) #just a quick way to view the differences between +G and -G for each gene reguardless of model.
  

enzymes%>%
  group_by(id, model, ASRV)%>%
  summarize(treelength=sum(branch_length))%>%
  filter(id=="13PK_A", ASRV=="TRUE")%>%
  ggplot(aes(x=model, y=treelength))+
  geom_col()+
  theme_classic() #visualizing the different model outputs with G for each model for a specific enzyme


  
#modeling------------------------
# show the wrangling again
birds%>%
  group_by(model, ASRV, id)%>%
  summarize(treelength=sum(branch_length)) %>%
  ungroup() -> bird_treelengths


mammals%>%
  group_by(model, ASRV, id)%>%
  summarize(treelength=sum(branch_length)) %>%
  ungroup() ->mammal_treelengths

enzymes%>%
  group_by(model, ASRV, id)%>%
  summarize(treelength=sum(branch_length)) %>%
  ungroup() ->enzyme_treelengths







# Mammals modeling-----------------------------------------

lm(treelength ~ model + ASRV, data = mammal_treelengths) %>% 
  aov() %>%
  TukeyHSD()


#now try to get the ASRV in the Tukey

mammal_treelengths%>%
  mutate(ASRV_modified= if_else(ASRV=="TRUE", "Yes", "No"))-> mammal_treelengths_ASRV_modified


lm(treelength ~ model + ASRV_modified, data = mammal_treelengths_ASRV_modified) %>% 
  aov() %>%
  TukeyHSD()


#now with random effects

lme4::lmer(treelength ~ model + ASRV + (1|id), data = mammal_treelengths) 





# birds modeling--------------------------------------------


lm(treelength ~ model + ASRV, data = bird_treelengths) %>% 
  aov() %>%
  TukeyHSD()



#Now with ASRV
bird_treelengths%>%
  mutate(ASRV_modified= if_else(ASRV=="TRUE", "Yes", "No"))-> bird_treelengths_ASRV_modified


lm(treelength ~ model + ASRV_modified, data = bird_treelengths_ASRV_modified) %>% 
  aov() %>%
  TukeyHSD()


#now with random effects

lme4::lmer(treelength ~ model + ASRV + (1|id), data = bird_treelengths) 


#enzymes modeling-------------------------------------

lm(treelength ~ model + ASRV, data = enzyme_treelengths) %>% 
  aov() %>%
  TukeyHSD()


#now with ASRV
enzyme_treelengths%>%
  mutate(ASRV_modified= if_else(ASRV=="TRUE", "Yes", "No"))-> enzyme_treelengths_ASRV_modified

lm(treelength ~ model + ASRV_modified, data = enzyme_treelengths_ASRV_modified) %>% 
  aov() %>%
  TukeyHSD()


#now with random effects
lme4::lmer(treelength ~ model + ASRV + (1|id), data = enzyme_treelengths) 

# mess around with making some figures----------------------------------------------------------------------------

mammal_treelengths%>%
  pivot_wider(names_from = model, values_from = treelength)%>%
  pivot_longer(cols=c("FLU", "JTT" , "Poisson", "WAG"), 
               names_to = "other_models", 
               values_to = "treelength")->mammal_treelengths_LG_pivot 

#this wrangling will allow for a direct comparison of LG treelengths vs all other models!!!

ggplot((mammal_treelengths_LG_pivot), aes(x=LG, y=treelength))+
  facet_wrap(vars(other_models))+
  geom_point()+
  theme_classic()+
  geom_smooth(method = "lm")+
  theme(strip.background =element_rect(fill="cornflowerblue"))+
  labs(title= "LG vs. Other model's Mammal Treelengths", x="Treelength obtained from LG", y="Treelength obtained from other models")

# now do the exact same thing for the other models!!!

bird_treelengths%>%
  pivot_wider(names_from = model, values_from = treelength)%>%
  pivot_longer(cols=c("FLU", "JTT" , "Poisson", "WAG"), 
               names_to = "other_models", 
               values_to = "treelength")->bird_treelengths_LG_pivot 

ggplot((bird_treelengths_LG_pivot), aes(x=LG, y=treelength))+
  facet_wrap(vars(other_models))+
  geom_point()+
  theme_classic()+
  geom_smooth(method = "lm")+
  theme(strip.background =element_rect(fill="cornflowerblue"))+
  labs(title= "LG vs. Other model's Bird Treelengths", x="Treelength obtained from LG", y="Treelength obtained from other models")




enzyme_treelengths%>%
  pivot_wider(names_from = model, values_from = treelength)%>%
  pivot_longer(cols=c("FLU", "JTT" , "Poisson", "WAG"), 
               names_to = "other_models", 
               values_to = "treelength")->enzyme_treelengths_LG_pivot 


ggplot((enzyme_treelengths_LG_pivot), aes(x=LG, y=treelength))+
  facet_wrap(vars(other_models))+
  geom_point()+
  theme_classic()+
  geom_smooth(method = "lm")+
  theme(strip.background =element_rect(fill="cornflowerblue"))+
  labs(title= "LG vs. Other model's Enzyme Treelengths", x="Treelength obtained from LG", y="Treelength obtained from other models")



  
  

  
  