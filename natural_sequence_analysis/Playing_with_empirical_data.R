library(tidyverse)
library(readr)
birds<- read_csv("Junior_year/Research/bird_empirical_branch_lengths.csv")

mammals<- read_csv("Junior_year/Research/mammal_empirical_branch_lengths.csv")

enzymes<- read_csv("Junior_year/Research/enzyme_empirical_branch_lengths.csv")

birds%>%
  filter(id==100, model== "WAG", ASRV=="FALSE")%>%
  summarize(treelength=sum(branch_length)) #checking one individual treelength


birds%>%
  group_by(model, ASRV, id)%>%
  summarize(treelength=sum(branch_length))->bird_treelengths #show all of the individual treelengths 

bird_treelengths%>%
  group_by(model)%>%
  summarize(avg_tree_length= mean(treelength))%>%
  ggplot(aes(x=model, y=avg_tree_length, fill=model))+
  geom_col() #shows which model gives the highest avg tree length for the bird data (FLU)

mammals%>%
  group_by(model, ASRV, id)%>%
  summarize(treelength=sum(branch_length))->mammal_treelengths # all treelengths for the mammal data

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
  

enzymes%>%
  group_by(id)%>%
  count(id) #and about 500 bacterial enzyme-coding genes      (?)

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





  





  
  

  
  