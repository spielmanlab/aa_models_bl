library(corrr)
library(tidyverse)
path_to_data <- "./"

birds_file <- file.path(path_to_data, "bird_empirical_branch_lengths.csv")
mammals_file <- file.path(path_to_data, "mammal_empirical_branch_lengths.csv")
enzymes_file <- file.path(path_to_data, "enzyme_empirical_branch_lengths.csv")


birds   <- read_csv(birds_file) %>% mutate(dataset =  "birds", id = as.character(id)) #might as well mutate here so we can bind rows later 
mammals <- read_csv(mammals_file) %>% mutate(dataset =  "mammals", id = as.character(id))
enzymes <- read_csv(enzymes_file) %>% mutate(dataset =  "enzymes", id = as.character(id))

group_by(iris,Species) %>%
  nest() %>%
  mutate(cordf = map(data, correlate)) %>%
  unnest(cordf)


#bind_rows(birds, mammals, enzymes) %>%
  
  
enzymes %>%
  unite(combined_model, model, ASRV) %>%
  group_by(id, dataset) %>%
  pivot_wider(names_from = combined_model, 
              values_from = branch_length) %>%
  ungroup() -> input_data

input_data %>%
  select(-node) %>%
  group_by(id, dataset) %>%
  nest() %>%
  mutate(corrrr = map(data, correlate)) %>%
  select(-data) %>%
  unnest(cols = c(corrrr)) %>%
  pivot_longer(contains("_"), 
               names_to = "term2", 
               values_to = "pearson") %>%
  ggplot(aes(x = term, y = pearson)) + 
    geom_boxplot() + 
    facet_wrap(~term2)

all_models <- names(input_data)[stringr::str_detect(names(input_data), "_")]


n_models <- length(all_models)


for (model1_index in 1:n_models) {
  for (model2_index in model1_index:n_models) {
    if (model1_index == model2_index) next
    m1 <- all_models[model1_index]
    m2 <- all_models[model2_index]
    # DO THE THINGS IN HERE:
    ## group by id and dataset, select out node column, 
    print(paste(m1,m2))
  }
}


tibble(x = numeric()) -> my_tibble
# in real world, you'd build something up with columns...
## id
## dataset
## term1
## term2
## whatever quantity you grabbed from the lm. STARTING WITH INTERCEPT

for (i in 1:10)
{
  new_tibble <- tibble(x = i) # analogous to getting the lm stuff with broom
  my_tibble <- bind_rows(my_tibble, new_tibble)
}
my_tibble