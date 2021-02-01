library(tidyverse)


result_path <- "../results/"

mammal <- read_csv(file.path(result_path, "empirical_treelengths_mammal.csv")) %>% mutate(dataset =  "mammal")
enzyme <- read_csv(file.path(result_path, "empirical_treelengths_enzyme.csv")) %>% mutate(dataset =  "enzyme")
bird <- read_csv(file.path(result_path, "empirical_treelengths_bird.csv"))  %>% mutate(dataset =  "bird", name = as.character(name))

bind_rows(mammal, enzyme, bird) %>%
  mutate(model = str_replace(model, "\\+F", ""),
         ASRV  = if_else(str_detect(model,"\\+G"), TRUE, FALSE),
         model = str_replace(model, "\\+G", "")) %>% 
  write_csv(file.path(result_path, "empirical_treelengths.csv"))

