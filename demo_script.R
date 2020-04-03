fits <- read_csv("../results/parsed_aic.csv") ### BAD FILE STAY TUNED!!

fits %>%
  ### gets rid of +F from model
  mutate(model = str_replace(model, "\\+F", "")) %>%
  ## create new column as TRUE if +G is in the model anf FALSE otherwsie
  mutate(ASRV = if_else( str_detect(model, "\\+G"), TRUE, FALSE)) %>%
  ## get rid ofthe +G we have it now in the new column
  mutate(model = str_replace(model, "\\+G", "")) %>%
  rename(sim_branch_length = bl) -> fits_clean
  print(fits_clean)
fits_clean %>% 
  dplyr::select(-BIC, -AICc) %>%
  group_by(site, model, ASRV) %>%
  mutate(aicrank = rank(AIC)) %>%
  filter(aicrank == 1) %>%
  ungroup() %>%
  group_by(site, ASRV, model) %>%
  tally()
  
write_csv(fits_clean, "../results/clean_parsed_aic.csv")

