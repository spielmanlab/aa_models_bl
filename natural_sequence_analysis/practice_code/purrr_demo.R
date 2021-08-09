library(tidyverse)


birds %>% 
  filter(ASRV == TRUE) %>% 
  select(-ASRV, -dataset) %>% 
  group_by(id) %>% 
  pivot_wider(names_from = model, values_from = branch_length) -> asrv_true_birds_wide



##########################

library(palmerpenguins)
penguins

# for each species, I want a regression bill length/depth
model_me_please <- function(df)
{
  lm(bill_depth_mm ~ bill_length_mm, data = df)
}

penguins %>%
  dplyr::group_by(species)  %>%
  tidyr::nest() %>%
  dplyr::mutate( fit = purrr::map(data, model_me_please)  ) %>%
  select(-data) %>%
  dplyr::mutate(tidied = purrr::map(fit, broom::tidy),
                glanced = purrr::map(fit, broom::glance)) %>%
  dplyr::select(-fit) %>%
  tidyr::unnest(glanced) %>%
  dplyr::select(species, tidied, r.squared, rsq.pvalue = p.value) %>%
  tidyr::unnest(tidied)


###########################################
purrr::map_dbl(1:10, sqrt)
sqrt(1:10)
5 %>% sqrt()












