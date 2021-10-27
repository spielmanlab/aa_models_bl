simple_function <- function(col_name){
  col_name_enquo <- enquo(col_name)
  iris %>%
    group_by(Species) %>%
    summarize(mean_value = mean(!!col_name_enquo))
}

simple_function_curly <- function(col_name){
  iris %>%
    group_by(Species) %>%
    summarize(mean_value = mean({{col_name}}))
}


simple_function(Sepal.Length)
simple_function_curly(Sepal.Length)





iris %>%
  group_by(Species) %>%
  summarize(mean_value = mean(Sepal.Length))
