iris %>%
  ggplot() + 
  aes(x = Sepal.Length, y = Petal.Length) +
  geom_point()

# imagine i want a shiny app with DROP DOWNS for "which variables should i plot?"

# one way: aes_string()

###### Functions ------------------------

# mean(iris$Sepal.Length, na.rm = TRUE) 
# ^^: 
# iris$Sepal.Length is a POSITIONAL argument. mean() knows what to do with that info because it was provided FIRST
# na.rm=TRUE is a KEYWORD argument where keywork is na.rm


add_two_numbers <- function(x, y) {
  # Whatever is on the LAST LINE of the function gets RETURNED
  x + y
}

add_two_numbers(13, 4)


add_two_numbers2 <- function(x = 1, y = 2) {
  # Whatever is on the LAST LINE of the function gets RETURNED
  x + y
}

a <- 12
b <- 14
add_two_numbers2()

add_two_numbers2(1,7) # overrides defaults  ---> 8
add_two_numbers2(x = 1, y = 7) # overrides defaults ---> 8
add_two_numbers2(x = 1, y = "7") # BUG


###############################################################



iris %>%
  ggplot() + 
  aes_string(x = "Sepal.Length", y = "Petal.Length") +
  geom_point()

f1_string <- function(x_variable, y_variable) {
  iris %>%
    ggplot() + 
    aes_string(x = x_variable, y = y_variable) +
    geom_point()
}


f1_string("Sepal.Length", "Petal.Width" ) # YES QUOTES




f2_fancy <- function(x_variable, y_variable) {
  iris %>%
    ggplot() + 
    aes(x = {{x_variable}}, y = {{y_variable}}) +  # curly-curly
    geom_point()
}


f2_fancy(Sepal.Length, Petal.Width) # NO QUOTES



f3_filter <- function(keep_species) {
  iris %>%
    filter(Species == keep_species) %>%
    ggplot() + 
    aes(x = Sepal.Length, y = Petal.Length) +
    geom_point()
}

f3_filter("setosa")

