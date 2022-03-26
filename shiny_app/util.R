#UI variables and functions file


# UI Variables ----------------------------------
min_np_model <- 1
max_np_model <- 498
choices_sbl <- c(0.01, 0.05, seq(from = 0.1, to = 3, by = 0.1))
yes_string <- "Yes"
no_string <- "No"
choices_line_of_best_fit <- c(yes_string, no_string)
choices_de <-  c("dnds", "entropy")
choices_bs <-  c("bias", "slope_when_yint0")

# Functions ----------------------------------------------
de_bs_plot_function <- function(x_axis, y_axis) {
  x_axis <- as.symbol(x_axis)
  y_axis <- as.symbol(y_axis)
  combined_data %>%
    ggplot() +
    aes(x = {{x_axis}}, 
        y = {{y_axis}}) +
    geom_hex(bins = 25) + #because SO many points
    scale_fill_viridis_c(option = 'viridis') +
    facet_grid(cols = vars(model),
               rows = vars(ASRV),
               scales = "free_y")
} 
