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

# Functions ------------------------------------------------------------------

#tab 1 make_sim_scatter function -------------------------------------------
make_sim_scatter <- function(np_model,
                             bias_or_slope,
                             line_of_bf,
                             color_line_of_bf) {
  combined_data %>%
    filter(np_sim_model == np_model) %>%
    mutate(bias = round(bias, 3), 
           slope_when_yint0 = round(slope_when_yint0, 3)) -> data_to_plot
  
  data_to_plot %>%
    filter(sim_branch_length == 0.01) -> data_to_label # this way labels aren't stacked 30+ times on top of each other
  
  # Convert string input$bias_or_slope to symbol so can use it in {{}}
  label_column_symbol <- as.symbol(bias_or_slope)
  
  ggplot(data_to_plot) + 
    aes(x = persite_count, 
        y = branch_length) + 
    #actual scatterplot 
    geom_point() +
    facet_grid(cols = vars(model),
               rows = vars(`+G4`)) + 
    geom_abline(color = "red") +
    #bias or slope text displayed on graph
    geom_text(data = data_to_label,
              aes(label = {{label_column_symbol}}), 
              y = Inf, 
              x = -Inf,
              hjust = -0.25, vjust = 2) + #so that values display (axes change according to np_model)
    theme_bw() -> sim_plot
  
  #add line of best fit
  if (line_of_bf == yes_string) {
    
    sim_plot <- sim_plot + 
      geom_smooth(method = "lm", 
                  color = color_line_of_bf, 
                  size = 0.5)
  } #if
  sim_plot # return the final plot
}


#tab 1 show_bf_model_wt function for ic table function -------------------------
show_bf_model_wt <- function(pick_ic_type,
                             np_model,
                             sim_bl) {
  combined_data %>%
    filter(ic_rank == 1,
           ic_type == pick_ic_type,
           np_sim_model == np_model,
           sim_branch_length == sim_bl) %>%
    #only want this value
    select(ic_weight) %>%
    #lots of decimals
    mutate(ic_weight = round(ic_weight, 2))
}

#tab 1 color_best_cell function for ic table function -------------------------
find_column_to_color <- function(pick_ic_type,
                                 data_for_ic_tables, 
                                 np_model, 
                                 sim_bl) {
  #make string of rank 1 model, preps for function
  data_for_ic_tables %>%
    # Filter to only relevant table parts
    filter(np_sim_model == np_model,
           sim_branch_length == sim_bl, 
           ic_type == pick_ic_type) %>%
    select(FLU:last_col()) %>%
    pivot_longer(everything(), 
                 names_to = "model",
                 values_to = "rank") %>%
    filter(rank == 1) %>%
    #makes column into vector
    pull(model) -> rank1_model_string
}

#tab 1 make_ic_table function-----------------------------------------------
make_ic_table <- function(pick_ic_type,
                          data_for_ic_tables, 
                          np_model, 
                          sim_bl,
                          rank1_model_string,
                          show_bf_model_wt) {
  data_for_ic_tables %>%
    # Filter to only relevant table parts
    filter(np_sim_model == np_model,
           sim_branch_length == sim_bl, 
           ic_type == pick_ic_type) %>%
    # Remove columns we don't want in the table
    select(-np_sim_model, -sim_branch_length, -ic_type) %>%
    gt() %>%
    tab_header(title = pick_ic_type,
               subtitle = "subtitle?") %>% 
    #like a title above these specific columns
    tab_spanner(label = "Model",
                columns = c(FLU, LG, JTT, WAG, Poisson)) %>%
    #Poisson wider than other cells so this makes model col width the same
    cols_width(c(FLU, LG, JTT, WAG, Poisson) ~ px(60)) %>%
    #color cell best fitting model
    tab_style(
      style = cell_fill(color = "lightblue"), #what to color
      locations = cells_body( #where the color should show up
        columns = rank1_model_string, 
        rows = c("No", "Yes") == 1
      )) %>%
    #adds a note to bottom of table
    tab_source_note(
      source_note = glue::glue("The best-fitting model weight is {show_bf_model_wt}"))
}


#tab 2 plot function, dnds/entropy (x), bias/slope (y) --------------------------
plot_de_bs_scatter <- function(x_axis, y_axis) {
  x_axis <- as.symbol(x_axis)
  y_axis <- as.symbol(y_axis)
  combined_data %>%
    ggplot() +
    aes(x = {{x_axis}}, 
        y = {{y_axis}}) +
    geom_hex(bins = 25) + #because SO many points
    scale_fill_viridis_c(option = 'viridis') +
    facet_grid(cols = vars(model),
               rows = vars(`+G4`),
               scales = "free_y")
} 
