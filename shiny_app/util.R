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

#tab 1 table function, ic rank/weight --------------------------------------
#prep_ic_table data function -------------------------------
prep_ic_table <-  function() { #argument?
  combined_data %>%
    #have to select otherwise gt shows every single column
    select(np_sim_model, sim_branch_length, model, `+G4`, ic_type, ic_rank) %>%
    #table changes when user changes these inputs in app
    filter(np_sim_model == input$np_model,
           sim_branch_length == input$sim_bl) %>%
    #don't want to filter so that function works
    group_by(ic_type) %>%
    #don't want these in table
    select(-np_sim_model, -sim_branch_length, -ic_type) %>%
    #model is column names
    pivot_wider(names_from = "model",
                values_from = "ic_rank")
} 

#make_ic_table function-----------------------------------------------
make_ic_table <- function(prep_ic_table, pick_ic_type) {
  combined_data %>%
    gt() %>%
    tab_header(title = pick_ic_type) %>% 
    #like a title above these specific columns
    tab_spanner(label = "Model",
                columns = c(FLU, LG, JTT, WAG, Poisson)) %>%
    #Poisson wider than other cells so this makes model col width the same
    cols_width(c(FLU, LG, JTT, WAG, Poisson) ~ px(60)) #%>%
  #color cells according to ic_weight
  #data_color(data = combined_data$ic_weight,
  #          columns = c(FLU, LG, JTT, WAG, Poisson),
  #         colors = scales::col_numeric(
  #          palette = c("blue", "white")),
  #       domain = c(0, 1)) #column scale endpoints
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
