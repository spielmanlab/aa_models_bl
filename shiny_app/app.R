#load libraries
library(shiny)
library(tidyverse)
library(magrittr)
library(shinydashboard)
library(shinyWidgets)
library(colourpicker)
library(gt)

## Prepare data for the app, separate file for UI variables and functions --------------------------------------
source("prepare_data.R")
source("util.R")


#1. builds the ui, the web document (like the drop down menus) --------------------
#dashboardPage instead of fluidPage
#from shinydashboard
ui <- dashboardPage(
  skin = "black", #change theme color of dashboard
  dashboardHeader(title = "Shiny app"),
  #sets up the sidebar -----------------------------------------
  dashboardSidebar(
    sidebarMenu( #makes sidebar work
      width = 200, #changes width of sidebar
      #this function is for each thing that will be in the sidebar
      menuItem("Tab 1", #name in the sidebar
               tabName = "tab_01"), #this needs to be the same as tabName further down
      menuItem("Tab 2", 
               tabName = "tab_02",
               menuSubItem("Subsection 1", #adds a submenu within that menu
                           tabName = "sub_01"))
    ) #sidebarMenu() 
  ), #dashboardSidebar() 
  #where the plots, tables, etc will go -------------------------
  dashboardBody(
    #designates what should go in what tab
    tabItems(
      #Tab 1 plot and buttons
      tabItem(
        # Tab 1 --------------------------------------------------------
        tabName = "tab_01", #tab id (defined above)
        h3("Tab 1 content"), #header level, h1, h2, etc.
        fluidRow(
          #Boxes need to be put in a row (or column)
          #add boxes for each thing, order of boxes is order in app
          column(width = 3,
            #Select nucleoprotein model, sbl, dnds/entropy values -------------------
            box(numericInput(inputId = "np_model", 
                              label = "Select nucleoprotein model",
                              value = 1, #start at 1
                              min = min_np_model,
                              max = max_np_model),
                  tableOutput(outputId = "de_np_value_table"),
                  width = NULL), #top left box
            box(pickerInput(inputId = "sim_bl",
                            label = "Select simulation branch length",
                            choices = choices_sbl,
                            options = list(
                              `live-search` = TRUE)),
                width = NULL),
            #show text of bias or slope ---------------------------------------
            box(
              awesomeRadio(inputId = "tab1_bias_or_slope_button",
                           label = "Display bias or slope on the plot",
                           choices = choices_bs,
                           inline = TRUE),
                width = NULL),
            #Line of best fit --------------------------------------------------
            box(
              #title = "title?",
              awesomeRadio(inputId = "line_of_best_fit_button",
                           label = "Show line of best fit?",
                           choices = choices_line_of_best_fit,
                           inline = TRUE), #makes the buttons inline
              width = NULL, #argument needed for column() to work (needs to be in each box)
              #adds condition that color picker will show only when yes selected
              conditionalPanel(condition = "input.line_of_best_fit_button == 'Yes'", #couldn't get yes_string to work
                colourInput(inputId = "line_bf_color", 
                            label = "Select line of best fit color", 
                            value = "purple"))) #start at purple
            ), #left column()
            #tab 1 scatterplot output -----------------------------------------------
            column(width = 6,
                   plotOutput(outputId = "sim_scatter"))
          ), #fluidRow() 
        #tab 1 ic table output --------------------------------------------------
        fluidRow(box(gt_output(outputId = "tab1_AIC_table"),
                     width = 4),
                 box(gt_output(outputId = "tab1_AICc_table"),
                     width = 4),
                 box(gt_output(outputId = "tab1_BIC_table"),
                     width = 4))
      ), #tabItem() 
      #Subsection 1 table
      # tabName sub_01 ----------------------------------------------
      tabItem(tabName = "sub_01", #tab id (defined above)
              h3("Tab 2 content"), #header level, h1, h2, etc.
              fluidRow(
                column(width = 3,
                       #Select dnds/entropy (x), bias/slope (y) -------------------
                       box(pickerInput(inputId = "tab2_sub1_x_axis_select", 
                                       label = "Select x-axis",
                                       choices = choices_de),
                            pickerInput(inputId = "tab2_sub1_y_axis_select", 
                                        label = "Select y-axis",
                                        choices = choices_bs),
                           width = NULL)), #column()
                plotOutput(outputId = "de_bs_plot")
              ) #fluidRow() 
      ) #tabItem() 
    ) #tabItems() 
  ) #dashboardBody() 
) #dashboardPage() 

#2. functions to make the plot, table ----------------------------------------------------
server <- function(input, output) {

  # Tab 1 renderPlot: simulation scatterplot -------------------------------------
  #base plot
  output$sim_scatter <- renderPlot({
    make_sim_scatter(input$np_model, input$tab1_bias_or_slope_button, input$line_of_best_fit_button, input$line_bf_color)
   },
  height = 576,
  width = 800) #renderPlot()
  
  #not separated by commas
  
  #Tab 1 renderTable: dnds, entropy values of simulation scatterplot (by np model) --------------
  output$de_np_value_table <- renderTable({
    combined_data %>%
      #dnds and entropy values will change with user selected nucleoprotein model
      filter(np_sim_model == input$np_model) %>%
      select(dnds, entropy) %>%
      distinct() #value kept repeating?
  }, digits = 3 #how many decimals
  )


  #Tab 1 render_gt: AIC, ic ranking corresponding to np_sim_model -----------------------------
  output$tab1_AIC_table <- render_gt({

    combined_data %>%
      filter(ic_rank == 1,
             ic_type == "AIC", 
             np_sim_model == input$np_model,
             sim_branch_length == input$sim_bl) %>%
      #only want this value
      select(ic_weight) %>%
      #lots of decimals
      mutate(ic_weight = round(ic_weight, 2)) -> bf_model_weight
    
    fill_model_cell <- "FLU"
    #table 
    combined_data %>%
      #have to select otherwise gt shows every single column
      select(np_sim_model, sim_branch_length, model, `+G4`, ic_type, ic_rank, ic_weight) %>%
      #table changes when user changes these inputs in app
      filter(ic_type == "AIC",
             np_sim_model == input$np_model,
             sim_branch_length == input$sim_bl) %>%
      #don't want these in table
      select(-np_sim_model, -sim_branch_length, -ic_type, -ic_weight) %>%
      #model is column names
      pivot_wider(names_from = "model",
                  values_from = "ic_rank") %>%
      #make table
      gt() %>%
      tab_header(title = "AIC",
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
          columns = c(fill_model_cell), 
          rows = (c(TRUE, FALSE) == 1))
        ) %>%
      #adds a note to bottom of table
      tab_source_note(
        source_note = glue::glue("The best-fitting model weight is {bf_model_weight}"))
  })
  
  #Tab 1 render_gt: AICc, ic ranking corresponding to np_sim_model -----------------------------
  output$tab1_AICc_table <- render_gt({
    #order in parentheses needs to be same order that is defined in function!!!!!!
    make_ic_table("AICc", data_for_ic_tables, input$np_model, input$sim_bl, 
                  color_best_cell,
                  show_bf_model_wt("AICc", input$np_model, input$sim_bl))
  })
  
  #Tab 1 render_gt: BIC, ic ranking corresponding to np_sim_model -----------------------------
  output$tab1_BIC_table <- render_gt({
    make_ic_table( "BIC", data_for_ic_tables, input$np_model, input$sim_bl, 
                  color_best_cell("BICc", input$np_model, input$sim_bl),
                  show_bf_model_wt("BIC", input$np_model, input$sim_bl))
  })
  
  
  #Tab 2 Subsection 1 renderPlot() dnds/entropy (x), bias/slope (y) -------------------------
  output$de_bs_plot <- renderPlot({
    plot_de_bs_scatter(input$tab2_sub1_x_axis_select, input$tab2_sub1_y_axis_select)
  },
  height = 500,
  width = 800)
}

#3. knits ui and server together -------------------------------------------------------
shinyApp(ui = ui, server = server)
