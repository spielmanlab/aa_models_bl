#load libraries
library(shiny)
library(tidyverse)
library(magrittr)
library(shinydashboard)
library(shinyWidgets)
library(colourpicker)
library(gt)

## Prepare data for the app --------------------------------------
source("prepare_data.R")

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
        #Boxes need to be put in a row (or column)
        fluidRow(
          #add boxes for each thing, order of boxes is order in app
          column(width = 3,
            #Select nucleoprotein model, dnds/entropy values -------------------
            box(numericInput(inputId = "np_model", 
                              label = "Select nucleoprotein model",
                              value = 1, #start at 1
                              min = min_np_model,
                              max = max_np_model),
                  width = NULL,
                  tableOutput(outputId = "de_np_value_table")),
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
            ), #column()
          column(width = 7,
                 plotOutput(outputId = "sim_scatter")) #column()
        ) #fluidRow() 
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
                box(plotOutput(outputId = "de_bs_plot"))
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
    sbl_de_bs_data %>%
      filter(np_sim_model == input$np_model) %>%
      mutate(bias = round(bias, 3), 
             slope_when_yint0 = round(slope_when_yint0, 3)) -> data_to_plot
    
    data_to_plot %>%
      filter(sim_branch_length == 0.01) -> data_to_label # this way labels aren't stacked 30+ times on top of each other
    
    # Convert string input$bias_or_slope to symbol so can use it in {{}}
    label_column_symbol <- as.symbol(input$tab1_bias_or_slope_button)
    
    
    ggplot(data_to_plot) + 
      aes(x = persite_count, 
          y = branch_length) + 
      geom_point() +
      facet_grid(cols = vars(model),
                 rows = vars(ASRV)) + 
      geom_abline(color = "red") +
      #this is not working (with only seeing bias and slope_when_yint0)
      geom_text(data = data_to_label,
                #couldn't get to work :|
                #input needs to be string (bias or slope) so you can pick from the 2 buttons in the app
                #label needs the actual data/numbers
                #if feed in actual data (ie. selecting the columns and saving to variable), Shiny thinks datapoints are options in app
               aes(label = {{label_column_symbol}}), # ALERT NEEDS TO BE INPUT$SOMETHINGOROTHER
               y = Inf,
               x = -Inf,
               hjust = -0.25, vjust = 2) +
      theme_bw() -> sim_plot
    
    #add line of best fit
    if (input$line_of_best_fit_button == yes_string) {
      
      sim_plot <- sim_plot + 
        geom_smooth(method = "lm", 
                    color = input$line_bf_color, 
                    size = 0.5)
      } #if
    sim_plot # return the final plot
    },
  height = 500,
  width = 800) #renderPlot()
  
  #not separated by commas
  
  #Tab 1 renderTable: dnds, entropy values of simulation scatterplot (by np model) --------------
  output$de_np_value_table <- renderTable({
    sbl_de_bs_data %>%
      filter(np_sim_model == input$np_model) %>%
      select(dnds, entropy) %>%
      distinct() #value kept repeating?
  }, digits = 3 #how many decimals
  )
  
  #Tab 2 Subsection 1 renderPlot() dnds/entropy (x), bias/slope (y) -------------------------
  output$de_bs_plot <- renderPlot({
    de_bs_plot_function(input$tab2_sub1_x_axis_select, input$tab2_sub1_y_axis_select)
  }) #aesthetics not updating???????
}

#3. knits ui and server together -------------------------------------------------------
shinyApp(ui = ui, server = server)
