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
            #Line of best fit --------------------------------------------------
            box(
              #title = "title?",
              #from shinyWidgets, replaces radioButtons
              awesomeRadio(inputId = "line_of_best_fit",
                           label = "Show line of best fit?",
                           choices = choices_line_of_best_fit,
                           inline = TRUE), #makes the buttons inline
              width = NULL, #argument needed for column() to work (needs to be in each box)
              #adds condition that color picker will show only when yes selected
              conditionalPanel(condition = "input.line_of_best_fit == 'Yes'", #couldn't get yes_string to work
                colourInput(inputId = "line_bf_color", 
                            label = "Select line of best fit color", 
                            value = "purple"))) #start at purple
            ), #column()
          column(width = 7,
            box(plotOutput(outputId = "sim_scatter", 
                            #how long the plot is?
                            height = 300),
                width = NULL)) #column()
        ) #fluidRow() 
      ), #tabItem() 
      #Subsection 1 table
      # tabName sub_01 ----------------------------------------------
      tabItem(tabName = "sub_01", #tab id (defined above)
              h3("Tab 2 content"), #header level, h1, h2, etc.
              fluidRow(
                column(width = 3,
                       #Select dnds/entropy (x), bias/slope (y) -------------------
                       box(pickerInput(inputId = "dnds_entropy", 
                                       label = "Select x-axis",
                                       choices = choices_de),
                            pickerInput(inputId = "bias_slope", 
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
  
  # renderPlot: simulation scatterplot ----------------------
  #base plot
  output$sim_scatter <- renderPlot({
    sbl_de_bs_data %>%
      filter(np_sim_model == input$np_model) %>%
      ggplot() + 
      aes(x = persite_count, 
          y = branch_length) + 
      geom_point() +
      facet_grid(cols = vars(model),
                 rows = vars(ASRV)) + 
      geom_abline(color = "red") +
      #this is not working (with only seeing bias and slope_when_yint0)
      annotate("text", #what should be annotated onto plot (text output)
               label = input$np_model, #is there a way for it to be attached to variables?
               y = 2.75,
               x = 0.25) +
      theme_bw() -> sim_plot
    
    #add line of best fit
    if (input$line_of_best_fit == yes_string) {
      
      sim_plot <- sim_plot + 
        geom_smooth(method = "lm", 
                    color = input$line_bf_color, 
                    size = 0.5)
    }
    sim_plot # return the final plot
  }) #renderPlot() 
  
  #not separated by commas
  
  #renderTable: dnds, entropy values of simulation scatterplot (by np model) --------
  output$de_np_value_table <- renderTable({
    sbl_de_bs_data %>%
      filter(np_sim_model == input$np_model) %>%
      select(dnds, entropy) %>%
      distinct() #value kept repeating?
  }, digits = 3 #how many decimals
  )
  
  #renderPlot() dnds/entropy (x), bias/slope (y)
  output$de_bs_plot <- renderPlot({
    de_bs_function <- function(x_axis, y_axis) {
    sbl_de_bs_data %>%
      ggplot() +
      aes(x = {{x_axis}}, 
          y = {{y_axis}}) +
      geom_point() +
      facet_grid(cols = vars(model),
                  rows = vars(ASRV)) 
    } #function
    #not working :(
    de_bs_function(input$choices_de, input$choices_bs)
    })
}

#3. knits ui and server together --------------------------------------------------
shinyApp(ui = ui, server = server)
