#load libraries
library(shiny)
library(tidyverse)
library(magrittr)
library(shinydashboard)
library(shinyWidgets)
library(colourpicker)


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
        # "Tab 1", #more text
        tabName = "tab_01", #tab id (defined above)
        h3("Tab 1 content"), #header level, h1, h2, etc.
        #Boxes need to be put in a row (or column)
        fluidRow(
          #add boxes for each thing, order of boxes is order in app
          column(width = 3,
            box(
              #title = "title?",
              #from shinyWidgets, replaces radioButtons
              awesomeRadio(inputId = "line_of_best_fit",
                           label = "Show line of best fit?",
                           choices = choices_line_of_best_fit,
                           inline = TRUE), #makes the buttons inline
              width = NULL, #argument needed for column() to work (needs to be in each box)
              colourInput(inputId = "line_bf_color", 
                          label = "Select line of best fit color", 
                          "purple")), 
            box(numericInput(inputId = "np_model", 
                             label = "Select nucleoprotein model",
                             value = 1,
                             min = 1,
                             max = 498),
                width = NULL,
                tableOutput(outputId = "table")),
            ), #column()
          column(width = 7,
            box(plotOutput(outputId = "plot", 
                            #how long the plot is?
                            height = 300),
                width = NULL)) #column()
        ) #fluidRow() 
      ), #tabItem() 
      #Subsection 1 table
      # tabName sub_01 ----------------------
      tabItem(tabName = "sub_01", #tab id (defined above)
              h3("Tab 2 content"), #header level, h1, h2, etc.
              fluidRow(
                box(tableOutput(outputId = "filler"))
              ) #fluidRow() 
              
      ) #tabItem() 
    ) #tabItems() 
  ) #dashboardBody() 
) #dashboardPage() 

#2. functions to make the plot, table ----------------------------------------------------
server <- function(input, output) {
  
  # renderPlot: simulation scatterplot ----------------------
  output$plot <- renderPlot({
    data %>%
      filter(np_sim_model == input$np_model) %>%
      ggplot() + 
      aes(x = persite_count, 
          y = branch_length) + 
      geom_point() +
      facet_grid(cols = vars(model),
                 rows = vars(ASRV)) + 
      geom_abline(color = "red") +
      theme_bw() -> plot
    
    if (input$line_of_best_fit == yes_string) {
      
      plot <- plot + 
        geom_smooth(method = "lm", 
                    color = input$line_bf_color, 
                    size = 0.5)
    }
    plot # return the final plot
  }) #renderPlot() 
  
  #not separated by commas
  output$table <- renderTable({
    info %>%
      filter(np_sim_model == input$np_model) %>%
      select(-np_sim_model) 
  }, digits = 3
  )
  
  output$filler <- renderTable({
    filler_table
  })
}

#3. knits ui and server together --------------------------------------------------
shinyApp(ui = ui, server = server)