#load libraries
library(shiny)
library(tidyverse)
library(shinydashboard)
library(shinyWidgets)

#file path to data
path_to_data <- file.path(here::here(), "results", "simulation_branch_lengths_counts.csv")

#read in data
#avoid getting confused with poor use of "site" term all over the place
data <- read_csv(path_to_data) %>% rename(np_sim_model = site)

#will write together
np_model_function <- c(1, 10, 100, 104)

filler_table <- c("hello", "shiny", "world", "!!!!!!!!!!!!!")

#1. builds the ui, the web document (like the drop down menus) --------------------
#dashboardPage instead of fluidPage
#from shinydashboard
ui <- dashboardPage(
  skin = "black", #change theme color of dashboard
  dashboardHeader(title = "Shiny app"),
  #sets up the sidebar -----------------------------------------
  dashboardSidebar(
    #this function is for each thing that will be in the sidebar
    menuItem("Tab 1", #name in the sidebar
             tabName = "tab_01"), #this needs to be the same as tabName further down
    menuItem("Tab 2", 
             tabName = "tab_02",
             menuSubItem("Subsection 1", #adds a submenu within that menu
                         tabName = "sub_01"))
  ), #dashboardSidebar() closes
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
          #add boxes for each thing
          box(plotOutput("plot", 
                         #how long the plot is?
                         height = 300)),
          box(
            #title = "title?",
            #from shinyWidgets, replaces radioButtons
            awesomeRadio(inputId = "np_model",
                         label = "Select nucleoprotein model",
                         choices = np_model_function,
                         inline = TRUE)) #makes the buttons inline
        ) #fluidRow() closes
      ), #tabItem() closes
      #Subsection 1 table
      tabItem(tabName = "sub_01", #tab id (defined above)
              h3("Tab 2 content"), #header level, h1, h2, etc.
              fluidRow(
                box(tableOutput("table"))) #fluidRow() closes
              
      ) #tabItem() closes
    ) #tabItems() closes
  ) #dashboardBody() closes
) #dashboardPage() closes

#2. functions to make the plot, table ----------------------------------------------------
server <- function(input, output) {
  output$plot <- renderPlot(
    data %>%
      filter(np_sim_model == input$np_model) %>%
      ggplot() + 
      aes(x = persite_count, 
          y = branch_length) + 
      geom_point() +
      facet_grid(cols = vars(model),
                 rows = vars(ASRV)) + 
      geom_abline(color = "red")  + # equality
      geom_smooth(method = "lm", color = "blue", size = 0.5)
  ) #renderPlot() closes
  
  output$table <- renderTable(
    filler_table
  )
}

#3. knits ui and server together --------------------------------------------------
shinyApp(ui = ui, server = server)
