library(shiny)
library(tidyverse)

# Define UI for application that draws a scatterplot
ui <- fluidPage(
    

    # Application title
    titlePanel("Site to Plot"),

    # Sidebar with a text input to simulate a site
    sidebarLayout(
        sidebarPanel(
            numericInput("site", "Site:", value = 1, min = 1, max = 498)
                      
        ),

        # Show a plot of the generated final tibble
        mainPanel(
           plotOutput("bl_plot")
        )
    )
)

# Define server logic required to draw a scatterplot
server <- function(input, output) {

    output$bl_plot <- renderPlot(height = 900, width = 1250,{
       
        # define the dataframe
        #add modeled slope bias from results
        #make the app look nicer
        #add slopes to each plot x=1 and y=1
        #push all of this over to git
        #npsite focus on entropy and print out under each plot the entropy use pull entropy and text output
        final_tibble %>%
        dplyr::filter(site == input$site) %>% 
        ggplot(aes(x = persite_count, y = branch_length, color = model)) +
            geom_point()  + 
            geom_smooth(method = "lm") +  #, se = FALSE) +
            facet_grid(ASRV~model) +#, scales="free") + 
            #facet_wrap(model~ASRV, nrow=5) + 
            geom_abline(color = "red") + 
            labs(title = "Count vs Branch Length", x = "Persite Count", y = "Branch Length") +
            theme_bw(base_size = 15) +
            theme(plot.background = element_rect(fill = "gray")) +
            
            theme(legend.position = "none") 
    })s
}

# Run the application 
shinyApp(ui = ui, server = server)
