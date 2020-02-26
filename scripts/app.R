library(shiny)
library(tidyverse)
library(shinythemes)



input_file <- "../results/final_tibble.csv"
final_tibble <- read_csv(input_file)
print(final_tibble)
#entropy_value <- pull(final_tibble, entropy)
#print(entropy_value)
# Define UI for application that draws a scatterplot
ui <- fluidPage(theme =shinytheme("darkly"),
    
   
    # Application title
    titlePanel("Site to Plot"),

    # Sidebar with a text input to simulate a site
    sidebarLayout(
        sidebarPanel(
            numericInput("site", "Site:", value = 1, min = 1, max = 498)
                      
        ),

        # Show a plot of the generated final tibble
        mainPanel(
           plotOutput("bl_plot"),
           br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
           textOutput("entropy_score")
        )
    )
)

# Define server logic required to draw a scatterplot
server <- function(input, output) {   
    
    output$entropy_score    <- renderText(paste0("Entropy for this site is ", final_tibble %>%  
                                                                              dplyr::filter(site == input$site) %>% 
                                                                              dplyr::pull(entropy[1])))
    output$bl_plot          <- renderPlot(height = 600, width = 1250,{
   
   
    
        
       
        # define the dataframe
        #add modeled slope bias from results
        #make the app look nicer
        #add slopes to each plot x=1 and y=1
        #push all of this over to git
        #  focus on entropy and print out under each plot the entropy use pull entropy and text output
        
        #entropy_value <- pull(final_tibble, entropy)
        #print(entropy_value)
        final_tibble %>% 
        dplyr::filter(site == input$site) %>%
   
        ggplot(aes(x = persite_count, y = branch_length, color = model)) +
            geom_point()  + 
            geom_smooth(method = "lm") +  #, se = FALSE) +
            facet_grid(ASRV~model) +#, scales="free") + 
            #facet_wrap(model~ASRV, nrow=5) + 
            geom_abline(color = "black") + 
            labs(title = "Count vs Branch Length", x = "Persite Count", y = "Branch Length") +
            theme_bw(base_size = 18) +
            theme(plot.background = element_rect(fill = "white"),
                legend.position = "none", 
                strip.background = element_rect(color = "black", fill = "white")) +
            geom_text( x = 0.6, y = 3, aes(label = bias),color = "black") 
         
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
