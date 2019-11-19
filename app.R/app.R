library(tidyverse)
library(ggplot2)
library(formattable)
library(ggalluvial)
library(shiny)

data <- readRDS("clean-data/clean_data.rds")

ui <- navbarPage("Donations of Harvard-Employed Individuals to 2020 Presidential Campaigns",
                 
                 tabPanel("About",
                          
                          p("This plot visualizes the locations of traffic violations in Oklahoma City Santa Fe and Spring Lake districts by the race of the violator. The data is taken from the Stanford Open Policing Project. This graphic was created by Miroslav Bergam, a freshman at Harvard College.")),
                 
                 tabPanel("Plot", plotOutput("plot1")
                          )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$plot1 <- renderPlot(
        {
            number_donations <- data %>%
                group_by(committee_name) %>%
                summarise(num_donations = n()) %>%
                mutate(num_donations = as.numeric(num_donations))
            
            
            ggplot(number_donations, aes(x = reorder(committee_name, num_donations), y = num_donations, fill = committee_name)) +
                geom_col() + 
                theme(axis.text.x = element_text(angle = 90)) + 
                theme(legend.position = "none") + 
                labs(title = "Harvard Political Donations to 2020 Presidential Campaigns",
                     subtitle = "Donations of Individuals Employed by Harvard University",
                     x = "Presidential  Campaigns",
                     y = "Number of Individual Donations")
        })
        
}

# Run the application 
shinyApp(ui = ui, server = server)
