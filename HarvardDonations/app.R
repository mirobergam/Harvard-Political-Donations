library(tidyverse)
library(ggplot2)
library(formattable)
library(ggalluvial)
library(shiny)

data <- read_rds("clean_data.rds")

ui <- navbarPage("Donations of Harvard-Employed Individuals to 2020 Presidential Campaigns",
                 
                 tabPanel("About",
                          
                          p("This plot visualizes the locations of traffic violations in Oklahoma City Santa Fe and Spring Lake districts by the race of the violator. The data is taken from the Stanford Open Policing Project. This graphic was created by Miroslav Bergam, a freshman at Harvard College.")),
                 
                 tabPanel("General Donation Information",
                          plotOutput("totalDonations"),
                          plotOutput("uniqueDonors"),
                          plotOutput("totalSum"),
                          plotOutput("meanDonations")
                 ),
                 
                 tabPanel("Graduate School Analysis", 
                          plotOutput("gradSchoolTotalDonations"),
                          plotOutput("gradSchoolMeanDonations"),
                          plotOutput("employerFacet")
                 ),
                 
                 tabPanel("Regressions", 
                          plotOutput("numDonorsToTotalSize"),
                          plotOutput("donationSizeToTotalSize"),
                          plotOutput("profRegress"),
                          plotOutput("ideologyRegress"),
                          plotOutput("coefPlot")
                 )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$totalDonations <- renderPlot(
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
    
    output$uniqueDonors <- renderPlot(
        {
            num_donors <- data %>%
                distinct(committee_name, contributor_name) %>%
                group_by(committee_name) %>%
                summarise(num_donors = n())
            
            ggplot(num_donors, aes(x = reorder(committee_name, num_donors), y = num_donors, fill = committee_name)) +
                geom_col() + 
                theme(axis.text.x = element_text(angle = 90)) + 
                theme(legend.position = "none") +
                labs(title = "Harvard Political Donors to 2020 Presidential Campaigns",
                     subtitle = "Individuals Employed by Harvard University",
                     x = "Presidential Campaigns",
                     y = "Number of Donors")
        })
    
    output$totalSum <- renderPlot(
        {
            amount_graph <- data %>%
                group_by(committee_name) %>%
                summarise(total_donations = sum(contribution_receipt_amount)) %>%
                mutate(total_donations = as.numeric(total_donations))
            
            ggplot(amount_graph, aes(x = reorder(committee_name, total_donations), y = total_donations, fill = committee_name)) +
                geom_col() + 
                theme(axis.text.x = element_text(angle = 90)) + 
                theme(legend.position = "none") +
                labs(title = "Harvard Employees' Total Donations to 2020 Presidential Campaigns",
                     subtitle = "Donations by Individuals Employed by Harvard University",
                     x = "Presidential Campaigns",
                     y = "Dollars")
        })
    
    output$meanDonations <- renderPlot(
      {
        amount_graph <- data %>%
          group_by(committee_name) %>%
          summarise(mean_donations = mean(contribution_receipt_amount)) %>%
          mutate(mean_donations = as.numeric(mean_donations))
        
        ggplot(amount_graph, aes(x = reorder(committee_name, mean_donations), y = mean_donations, fill = committee_name)) +
          geom_col() + 
          theme(axis.text.x = element_text(angle = 90)) + 
          theme(legend.position = "none") +
          labs(title = "Harvard Employees' Mean Donations to 2020 Presidential Campaigns",
               subtitle = "Donations by Individuals Employed by Harvard University",
               x = "Presidential Campaigns",
               y = "Dollars")
      })
    
    output$gradSchoolTotalDonations <- renderPlot(
      {
        by_employer <- data %>%
          filter(contributor_employer == "HARVARD KENNEDY SCHOOL"  | 
                   contributor_employer == "HARVARD LAW SCHOOL" | 
                   contributor_employer == "HARVARD BUSINESS SCHOOL" | 
                   contributor_employer == "HARVARD SCHOOL OF PUBLIC HEALTH" |
                   contributor_employer == "HARVARD GRADUATE SCHOOL OF ARTS AND SCIENCES" |
                   contributor_employer == "HARVARD GRADUATE SCHOOL OF EDUCATION" |
                   contributor_employer == "HARVARD DIVINITY SCHOOL")
        
        by_employer_sum <- by_employer %>%  
          group_by(contributor_employer) %>%
          summarise(total_donations = sum(contribution_receipt_amount)) %>%
          mutate(total_donations = as.numeric(total_donations))
        
        ggplot(by_employer_sum, aes(x = reorder(contributor_employer, total_donations), y = total_donations, fill = contributor_employer)) +
          geom_col() + 
          theme(axis.text.x = element_text(angle = 90)) + 
          theme(legend.position = "none") +
          labs(title = "Harvard Grad School Employees' Total Donations to 2020 Pres. Campaigns",
               subtitle = "Donations by Individuals Employed by Graduate Schools",
               x = "Graduate Schools",
               y = "Dollars")
      })
    
    output$gradSchoolMeanDonations <- renderPlot(
      {
        by_employer <- data %>%
          filter(contributor_employer == "HARVARD KENNEDY SCHOOL"  | 
                   contributor_employer == "HARVARD LAW SCHOOL" | 
                   contributor_employer == "HARVARD BUSINESS SCHOOL" | 
                   contributor_employer == "HARVARD SCHOOL OF PUBLIC HEALTH" |
                   contributor_employer == "HARVARD GRADUATE SCHOOL OF ARTS AND SCIENCES" |
                   contributor_employer == "HARVARD GRADUATE SCHOOL OF EDUCATION" |
                   contributor_employer == "HARVARD DIVINITY SCHOOL")
        
        by_employer_mean <- by_employer %>%  
          group_by(contributor_employer) %>%
          summarise(total_donations = mean(contribution_receipt_amount)) %>%
          mutate(total_donations = as.numeric(total_donations))
        
        ggplot(by_employer_mean, aes(x = reorder(contributor_employer, total_donations), y = total_donations, fill = contributor_employer)) +
          geom_col() + 
          theme(axis.text.x = element_text(angle = 90)) + 
          theme(legend.position = "none") +
          labs(title = "Harvard Grad School Employees' Mean Donations to 2020 Pres. Campaigns",
               subtitle = "Donations by Individuals Employed by Graduate Schools",
               x = "Graduate Schools",
               y = "Dollars")
      })
    
    output$employerFacet <- renderPlot(
        {
            by_employer_facet <- data %>%
                filter(contributor_employer == "HARVARD KENNEDY SCHOOL"  | 
                           contributor_employer == "HARVARD LAW SCHOOL" | 
                           contributor_employer == "HARVARD BUSINESS SCHOOL" | 
                           contributor_employer == "HARVARD SCHOOL OF PUBLIC HEALTH" |
                           contributor_employer == "HARVARD GRADUATE SCHOOL OF EDUCATION")
            
            ggplot(by_employer_facet, aes(x = committee_name, fill = committee_name)) +
                geom_bar() + 
                theme(axis.text.x = element_text(angle = 90)) + 
                theme(legend.position = "none") + 
                facet_wrap(~ contributor_employer) +
                labs(y = "Number of Donations",
                     x = "Candidate")
        })
    
    output$ideologyRegress <- renderPlot(
        {
            lm(data = data, contribution_receipt_amount ~ estimate)
            
            ggplot(data, aes(x = estimate, y = contribution_receipt_amount))+
                scale_y_log10()+
                geom_jitter() + 
                geom_smooth(method = "lm") +
                labs(title = "Relationship Between Donation Size and Politican Ideology",
                     x = "Estimated Political Ideology",
                     y = "Donation Amount")
        })
    
    output$profRegress <- renderPlot(
        {
            lm(data = data, contribution_receipt_amount ~ professor)
            
            ggplot(data, aes(x = professor, y = contribution_receipt_amount))+
                scale_y_log10()+
                geom_jitter() + 
                geom_smooth(method = "lm") +
                labs(title = "Relationship Between Donation Size and Professorship",
                     x = "Professor or Not",
                     y = "Donation Size")
        })
    
    output$donationSizeToTotalSize <- renderPlot(
        {
            ggplot(data, aes(x = receipts, y = contribution_receipt_amount))+
                scale_y_log10()+
                geom_jitter() + 
                geom_smooth(method = "lm") +
                #geom_smooth(method = "glm", method.args = list(family = "binomaial"), se = FALSE)+
                labs(title = "Relationship Between Donation Size and Campaign Total Donations",
                     x = "Total Campaign Donation Receipts",
                     y = "Donation Amount")
        })
    
    output$numDonorsToTotalSize <- renderPlot(
        {
            donor_to_size <- data %>%
                group_by(committee_name) %>%
                mutate(num_donors = n()) %>%
                slice(1) %>%
                ungroup()
            
            ggplot(donor_to_size, aes(x = receipts, y = num_donors))+
                scale_y_log10()+
                geom_jitter() + 
                geom_smooth(method = "lm") +
                labs(title = "Relationship Between Number of Donors and Campaign Total Donations",
                     x = "Total Campaign Donation Receipts",
                     y = "Number of Donors")
        })
    
    output$coefPlot <- renderPlot(
        {
            m1 <- lm(data = data, contribution_receipt_amount ~ professor + estimate) # plus professor
        
            summary(m1)
            
            #Visualize the coefficients
            
            coefplot(m1)
        })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
