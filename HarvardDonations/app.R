library(tidyverse)
library(ggplot2)
library(formattable)
library(ggalluvial)
library(shiny)
library(shinythemes)
library(coefplot)

data <- read_rds("clean_data.rds")
data <- read_rds("employer_clean_data.rds")

ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage("Donations of Harvard-Employed Individuals to 2020 Presidential Campaigns",
                 
                 tabPanel("About the Project",
                          
                          p("This project looks to analyze how individuals employed by Harvard University donate their political dollars. I visualize in multiple different ways the size of the donations, the support different candidates recieve, and how political ideology impacts donations. I also look at how these donations differ within the graduate schools and how all of the donations compare to the general population's donations."),
                          p("I accessed data on the political donations of individuals employed by Harvard University on the website of the Federal Election Committee, which releases information on every donation made to a presidential, senate, or house political campaign, including the name of the donor, their occupation, their employer, the donated amount, their state, and the recipient campaign."),
                          p("Although this data was easily accessible by downloading a csv file, there was much data cleaning to be done to ensure that my analyses are accurate. First, I only observed donations made to 2020 presidential campaigns, excluding senate and house donations. Second, I filtered for donations made after January 1st, 2017, as there was no presidential donations to 2020 campaigns in 2016 or earlier. Third, I had to parse through the contributor employer data and filter out donations whose donors were not actually employed by Harvard University. For example, the donations from the employees of Harvard-Westlake School were present in the data, as they contain the word “Harvard.” Donations like this were removed."),
                          p("I also used the Federal Election Committee website to find data on the campaign donations of the general population, the total campaign sizes, and the political party affiliations of the different committees."),
                          p("Additionally, I sourced data from a DataForProgress.org article, which offered ideology scores for 2020 candidates who served in Congress. The specific data was not publicly availible, and I had to reach out to the article author to access it.")
                 ),
                 
                 tabPanel("About the Author",
                          h1("Miroslav Bergam"),
                          p("I'm from New Jersey and I'm currently a freshman at Harvard College. I'm interested in data science, government, and the humanities.")
                 ),
                 
                 tabPanel("Donation Information",
                          p("Select from the following plots to see how individuals employed by Harvard donate to 2020 presidential campaigns."),
                          selectInput("plot", label = h5("Select Plot"), 
                                      choices = list("Number of Donations" = "totalDonations",
                                                     "Unique Donors" = "uniqueDonors",
                                                     "Total Donation Sum" = "totalSum",
                                                     "Mean Donation Size" = "meanDonations",
                                                     "Donation Boxplot" = "donationBoxplot"), 
                                      selected =  "totalDonations",
                                      multiple = FALSE,
                                      selectize = FALSE,
                                      width = '400px',
                                      size = 1),
                          plotOutput("donationInfo")
                 ),
                 
                 tabPanel("Graduate School Analysis", 
                          p("Select from the following plots to see how employees of Harvard's graduate schools donate to 2020 presidential campaigns."),
                          selectInput("plot2", label = h5("Select Plot"), 
                                      choices = list("Total Donations" = "gradSchoolTotalDonations",
                                                     "Mean Donations" = "gradSchoolMeanDonations",
                                                     "Candidate Support by Employer" = "employerFacet",
                                                     "Candidate Support Flowchart" = "sankeyChart"), 
                                      selected = "gradSchoolTotalDonations",
                                      multiple = FALSE,
                                      selectize = FALSE,
                                      width = '400px',
                                      size = 1),
                          plotOutput("gradSchoolInfo")
                          
                 ),
                 
                 tabPanel("Professorship and Ideology Analysis",
                          p("Select from the following plots to see how factors such as whether the donor is a professor or the ideology of a candidate affect the size of their donations."),
                          selectInput("plot3", label = h5("Select Plot"), 
                                      choices = list("Professorship Regression" = "profRegress",
                                                     "Ideology Regression" = "ideologyRegress",
                                                     "Coefficient Plot" = "coefPlot"), 
                                      selected = "coefPlot",
                                      multiple = FALSE,
                                      selectize = FALSE,
                                      width = '400px',
                                      size = 1),
                          plotOutput("profIdeoAnalysis")
                 ),
                 
                 tabPanel("General Population Comparison",
                          p("Select from the following plots to see how Harvard donations compare to the overall donations to presidential campaigns."),
                          selectInput("plot4", label = h5("Select Plot"), 
                                      choices = list("Donation Size to Campaign Amount" = "donationSizeToTotalSize",
                                                     "Number of Donors to Campaign Amount" = "numDonorsToTotalSize"), 
                                      selected = "donationSizeToTotalSize",
                                      multiple = FALSE,
                                      selectize = FALSE,
                                      width = '400px',
                                      size = 1),
                          plotOutput("generalPop")
                 ))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
  output$donationInfo <- renderPlot({
    
    if(input$plot == "totalDonations") {     
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
    
      } else if(input$plot == "uniqueDonors") {
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
        
    } else if(input$plot == "totalSum"){
      
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
      
    } else if(input$plot == "meanDonations") {
      
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
    
      } else if(input$plot == "donationBoxplot") {
      
        data %>%
          group_by(committee_name) %>%
          mutate(num_donations = n()) %>%
          filter(num_donations > 20) %>%
          ggplot(aes(x = committee_name, y = contribution_receipt_amount))+
          theme(axis.text.x = element_text(angle = 90)) + 
          scale_y_log10()+
          geom_boxplot() + 
          labs(title = "Donations to Candidates with Greater than 20 Donations",
               subtitle = "Note the scale of the Y axis",
               x = "Candidates",
               y = "Donation Size in Dollars")
      
    }
  })
  
  output$gradSchoolInfo <- renderPlot({
    
    if(input$plot2 == "gradSchoolTotalDonations") {     
      
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
      
    } else if(input$plot2 == "gradSchoolMeanDonations") {

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
      
    } else if(input$plot2 == "employerFacet"){
      
      ggplot(by_employer, aes(x = committee_name, fill = committee_name)) +
        geom_bar() + 
        theme(axis.text.x = element_text(angle = 90)) + 
        theme(legend.position = "none") + 
        facet_wrap(~ contributor_employer) +
        labs(y = "Number of Donations",
             x = "Candidate")
      
    } else if(input$plot2 == "sankeyChart") {
      
      by_employer_grouped <- by_employer %>%
        group_by(contributor_employer, committee_name) %>%
        summarise(num_donors = n())
      
      ggplot(by_employer_grouped,
             aes(y = num_donors, axis1 = contributor_employer, axis2 = committee_name)) +
        geom_alluvium(aes(fill = committee_name), width = 1/12)+
      geom_stratum(width = 1/12, fill = "black", color = "grey") +
      geom_label(stat = "stratum", label.strata = TRUE)# +
      #scale_x_discrete(limits = c("Gender", "Dept"), expand = c(.05, .05)) +
      #scale_fill_brewer(type = "qual", palette = "Set1") +
      #ggtitle("UC Berkeley admissions and rejections, by sex and department")
      
    } 
  })
  
  output$profIdeoAnalysis <- renderPlot({
    
    if(input$plot3 == "ideologyRegress") {     
      
      lm(data = data, contribution_receipt_amount ~ estimate)
      
      ggplot(data, aes(x = estimate, y = contribution_receipt_amount))+
        scale_y_log10()+
        geom_jitter() + 
        geom_smooth(method = "lm") +
        labs(title = "Relationship Between Donation Size and Politican Ideology",
             x = "Estimated Political Ideology",
             y = "Donation Amount")
      
    } else if(input$plot3 == "profRegress") {
      
      lm(data = data, contribution_receipt_amount ~ professor)
      
      ggplot(data, aes(x = professor, y = contribution_receipt_amount))+
        scale_y_log10()+
        geom_jitter() + 
        geom_smooth(method = "lm") +
        labs(title = "Relationship Between Donation Size and Professorship",
             x = "Professor or Not",
             y = "Donation Size")
      
    } else if(input$plot3 == "coefPlot"){
      
      m1 <- lm(data = data, contribution_receipt_amount ~ professor + estimate) # plus professor
      
      summary(m1)
      
      #Visualize the coefficients
      
      coefplot(m1)
      
    }
  })
  
  output$generalPop <- renderPlot({
    
    if(input$plot4 == "donationSizeToTotalSize") {     
      
      ggplot(data, aes(x = receipts, y = contribution_receipt_amount))+
        scale_y_log10()+
        geom_jitter() + 
        geom_smooth(method = "lm") +
        #geom_smooth(method = "glm", method.args = list(family = "binomaial"), se = FALSE)+
        labs(title = "Relationship Between Donation Size and Campaign Total Donations",
             x = "Total Campaign Donation Receipts",
             y = "Donation Size in Dollars") + 
        scale_x_continuous(labels = function(x) format(x, scientific = FALSE))
      
    } else if(input$plot4 == "numDonorsToTotalSize") {
      
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
             y = "Number of Donors") +
        scale_x_continuous(labels = function(x) format(x, scientific = FALSE))
      
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
