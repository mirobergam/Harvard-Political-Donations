library(tidyverse)
library(ggplot2)
library(formattable)
library(ggalluvial)
library(shiny)
library(shinythemes)
library(coefplot)

data <- read_rds("clean_data.rds")
by_employer <- read_rds("employer_clean_data.rds")

# Using flatly theme

ui <- fluidPage(theme = shinytheme("flatly"),
               
                # Main page title
                
                 navbarPage("Donations of Harvard-Employed Individuals to 2020 Presidential Campaigns",
                 
                # About the project
                
                 tabPanel("About the Project",
                          
                          # Project description
                          
                          p("This project looks to analyze how individuals employed by Harvard University donate their political dollars. I visualize in multiple different ways the size of the donations, the support different candidates recieve, and how political ideology impacts donations. I also look at how these donations differ within the graduate schools and how all of the donations compare to the general population's donations."),
                          p("I accessed data on the political donations of individuals employed by Harvard University on the website of the Federal Election Committee, which releases information on every donation made to a presidential, senate, or house political campaign, including the name of the donor, their occupation, their employer, the donated amount, their state, and the recipient campaign."),
                          p("Although this data was easily accessible by downloading a csv file, there was much data cleaning to be done to ensure that my analyses are accurate. First, I only observed donations made to 2020 presidential campaigns, excluding senate and house donations. Second, I filtered for donations made after January 1st, 2017, as there was no presidential donations to 2020 campaigns in 2016 or earlier. Third, I had to parse through the contributor employer data and filter out donations whose donors were not actually employed by Harvard University. For example, the donations from the employees of Harvard-Westlake School were present in the data, as they contain the word “Harvard.” Donations like this were removed."),
                          p("I also used the Federal Election Committee website to find data on the campaign donations of the general population, the total campaign sizes, and the political party affiliations of the different committees."),
                          p("Additionally, I sourced data from a DataForProgress.org article, which offered ideology scores for 2020 candidates who served in Congress. The specific data was not publicly availible, and I had to reach out to the article author to access it.")
                 ),
                
                # About the author
                 
                 tabPanel("About the Author",
                          h1("Miroslav Bergam"),
                          p("I'm from New Jersey and I'm currently a freshman at Harvard College. I'm interested in data science, government, and the humanities.")
                 ),
                 
                # General donation information graphs
                
                 tabPanel("Donation Information",
                          p("Select from the following plots to see how individuals employed by Harvard donate to 2020 presidential campaigns."),
                          
                          # Drop-down menu for the different graphs, linked to r chunks in the server
                          
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
                          
                          # donationInfo is the server's name for the selected chart
                          
                          plotOutput("donationInfo")
                 ),
                 
                
                # Grad school donation graphs
                
                 tabPanel("Graduate School Analysis", 
                          p("Select from the following plots to see how employees of Harvard's graduate schools donate to 2020 presidential campaigns."),
                          
                          # Drop-down menu for the different graphs, linked to r chunks in the server
                          
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
                          
                          # gradSchoolInfo is the server's name for the selected chart
                          
                          plotOutput("gradSchoolInfo")
                          
                 ),
                 
                # Educator and Ideological analysis graphs
                
                 tabPanel("Professorship and Ideology Analysis",
                          p("Select from the following plots to see how factors such as whether the donor is a professor or the ideology of a candidate affect the size of their donations."),
                          
                          # Drop-down menu for the different graphs, linked to r chunks in the server
                          
                          selectInput("plot3", label = h5("Select Plot"), 
                                      choices = list("Professorship Regression" = "profRegress",
                                                     "Ideology Regression" = "ideologyRegress",
                                                     "Coefficient Plot" = "coefPlot"), 
                                      selected = "coefPlot",
                                      multiple = FALSE,
                                      selectize = FALSE,
                                      width = '400px',
                                      size = 1),
                          
                          # profIdeoAnalysis is the server's name for the selected chart
                          
                          plotOutput("profIdeoAnalysis")
                 ),
                
                # General population donation comparison graphs
                 
                 tabPanel("General Population Comparison",
                          p("Select from the following plots to see how Harvard donations compare to the overall donations to presidential campaigns."),
                          
                          # Drop-down menu for the different graphs, linked to r chunks in the server
                          
                          selectInput("plot4", label = h5("Select Plot"), 
                                      choices = list("Donation Size to Campaign Amount" = "donationSizeToTotalSize",
                                                     "Number of Donors to Campaign Amount" = "numDonorsToTotalSize"), 
                                      selected = "donationSizeToTotalSize",
                                      multiple = FALSE,
                                      selectize = FALSE,
                                      width = '400px',
                                      size = 1),
                          
                          # donationInfo is the server's name for the general population chart
                          
                          plotOutput("generalPop")
                 ))
)


server <- function(input, output) {

  # Chart selected by user gets assigned to donationInfo, which is outputted
  
  output$donationInfo <- renderPlot({
    
    if(input$plot == "totalDonations") { 
      
      # Cleaning data for the graphic for the number of donations
      
      number_donations <- data %>%
        
        # Grouping by committee name and...
        
        group_by(committee_name) %>%
        
        # Counting the number of rows in each group. Each row is a donation
        
        summarise(num_donations = n()) %>%
        
        # Converting variable into a number 
        
        mutate(num_donations = as.numeric(num_donations))
      
      # Graphing the nnumber of donations to each candidate 
      
      ggplot(number_donations, aes(x = reorder(committee_name, num_donations), y = num_donations, fill = committee_name)) +
        geom_col() + 
        theme(axis.text.x = element_text(angle = 90)) + 
        theme(legend.position = "none") + 
        labs(title = "Harvard Political Donations to 2020 Presidential Campaigns",
             subtitle = "Donations of Individuals Employed by Harvard University",
             x = "Presidential  Campaigns",
             y = "Number of Individual Donations")
      
      } else if(input$plot == "uniqueDonors") {
        
        # Cleaning the data for the graphic of number of unique donors
        
        num_donors <- data %>%
         
          # Combining  the donations of any person who's donated to the same
          # campaign multiple times into one row
          
          distinct(committee_name, contributor_name) %>%
          
          # Grouping each of these unique donors by the campaign they donated to
          
          group_by(committee_name) %>%
          
          # Counting the number of unique donors in each group
          
          summarise(num_donors = n())
        
        # Graphing the number of unique donors to each campaign
        
        ggplot(num_donors, aes(x = reorder(committee_name, num_donors), y = num_donors, fill = committee_name)) +
          geom_col() + 
          theme(axis.text.x = element_text(angle = 90)) + 
          theme(legend.position = "none") +
          labs(title = "Harvard Political Donors to 2020 Presidential Campaigns",
               subtitle = "Individuals Employed by Harvard University",
               x = "Presidential Campaigns",
               y = "Number of Donors")
        
    } else if(input$plot == "totalSum"){
      
      # Cleaning the data for the amount of dollars donated to each campaign 
      
      amount_graph <- data %>%
        
        # Grouping by the campaign
        
        group_by(committee_name) %>%
        
        # Summing up all of the donation receipts to get aggregate donations
        
        summarise(total_donations = sum(contribution_receipt_amount)) %>%
        
        # Converting the value to a number
        
        mutate(total_donations = as.numeric(total_donations))
      
      # Graphing the amount of money donation to each campaign 
      
      ggplot(amount_graph, aes(x = reorder(committee_name, total_donations), y = total_donations, fill = committee_name)) +
        geom_col() + 
        theme(axis.text.x = element_text(angle = 90)) + 
        theme(legend.position = "none") +
        labs(title = "Harvard Employees' Total Donations to 2020 Presidential Campaigns",
             subtitle = "Donations by Individuals Employed by Harvard University",
             x = "Presidential Campaigns",
             y = "Dollars")
      
    } else if(input$plot == "meanDonations") {
      
      # Cleaning data for mean donation graph
      
      amount_graph <- data %>%
        
        # Grouping by campaign
        
        group_by(committee_name) %>%
        
        # Taking the mean of all of the donation receipts within each campaign
        
        summarise(mean_donations = mean(contribution_receipt_amount)) %>%
       
        # Converting this mean to a number
        
         mutate(mean_donations = as.numeric(mean_donations))
      
      # Plotting the mean donations to each campaign 
      
      ggplot(amount_graph, aes(x = reorder(committee_name, mean_donations), y = mean_donations, fill = committee_name)) +
        geom_col() + 
        theme(axis.text.x = element_text(angle = 90)) + 
        theme(legend.position = "none") +
        labs(title = "Harvard Employees' Mean Donations to 2020 Presidential Campaigns",
             subtitle = "Donations by Individuals Employed by Harvard University",
             x = "Presidential Campaigns",
             y = "Dollars")
    
      } else if(input$plot == "donationBoxplot") {
      
        # Creating a boxplot of the donations to the top 6 candidates
        
        data %>%
          
          # Grouping by campaign
          
          group_by(committee_name) %>%
          
          # Creating a new column that has the number of donations to the
          # respective campaign
          
          mutate(num_donations = n()) %>%
          
          # Filtering out campaigns that had less than 20 donations to clear up
          # space on graph
          
          filter(num_donations > 20) %>%
          
          # Creating a boxplot of donation sizes 
          
          ggplot(aes(x = committee_name, y = contribution_receipt_amount))+
          theme(axis.text.x = element_text(angle = 90)) + 
          scale_y_log10()+
          geom_boxplot() + 
          labs(title = "Donations to Candidates with Greater than 20 Donations",
               subtitle = "Note the logarithmic scale of the Y axis",
               x = "Candidates",
               y = "Donation Size in Dollars")
      
    }
  })
  
  # Chart selected by user gets assigned to gradSchoolInfo, which is outputted
  
  output$gradSchoolInfo <- renderPlot({
    
    if(input$plot2 == "gradSchoolTotalDonations") {     
      
      # Cleaning data to graph the sum of donations by graduate school employees
      
      by_employer_sum <- by_employer %>%  
        
        # Grouping by the graduate school employing the donors
        
        group_by(contributor_employer) %>%
        
        # Adding the donation receipts to get total sum of donations
        
        summarise(total_donations = sum(contribution_receipt_amount)) %>%
        
        # Casting the value into a number
        
        mutate(total_donations = as.numeric(total_donations))
      
      # Plotting the total sum of donations from employees of graduate schools
      
      ggplot(by_employer_sum, aes(x = reorder(contributor_employer, total_donations), y = total_donations, fill = contributor_employer)) +
        geom_col() + 
        theme(axis.text.x = element_text(angle = 90)) + 
        theme(legend.position = "none") +
        labs(title = "Harvard Grad School Employees' Total Donations to 2020 Pres. Campaigns",
             subtitle = "Donations by Individuals Employed by Graduate Schools",
             x = "Graduate Schools",
             y = "Dollars")
      
    } else if(input$plot2 == "gradSchoolMeanDonations") {
        
      # Cleaning the data to graph the mean donation of each grad school
      
      by_employer_mean <- by_employer %>% 
        
        # Grouping by grad school
        
        group_by(contributor_employer) %>%
        
        # Finding the mean contribution within each grad schoool
        
        summarise(total_donations = mean(contribution_receipt_amount)) %>%
        
        # Converting to a number
        
        mutate(total_donations = as.numeric(total_donations))
      
      # Graphing the mean donation of employees of Harvard graduate schools
      
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
      
      # Creating an alluvium/sankey chart of donor support
      
      by_employer_grouped <- by_employer %>%
        
        # Recoding grad school names to their abbreviations for space on the chart
        
        mutate(contributor_employer=dplyr::recode(contributor_employer,
                                  `HARVARD BUSINESS SCHOOL` = "HBS",
                                  `HARVARD LAW SCHOOL` = "HLS",
                                  `HARVARD DIVINITY SCHOOL` = "HDS",
                                  `HARVARD KENNEDY SCHOOL` = "HKS",
                                  `HARVARD SCHOOL OF PUBLIC HEALTH` = "HSPH",
                                  `HARVARD GRADUATE SCHOOL OF EDUCATION` = "HGSE")) %>%
        
        # Grouping by committee and filtering out candidtes with less than 5
        # grad school employee donations
        
        group_by(committee_name) %>%
        mutate(num_donations = n()) %>%
        filter(num_donations >= 5) %>%
        
        # Ungrouping
        
        ungroup() %>%
        
        # Now grouping by employer and commitee name, as alluvium charts need to
        # show the subdivisions within these two variables
        
        group_by(contributor_employer, committee_name) %>%
        
        # Summarising the number of donors within each subgroup
        
        summarise(num_donors = n())
      
      # Making an alluvium chart of graduate school employee donor support
      
      ggplot(by_employer_grouped, aes(y = num_donors, 
                                      axis1 = contributor_employer, 
                                      axis2 = committee_name)) +
        geom_alluvium(aes(fill = committee_name), width = 1/12)+
      geom_stratum(width = 1/12, fill = "black", color = "grey") +
      geom_label(stat = "stratum", label.strata = TRUE) +
      theme(legend.position = "none",
            axis.text.y = element_blank(),
            axis.text.x = element_blank()) +
      labs(y = "Graduate Schools",
           title = "Alluvium Chart of Graduate School Donor Support",
           subtitle = "Showing candidates with more than 5 donations")
    } 
  })
  
  # Chart selected by user gets assigned to profIdeoAnalysis, which is outputted
  
  output$profIdeoAnalysis <- renderPlot({
    
    if(input$plot3 == "ideologyRegress") {     
      
      # Linear model of estimate explaining contribution size
      #lm(data = data, contribution_receipt_amount ~ estimate)
      
      # Plotting the ideological estimates against the donation size and fitting
      # a linear model to it using geom_smooth
      
      ggplot(data, aes(x = estimate, y = contribution_receipt_amount))+
        scale_y_log10()+
        geom_jitter() + 
        geom_smooth(method = "lm") +
        labs(title = "Relationship Between Donation Size and Politican Ideology",
             x = "Estimated Political Ideology",
             y = "Donation Amount")
      
    } else if(input$plot3 == "profRegress") {
      
      # Linear model of education occupation explaining contribution size
      #lm(data = data, contribution_receipt_amount ~ professor)
      
      # Plotting whether or not the contributor is an educator against the
      # donation size and fitting a linear model to it using geom_smooth
      
      ggplot(data, aes(x = professor, y = contribution_receipt_amount))+
        scale_y_log10()+
        geom_jitter() + 
        geom_smooth(method = "lm") +
        labs(title = "Relationship Between Donation Size and Education Occupation",
             x = "Not an Educator vs. Educator",
             y = "Donation Size",
             caption = "Educator includes any donor whose listed occupation is 'Professor', 'Educator', 'Faculty', or 'Teacher'")
      
    } else if(input$plot3 == "coefPlot"){
      
      # Creating a coefficient plot displaying the coefficients of ideological
      # estimate and educatorship
      
      m1 <- lm(data = data, contribution_receipt_amount ~ professor + estimate)
      
      #Visualize the coefficients using coefplot
      
      coefplot(m1)
      
    }
  })
  
  # Chart selected by user gets assigned to generalPop, which is outputted
  
  output$generalPop <- renderPlot({
    
    if(input$plot4 == "donationSizeToTotalSize") {     
      
      # Plotting the campaign donation sizes against the total size of the
      # respective campaigns being donated to. Fitting a linear model to this
      # using geom_smooth
      
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
      
      # Cleaning data to have one row from each campaign that includes the
      # number of donors to the total campaign
      
      donor_to_size <- data %>%
        group_by(committee_name) %>%
        mutate(num_donors = n()) %>%
        slice(1) %>%
        ungroup()
      
      # Plotting the total campaign size in donations against the number of
      # donors and fitting a linear model to it using geom_smooth
      
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
