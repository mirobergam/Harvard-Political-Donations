library(tidyverse)
library(ggplot2)
library(formattable)
library(ggalluvial)
library(readxl)

# Creating a directory for the clean data

dir.create("clean-data")

# Reading in the FEC data on presidential campaign donations from individuals
# with an employer listed as "harvard" from 2017 to present

data <- read_csv("presdonors.csv") %>%
  
  # Selecting relevant variables
  
  select(committee_name, contributor_name, 
         contributor_employer, 
         contributor_occupation, 
         contributor_first_name, 
         contributor_last_name, 
         contribution_receipt_amount, 
         contribution_receipt_date) %>%
  
  # Filtering out donations that don't include the campaign name
  
  filter(committee_name != "") %>%
  
  # Filtering out employees of the harvard-westlake school, which were included
  # in the search  results
  
  filter(contributor_employer != "HARVARD-WESTLAKE SCHOOL") %>%
  
  # Filtering out employees of Harvard Vanguard, Health, Hospital, etc, who were
  # also included in the search results because Harvard is in their employer's
  # name
  
  filter(contributor_employer != "HARVARD VANGUARD") %>%
  filter(contributor_employer != "HARVARD VANGUARD MEDICAL ASSOCIATES") %>%
  filter(contributor_employer != "HARVARD VANGUARD HEALTH ED") %>%
  filter(contributor_employer != "HARVARD-EPWORTH UMC") %>%
  filter(contributor_employer != "HARVARD HOSPITAL") %>%
  
  # 
  
  filter(committee_name != "DEMOCRATS WORK FOR AMERICA, INC") %>%
  filter(committee_name != "BLACK PAC") %>%
  filter(committee_name != "FORWARD MAJORITY ACTION") %>%
  filter(committee_name != "MIND THE GAP") %>%
  filter(committee_name != "NEED TO IMPEACH") %>%
  filter(committee_name != "UNITE AMERICA CONGRESSIONAL ELECTION FUND") %>%
  filter(committee_name != "WYOMING STRONG ACTION") %>%
  filter(committee_name != "LCV VICTORY FUND") %>%
  filter(committee_name != "WITH HONOR FUND, INC.") %>%
  filter(committee_name != "JOE SESTAK FOR PRESIDENT") %>%
  filter(committee_name != "SMP") %>%
  filter(committee_name != "HOUSE MAJORITY PAC") %>%
  
  # Renaming the campaign names to be the names of the candidates, both for the
  # graphical purposes and to more easily link with other datasets
  
  mutate(committee_name=dplyr::recode(committee_name,
                                      `AMY FOR AMERICA` = "Amy Klobuchar",
                                      `BENNET FOR AMERICA` = "Michael Bennet",
                                      `BERNIE 2020` = "Bernie Sanders",
                                      `BETO FOR AMERICA` = "Beto O'Rourke",
                                      `BIDEN FOR PRESIDENT` = "Joe Biden",
                                      `BULLOCK FOR PRESIDENT` = "Steve Bullock",
                                      `CORY 2020` = "Cory Booker",
                                      `DONALD J. TRUMP FOR PRESIDENT, INC.` = "Donald Trump",
                                      `FRIENDS OF ANDREW YANG` = "Andrew Yang",
                                      `FRIENDS OF JOHN DELANEY` = "John Delaney",
                                      `GILLIBRAND 2020` = "Kirstan Gillibrand",
                                      `HICKENLOOPER 2020` = "John Hickenlooper",
                                      `INSLEE FOR AMERICA` = "Jay Inslee",
                                      `JULIAN FOR THE FUTURE` = "Julian Castro",
                                      `KAMALA HARRIS FOR THE PEOPLE` = "Kamala Harris",
                                      `PETE FOR AMERICA, INC.` = "Pete Buttigieg",
                                      `SETH MOULTON FOR AMERICA, INC.` = "Seth Moulton",
                                      `SWALWELL FOR AMERICA` = "Eric Swalwell",
                                      `TOM STEYER 2020` = "Tom Steyer",
                                      `TULSI NOW` = "Tulsi Gabbard",
                                      `WARREN FOR PRESIDENT, INC.` = "Elizabeth Warren",
                                      `WALSH FOR PRESIDENT` = "Joe Walsh",
                                      `WELD 2020 PRESIDENTIAL CAMPAIGN COMMITTEE, INC.` = "Bill Weld"))

# Reading in data on presidential candidates, which includes their political
# party and the current size of their campaigns in donation money

pres <- read_csv("pres.csv") %>%
  
  # Renaming them to join with the main dataset later
  
  mutate(name=dplyr::recode(name,
                            `KLOBUCHAR, AMY J.` = "Amy Klobuchar",
                            `BENNET, MICHAEL F.` = "Michael Bennet",
                            `SANDERS, BERNARD` = "Bernie Sanders",
                            `O'ROURKE, ROBERT BETO` = "Beto O'Rourke",
                            `BIDEN, JOSEPH R JR` = "Joe Biden",
                            `BULLOCK, STEVE` = "Steve Bullock",
                            `BOOKER, CORY A.` = "Cory Booker",
                            `TRUMP, DONALD J.` = "Donald Trump",
                            `YANG, ANDREW MR.` = "Andrew Yang",
                            `DELANEY, JOHN K.` = "John Delaney",
                            `GILLIBRAND, KIRSTEN` = "Kirstan Gillibrand",
                            `HICKENLOOPER, JOHN W.` = "John Hickenlooper",
                            `INSLEE, JAY R` = "Jay Inslee",
                            `CASTRO, JULIAN` = "Julian Castro",
                            `HARRIS, KAMALA D.` = "Kamala Harris",
                            `BUTTIGIEG, PETE` = "Pete Buttigieg",
                            `MOULTON, SETH` = "Seth Moulton",
                            `SWALWELL, ERIC MICHAEL` = "Eric Swalwell",
                            `STEYER, TOM` = "Tom Steyer",
                            `GABBARD, TULSI` = "Tulsi Gabbard",
                            `WARREN, ELIZABETH` = "Elizabeth Warren",
                            `WALSH, JOE` = "Joe Walsh",
                            `WELD, WILLIAM FLOYD (BILL)` = "Bill Weld")) %>%

  # Selecting relevant variables
  
  select(name, party_full, receipts)

# Joining the dataset with political party and campaign size to the main dataset

data <- data %>%
  left_join(pres, by = c("committee_name" = "name"))

# Reading in list of ideological scores for the candidates

ideologies <- read_csv("ideology_score.csv") %>%
  select(Name, estimate)

# Merging ideological scores to main dataset

data <- data %>%
  left_join(ideologies, by = c("committee_name" = "Name"))

# Reading in ideological scores that include estimates for candidates who don't
# have a score in the other dataset

ideologies_estimates <- read_csv("ideology_score_estimates.csv") %>%
  select(Name, guesstimate)

# Merging to main dataset

data <- data %>%
  left_join(ideologies_estimates, by = c("committee_name" = "Name"))

# Creating a variable that has the number 1 if the donor is an educator, and 0
# if not

data <- data %>%
  mutate(professor = ifelse(contributor_occupation == "PROFESSOR" |
                              contributor_occupation == "FACULTY" |
                              contributor_occupation == "TEACHER" |
                              contributor_occupation == "EDUCATOR", 1, 0))

# Creating a dataset that ONLY includes data on donors from the graduate schools

by_employer <- data %>%
  filter(contributor_employer == "HARVARD KENNEDY SCHOOL"  | 
           contributor_employer == "HARVARD LAW SCHOOL" | 
           contributor_employer == "HARVARD BUSINESS SCHOOL" | 
           contributor_employer == "HARVARD SCHOOL OF PUBLIC HEALTH" |
           contributor_employer == "HARVARD GRADUATE SCHOOL OF ARTS AND SCIENCES" |
           contributor_employer == "HARVARD GRADUATE SCHOOL OF EDUCATION" |
           contributor_employer == "HARVARD DIVINITY SCHOOL")

# Writing to an RDS so it can be used in the shinyapp file

write_rds(data, "HarvardDonations/clean_data.rds")
write_rds(by_employer, "HarvardDonations/employer_clean_data.rds")
