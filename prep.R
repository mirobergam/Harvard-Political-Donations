library(tidyverse)
library(ggplot2)
library(formattable)
library(ggalluvial)

dir.create("clean-data")

data <- read_csv("presdonors.csv") %>%
  select(committee_name, contributor_name, 
         contributor_employer, 
         contributor_occupation, 
         contributor_first_name, 
         contributor_last_name, 
         contribution_receipt_amount, 
         contribution_receipt_date) %>%
  filter(committee_name != "") %>%
  filter(contributor_occupation != "STUDENT") %>%
  filter(contributor_occupation != "GRADUATE STUDENT") %>%
  filter(contributor_employer != "HARVARD-WESTLAKE SCHOOL") %>%
  filter(contributor_employer != "HARVARD VANGUARD") %>%
  filter(contributor_employer != "HARVARD VANGUARD MEDICAL ASSOCIATES") %>%
  filter(contributor_employer != "HARVARD VANGUARD HEALTH ED") %>%
  filter(contributor_employer != "HARVARD-EPWORTH UMC") %>%
  filter(contributor_employer != "HARVARD HOSPITAL") %>%
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


pres <- read_csv("pres.csv") %>%
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
  select(name, party_full, receipts)

data <- data %>%
  left_join(pres, by = c("committee_name" = "name"))

ideologies <- read_csv("ideology_score.csv") %>%
  select(Name, estimate)

data <- data %>%
  left_join(ideologies, by = c("committee_name" = "Name"))

write_rds(data, "clean-data/clean_data.rds")
