# tidy tuesday: Data on UK gender pay gap

# author: Simon Hulme
# organisation: dataGP-UK

# date created: 12th September 2022

# setup ----

library(tidyverse)

paygap <- 
  read_csv(paste0(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master',
    '/data/2022/2022-06-28/paygap.csv'))

# save raw data to data folder  ** TO DO **

# initial exploration ----

str(paygap)
head(paygap)
glimpse(paygap)
summary(paygap)

# preliminary feature selection
paygap <- 
  paygap %>% 
  select(-c(address, employer_name, company_link_to_gpg_info, 
            responsible_person))

# manage categorical variable: employer size

# what unique values are there for this variable
unique(paygap$employer_size)

# formally define missing data: currently "Not provided"
paygap <- 
  paygap %>% 
  mutate(employer_size = na_if(employer_size, 'Not Provided'))
unique(paygap$employer_size)  # check output

# mutate into ordered factor
paygap <- 
  paygap %>%
  mutate(employer_size =
           factor(
             employer_size,
             ordered = TRUE,
             levels = c(
               "250 to 499",
               "500 to 999",
               "1000 to 4999",
               "5000 to 19,999",
               "20,000 or more"
             )
           ))

# check output
levels(paygap$employer_size)
str(paygap)
