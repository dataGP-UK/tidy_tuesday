# tidy tuesday: Data on UK gender pay gap

# author: Simon Hulme
# organisation: dataGP-UK

# date created: 12th September 2022

# setup ----

library(tidyverse)
library(lubridate)
library(finalfit)

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

## check for any duplicated observations ----
sum(duplicated(paygap))  # zero duplicate records

# how about employer_id?
n_distinct(paygap$employer_id)  # less than number of observations

# visualise 
paygap %>% 
  count(employer_id) %>% 
  ggplot() +
  geom_histogram(aes(n), binwidth = 0.5)

# most employer ids appear to be associated with multiple records
# explore reasons for this e.g. are they from different years?

# there are 2 variables for dates (submitted and due)

# explore 'date_submitted' variable
min(paygap$date_submitted)
max(paygap$date_submitted)

# convert to year and explore frequency distribution by employer id and year
paygap %>% 
  mutate(
    year = year(date_submitted) # extract year from YMD format
  ) %>% 
  group_by(employer_id, year) %>% 
  count() %>% 
  arrange(desc(n))

# shows that some years have multiple entries 
# need to explore using an example

paygap %>% 
  filter(employer_id == 132) %>% 
  view

# for this employer all data submitted in 2021 includes previous years
# this is not reflected by the logical variable 'submitted after the deadline'

# how do date due and date submitted relate to each other?
submission_delays <- 
  paygap %>% 
  select(date_submitted, due_date) %>% 
  mutate(
    delay = date_submitted - due_date,
    delay = as.numeric(as.duration(delay), 'days'))

submission_delays$delay %>% hist()
summary(submission_delays$delay)
rm(submission_delays)

# I am interested in exploring time series data so create year variable 
# using year when data due

min(paygap$due_date)
max(paygap$due_date)

# reassign year variable using due date
paygap %>%
  mutate(year = lubridate::year(due_date)) %>%
  group_by(employer_id, year) %>%
  count() %>%
  arrange(desc(n))

# there are now 4 companies who have 2 sets of data due in the same year
# are there any common features
paygap %>%
  mutate( year = lubridate::year(due_date)) %>% 
  filter(employer_id %in% c(8295, 11744, 11766, 20048) & 
           year %in% c(2020, 2021)) %>% 
  select(current_name, employer_id, current_name, year, due_date, 
         date_submitted, submitted_after_the_deadline) %>% 
  arrange(current_name, year) %>% 
  view()

# remove duplicate entries and repeat above code
paygap %>% 
  select(-c(date_submitted, submitted_after_the_deadline)) %>% 
  unique() %>% 
  mutate(year = lubridate::year(due_date)) %>% 
  filter(employer_id %in% c(8295, 11744, 11766, 20048) & 
           year %in% c(2020, 2021)) %>% 
  view()

# there is a possible error for company 8295 who have 2 identical submissions
# in 2021 with 2 different employer sizes. 
# TO DO: explore source data for this company

# otherwise run this code to create working dataset with data by year and 
# without duplicate entries

paygap_w <- 
  paygap %>% 
  mutate(year = lubridate::year(due_date)) %>% 
  select(-c(date_submitted, submitted_after_the_deadline)) %>% 
  unique()

## missing data ----

### explicit missing data ----

finalfit::ff_glimpse(paygap_w)

## explore differences in bonuses between males and females

## there is no missing data for percent of men/women paid a bonus
paygap_w %>% 
  ggplot() +
  geom_histogram(aes(male_bonus_percent))

paygap_w %>% 
  ggplot() +
  geom_histogram(aes(female_bonus_percent))

## but there are 8932 observations with missing date for difference in bonuses

## Q: Are those who don't receive a bonus those with missing values in diff?
paygap_w %>% 
  filter(female_bonus_percent == 0 | male_bonus_percent == 0) %>% 
  summarise(missing = sum(is.na(diff_mean_bonus_percent)))

paygap_w %>% 
  filter(female_bonus_percent == 0 | male_bonus_percent == 0) %>% 
  summarise(missing = sum(is.na(diff_median_bonus_percent)))

# it appears that most of missing values relate to bonuses not being paid

missing_bonus <- 
  paygap_w %>% 
  # select observations where bonuses are paid to male and female but there
  # is missing data for values of mean or median difference in bonus
  filter((female_bonus_percent != 0 & male_bonus_percent != 0) &
           (is.na(diff_median_bonus_percent) | is.na(diff_mean_bonus_percent))) %>% 
  select(employer_id, diff_mean_bonus_percent, diff_median_bonus_percent,
         male_bonus_percent, female_bonus_percent, everything()) %>% 
  arrange(employer_id)

missing_bonus

## 14 observations where data where males and females receive a bonus 
## and no mean and or median difference available

missing_plot(missing_bonus)  # no additional pattern evident

## TO DO: decide how to handle missing data in these 14 observations
## what type of missing data is this??

## explore missing values in hourly pay quartile data

## it appears that 392 observations have missing data relating to hourly pay
## quartile data although there is no missing data in the diff hourly percent
## variables.

## TO DO: explore the above ****







### is there implicit missing data ----

## ie. are there years missing that don't show up with any values?

complete_paygap <-
  paygap_w %>%
  complete(employer_id, year)

summary(complete_paygap)

# there are many companies with data missing for years 
# explore if there are any patterns in this data

complete_paygap %>% 
  select(employer_id, year, current_name) %>% 
  group_by(year) %>% 
  summarise(missing_data = sum(is.na(current_name))) %>% 
  ggplot() +
  geom_col(aes(year, missing_data))

# i can understand why data incomplete for 2023 but unsure why other years
# especially spike in missing data for 2020

# what data is there for 2023 considering that this is next year?
paygap_w %>% 
  filter(year == 2023) %>% 
  select(employer_id, current_name, due_date, year) %>% 
  n_distinct()

# 125 companies have submitted data for next year already
# for this analysis I will exclude these observations

paygap_upto22 <- 
  paygap_w %>% 
  filter(year != 2023)

# i would now like to plot missing data by year and company

finalfit::missing_plot(complete_paygap)
