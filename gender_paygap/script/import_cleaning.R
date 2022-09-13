# tidy tuesday: Data on UK gender pay gap

# author: Simon Hulme
# organisation: dataGP-UK

# date created: 12th September 2022

# setup ----

library(tidyverse)
library(lubridate)
library(finalfit)
library(here)

paygap <- 
  read_csv(paste0(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master',
    '/data/2022/2022-06-28/paygap.csv'))

# save raw data to data folder
write_csv(paygap, 'gender_paygap/data/raw/paygap_raw.csv')

# initial exploration ----

str(paygap)
head(paygap)
glimpse(paygap)
summary(paygap)

## preliminary feature selection ----
paygap <- 
  paygap %>% 
  # remove variables that will not be used in analysis
  select(-c(address, employer_name, company_link_to_gpg_info, 
            responsible_person))

## manage categorical variables ----

# employer_size is the only categorical variable to convert to factor

# this is ordinal data so ensure ordered factor created
# missing data currently labelled as "Not provided"

paygap <-
  paygap %>%
  mutate(
    # replace 'Not Provided' with NA
    employer_size = na_if(employer_size, 'Not Provided'),
    # create ordered factor
    employer_size =
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
      )
  )

# check output
class(paygap$employer_size)
unique(paygap$employer_size)


## check for duplicates ----
### check for any duplicated observations ----
sum(duplicated(paygap))  # zero duplicate records

### how about duplicated employer_id? ----

nrow(paygap) / n_distinct(paygap$employer_id)  # observations ~ X4 unique ids

# visualise 
paygap %>% 
  count(employer_id) %>% 
  ggplot() +
  geom_histogram(aes(n), binwidth = 0.5)

# most employer ids appear to be associated with multiple records
#  - are they from different years?

# there are 2 different variables for dates (submitted and due)

#### explore 'date_submitted' variable ----
min(paygap$date_submitted)
max(paygap$date_submitted)

# convert to year and explore frequency distribution by employer id and year
paygap %>% 
  mutate(
    year = year(date_submitted) # use lubridate::year() to extract year
  ) %>% 
  group_by(employer_id, year) %>% 
  count() %>% 
  filter(n > 1) %>% 
  arrange(desc(n))

# shows that ~ 3400 id/yr combinations have > 1 observation
# explore using an example (employer id 132)

paygap %>% 
  filter(employer_id == 132) %>% 
  select(due_date, date_submitted, submitted_after_the_deadline)

# for this employer all previous years data submitted in sept/oct 2021
# unsure what 'submitted after the deadline' variable indicates as this is 
# false for each of these observations

# check another example sampled from data (employer id 16321)

paygap %>% 
  filter(employer_id == 16321) %>% 
  select(due_date, date_submitted, submitted_after_the_deadline)

# similar situation for 2021 and 2022 data submitted at the same time
# again submitted_after_the_deadline is FALSE

#### explore 'due_date' variable ----

min(paygap$due_date)
max(paygap$due_date)

paygap %>% 
  mutate(
    year = year(due_date)
  ) %>% 
  group_by(employer_id, year) %>% 
  count() %>% 
  filter(n > 1) %>% 
  arrange(desc(n))

# there are 4 companies who have 2 sets of data due in the same year
# are there any common features?
paygap %>%
  mutate( year = lubridate::year(due_date)) %>% 
  filter(employer_id %in% c(8295, 11744, 11766, 20048) & 
           year %in% c(2020, 2021)) %>% 
  select(current_name, employer_id, current_name, year, due_date, 
         date_submitted, submitted_after_the_deadline) %>% 
  arrange(current_name, year) %>% 
  view()

# 2 approaches
#   1.) filter for unique observation
#   2.) retain most recent submission

# remove duplicate entries by filtering for unique obs
paygap %>% 
  mutate(year = lubridate::year(due_date)) %>% 
  # remove variables that may differ between duplicates
  select(current_name, employer_id, year, everything(),
         -c(date_submitted, submitted_after_the_deadline)) %>% 
  # filter for employer id/year pairs where previous duplicated observations
  filter(employer_id %in% c(8295, 11744, 11766, 20048) & 
           year %in% c(2020, 2021)) %>% 
  # select unique
  unique() %>% 
  arrange(employer_id, year) %>% 
  view()

# this has removed duplicated entries for 3 employers but there is a possible 
# error for employer id 8295 who have 2 identical submissions in 2021 
# it appears data filed reporting 2 different employer_sizes for same year
# TO DO: explore source data for this company

#### date due vs submission date ----

# explore delays in submitting data
## numeric
submission_delays <- 
  paygap %>% 
  mutate(
    delay = date_submitted - due_date,
    delay = as.numeric(as.duration(delay), 'days'),
    late_submit = delay > 0) %>% 
  select(date_submitted, due_date, delay,
         late_submit, submitted_after_the_deadline)

summary(submission_delays$delay) 

## graphically
submission_delays$delay %>% hist() # r skewed distribution
(submission_delays$delay) %>% boxplot() # log transformed to aid visual

## interpretation:
# mean and median values consistent with no delays on average
# however there appear to be outliers in both directions which call into 
# question the validity of this data
# I am especially concerned about values submitted > 1 year before due_date
# how can they accurately reflect the year the data should relate to?

####  create working data set ----
# year = year due. Duplicate entries removed.

paygap_w <- 
  paygap %>% 
  mutate(year = lubridate::year(due_date)) %>% 
  # remove variable relating to date of submission
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
