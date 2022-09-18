# tidy tuesday: Data on UK gender pay gap

# quality assessment

# author: Simon Hulme
# organisation: dataGP-UK

# date created: 12th September 2022

# setup ----

library(tidyverse)
library(lubridate)
library(finalfit)
library(naniar)
library(here)

## manage categorical variables ----

###  employer_size ----

# missing data currently labelled as "Not provided"
# this is ordinal data so ensure ordered factor created

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
unique(paygap$employer_size)

### year  ----

# what year is most useful for analysis
# data provided is for due date and date of submission
# guidance relates to 'snapshot date' which is 12 months prior to due date
# this is the date that all the data relates to so therefore this should be
# the 'year' that is added to the table

# POSIXct variables for date submitted and due date but no 'year' variables
# create as discrete numeric variable (integer)

paygap <- 
  paygap %>% 
  mutate(
    # use lubridate::year() to extract year
      year = as.integer(year(due_date - years(1)))
  ) 

### sex ----

# many of the quantitative variables have male and female versions
# transform data to long format with new categorical variables

#   - 'sex' to include 'male', 'female', and 'difference'
#   - 'metric' to include each of the reported metrics

paygap_long <-
  paygap %>%
  pivot_longer(
    data = .,
    # select columns that contain metrics
    cols = c(contains('Male') |
               contains('Female') | contains('diff')),
    names_to = 'metric',
    values_to = 'value'
  ) %>%
  # split data further by 'sex' - includes metrics relating to each sex
  # plus those that describe the difference in values between sexes
  separate(
    col = metric,
    sep = "_",
    into = c("sex", "metric"),
    extra = "merge"
  ) %>%
  # create factors from categorical variables
  mutate(
    sex = factor(
      sex,
      levels = c("male", "female", "diff"),
      labels = c("male", "female", "difference")
      ),
    metric = factor(metric)
    ) 


## consistency ----

# no concerns evident from initial exploration

# Finalise clean working data sets ----

## select features
paygap <- 
  paygap %>% 
  select(-c(late_submit, delay))


## review structure
str(paygap)

## save as working data set (Rdata)
saveRDS(paygap, 'gender_paygap/data/processed/paygap_clean.rda')

## create updated tidy long version

paygap_long <-
  paygap %>%
  pivot_longer(
    data = .,
    # select columns that contain metrics
    cols = c(contains('Male') |
               contains('Female') | contains('diff')),
    names_to = 'metric',
    values_to = 'value'
  ) %>%
  separate(
    col = metric,
    sep = "_",
    into = c("sex", "metric"),
    extra = "merge"
  ) %>%
  # create factors from categorical variables
  mutate(
    sex = factor(
      sex,
      levels = c("male", "female", "diff"),
      labels = c("male", "female", "difference")
    ),
    metric = factor(metric)
  ) 

## save as working data set (Rdata)
saveRDS(paygap_long, 'gender_paygap/data/processed/paygap_tidy.rda')




# investigate extreme values for bonus pay differences

# identify extreme negative outliers

Q3 <- as.numeric(summary(paygap$diff_median_bonus_percent)['3rd Qu.'])
Q1 <- as.numeric(summary(paygap$diff_median_bonus_percent)['1st Qu.'])
IQR <- Q3 - Q1

upper_limit <-  Q3 + 1.5 * IQR
lower_limit <- Q1 - 1.5 * IQR

paygap %>%
    filter(diff_median_bonus_percent < lower_limit)

outlying_bonus <- paygap %>%
    slice_min(diff_median_bonus_percent, n = 10) %>%
    group_by(employer_id) %>%
    count(sort = TRUE) %>%
    filter(n == 1) %>%
    ungroup() %>%
    left_join(paygap) %>% 
    select(employer_id, current_name, year, diff_median_bonus_percent)









  