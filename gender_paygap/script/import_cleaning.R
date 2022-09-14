# tidy tuesday: Data on UK gender pay gap

# author: Simon Hulme
# organisation: dataGP-UK

# date created: 12th September 2022

# setup ----

library(tidyverse)
library(lubridate)
library(finalfit)
library(naniar)
library(here)

paygap <- 
  read_csv(paste0(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master',
    '/data/2022/2022-06-28/paygap.csv'))

# save raw data to data folder
# write_csv(paygap, 'gender_paygap/data/raw/paygap_raw.csv')

# initial exploration ----

str(paygap)
head(paygap)
glimpse(paygap)
summary(paygap)

## preliminary feature selection ----
paygap <- 
  paygap %>% 
  # remove redundant variables that will not be used in analysis
  select(-c(address, company_number,
            company_link_to_gpg_info, responsible_person))

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

# POSIXct variables for date submitted and due date but no 'year' variables
# create as discrete numeric variable (integer)

paygap <- 
  paygap %>% 
  mutate(
    # use lubridate::year() to extract year
    year_due = as.integer(year(due_date)),
    year_submitted = as.integer(year(date_submitted))
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

# quality assessment ----

## uniqueness ----
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

#### explore using year_submitted ----
min(paygap$date_submitted)
max(paygap$date_submitted)

# convert to year and explore frequency distribution by employer id and year
paygap %>% 
  group_by(employer_id, year_submitted) %>% 
  count() %>% 
  filter(n > 1) %>% 
  arrange(desc(n))

# shows that ~ 3400 employer_id/yr combinations have > 1 observation
# explore using an example (employer id 132)

paygap %>% 
  filter(employer_id == 132) %>% 
  select(due_date, date_submitted, submitted_after_the_deadline)

# for this employer all previous years data submitted at once in sept/oct 2021
# unsure what 'submitted after the deadline' variable indicates as this is 
# false for each of these observations despite date submitted being later
# than due_date

# check another example sampled from data (employer id 16321)

paygap %>% 
  filter(employer_id == 16321) %>% 
  select(due_date, date_submitted, submitted_after_the_deadline)

# similar situation for 2021 and 2022 data submitted at the same time
# again submitted_after_the_deadline is FALSE

#### explore using year due ----

min(paygap$due_date)
max(paygap$due_date)

paygap %>% 
  group_by(employer_id, year_due) %>% 
  count() %>% 
  filter(n > 1) %>% 
  arrange(desc(n))

# there are 4 companies who have 2 sets of data due in the same year
# are there any common features?
duplicates <-
  paygap %>% 
  # remove variables that may differ between duplicates
  select(
    current_name,
    employer_id,
    year_due,
    everything(),-c(
      submitted_after_the_deadline,
      employer_name
    )
  ) %>%
  # filter for employer id/year pairs where previous duplicated observations
  filter(employer_id %in% c(8295, 11744, 11766, 20048) &
           year_due %in% c(2020, 2021)) %>%
  arrange(current_name, year_due)

duplicates

### managing duplicated data ----

## 2 approaches

## 1.) remove duplicate entries by filtering for unique obs
duplicates %>% 
  select(-date_submitted) %>% 
  # select unique
  unique() %>% 
  arrange(employer_id, year_due) %>% 
  select(current_name, due_date, year_due)

# this has removed duplicated entries for 3 employers but there is a possible 
# error for employer id 8295 who have 2 identical submissions in 2021 
# it appears data filed reporting 2 different employer_sizes for same year

## 2.) remove duplicate entries by retaining most recent observation
duplicates %>% 
  group_by(employer_id, year_due) %>% 
  arrange(desc(date_submitted)) %>% 
  select(current_name, date_submitted, year_due) %>% 
  slice(1)

# this handles all duplicated values by using the most up to data copy as the
# current data when there is a duplicate observation by employer id and year

# apply option 2 to paygap dataset so most recent data used
paygap <- 
  paygap %>% 
  group_by(employer_id, year_due) %>% 
  arrange(desc(date_submitted)) %>%
  slice(1) %>%
  ungroup()

rm(duplicates)

# timeliness ----

## explore delays in submitting data ----

## add variables including delay and whether submission late
paygap <- 
  paygap %>% 
  mutate(
    delay = date_submitted - due_date,
    delay = as.numeric(as.duration(delay), 'days'),
    late_submit = delay > 0) 

## numerically

summary(paygap$delay) 

## graphically
paygap$delay %>% hist() # approx normal distribution
paygap$delay %>% boxplot() # multiple high and low outliers

## interpretation

# mean and median values consistent with no delays on average
# but outliers call into question timeliness & validity of data

# timeliness: submissions after the due date (risk of inaccuracy)
# validity: submissions significantly before due_date (not possible)

# validity ----

## early submission of data ----

# create dataframe of early submissions
early_submissions <- 
  paygap %>% 
  filter(
    delay <= -366
  ) 

# view results
early_submissions %>% 
  select(current_name, date_submitted, due_date, delay) %>% 
  arrange(desc(delay)) %>% 
  head(20)

min(early_submissions$date_submitted)
max(early_submissions$date_submitted)

# years associated with early submissions
early_submissions$year_due %>% unique()         # only appears to be 2021
early_submissions$year_submitted %>% unique()   # only appears to be 2020

# TO DO: explore why data submitted in 2020 for 2021

# may need to remove values reported well in advance of due date if analysing
# time series data - ? use filtering join

### are there any other patterns ----

# no employers submitted data early multiple times
early_submissions %>% 
  group_by(current_name) %>% 
  count() %>% 
  filter(n > 1) %>% 
  nrow()  # 0

rm(early_submissions)

## numeric variables ----
## explore ranges and distribution

### bonus_percent ---- 

# ?any values < 0% or > 100%
paygap %>%
  select(male_bonus_percent, female_bonus_percent) %>%
 summary()

## may be an issue with max female paid bonus of 100.4% (>100%)

### percent_difference ----
paygap %>% 
  select(contains('diff')) %>% 
  summary()

## there are some very large percentage differences between males and females
## especially for mean and median bonus percent

### visualise distribution of these
paygap %>% 
  ggplot(aes(y = diff_mean_bonus_percent)) +
  geom_boxplot()

paygap %>% 
  ggplot(aes(y = diff_median_bonus_percent)) +
  geom_boxplot() 

### View data
paygap %>% 
  arrange(diff_mean_bonus_percent) %>% 
  head(20) %>% view

paygap %>% 
  arrange(diff_median_bonus_percent) %>% 
  head(20) %>% view

# these extremes may be possible values especially if bonuses are infrequent & 
# uncommon e.g. executive bonus 
# TO DO: may be an area to explore later

### hourly pay quartiles ----
paygap %>% 
  select(contains('quartile')) %>% 
  summary()

# these are percentages and all are valid ie. between 0-100%

# TO DO: check if male + female = 100% for each metric

# completeness ----

# missing data (explicit and implicit)

## explicit missing data ----

pct_miss(paygap)          # 2.0 % of all data missing
pct_miss_case(paygap)     # 27.0 % of all observations have some missing data
pct_complete_case(paygap) # 73.0 % of observations have complete data

# visualise missing data 

paygap %>% 
  gg_miss_var(show_pct = TRUE)

## variables containing missing data:
## diff bonus %, sic_codes, employer size, hourly rate quartiles, postcode

# explore most critical variables relating to metrics being analysed

### diff bonus percent ----

# largest amount of missing data is for % differences in bonuses 
# between males and females

## there are 8932 observations with missing date for diff bonus percent
## but no missing values for percent employees receiving bonus

## if either no males or no females paid a bonus then it will not be possible
## calculate a difference in bonus paid - therefore mean/median will = NA

## if both sexes paid a bonus then data is missing for some reason

## employers who don't pay a bonus to one or both sexes
paygap %>% 
  filter(female_bonus_percent == 0 | male_bonus_percent == 0) %>% 
  summarise(missing_mean = sum(is.na(diff_mean_bonus_percent)),
            missing_median = sum(is.na(diff_median_bonus_percent)))

# it appears that most of missing values relate employers where bonuses 
# are not paid to either (or both) males or females

# missing values where bonuses are paid to both sexes

paygap %>%
  # select observations where bonuses are paid to male and female but there
  # is still missing data for values of mean or median difference in bonus %
  filter((female_bonus_percent != 0 & male_bonus_percent != 0) &
           (is.na(diff_median_bonus_percent) | is.na(diff_mean_bonus_percent)
            )
         )%>%
  nrow() 

## there are 14 observations where data where males and females receive a bonus 
## and no value for mean and/or median difference available

## TO DO: decide how to handle missing data in these 14 observations

#### hourly pay quartile metrics ----

paygap %>% 
  select(contains('quartile')) %>%
  summary()

## it appears that 392 observations have missing data relating to hourly pay
## quartile data although there is no missing data in the diff hourly percent
## variables.

# are all missing values in same observations
paygap %>% 
  select(contains('quartile')) %>%
  # plot missingness by variable and observation
  missing_plot()

## it appears that data is missing across all quartiles for 392 observations
## ie. there is either data for all quartiles or for none

#### employer size ----

# explore whether missing data in other variables related to categories of 
# employer size (including where employer size is NA)

paygap %>% 
  gg_miss_var(show_pct = TRUE, facet = employer_size)

## it appears that as employer size increases the percent of missing data for
## all variables appears to decrease

## conversely the percentage of missing data for all variables appears to be 
## highest where employer size is unknown

### implicit missing data ----

## ie. are there years missing that don't show up with any values?

complete_paygap <-
  paygap %>%
  complete(employer_id, year_due)

summary(complete_paygap)

# there are many companies with data missing for years 
# explore if there are any patterns in this data

complete_paygap %>% 
  select(employer_id, year_due, current_name) %>% 
  group_by(year_due) %>% 
  summarise(missing_data = sum(is.na(current_name))) %>% 
  ggplot() +
  geom_col(aes(year_due, missing_data))

# i can understand why data incomplete for 2023 but unsure why other years
# especially spike in missing data for 2020

# what data is there for 2023 considering that this is next year?
paygap %>% 
  filter(year_due == 2023) %>% 
  nrow()

# 125 companies have submitted data for next year already


## accuracy ----

### submitted_after_the_deadline ----

# assess accuracy by comparing with late_submit which has been calculated from
# the dates provided

paygap %>% 
  xtabs(~ submitted_after_the_deadline + late_submit, data = .)

## accuracy = total correct / total observations
## ~ 92% accurate - I am unsure why these don't match 100%
## it could be a data collection issue
## plan: replace submitted_after_the_deadline with values from late_submiy

paygap <- 
  paygap %>% 
  mutate(
    submitted_after_the_deadline = late_submit
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

  