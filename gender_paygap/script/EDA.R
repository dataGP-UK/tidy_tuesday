# tidy tuesday: Data on UK gender pay gap

# author: Simon Hulme
# organisation: dataGP-UK

# date created: 14th September 2022

# setup ----

library(tidyverse)
library(finalfit)
library(here)

paygap_long <- readRDS(file = "gender_paygap/data/processed/paygap_tidy.rds")
paygap <- readRDS(file = "gender_paygap/data/processed/paygap_clean.rds")

# univariate analysis ----

## categorical ----

##  employer size and year due
paygap %>% 
  group_by(employer_size) %>% 
  count()

paygap %>% 
  group_by(employer_size) %>% 
  count %>% 
  ggplot() +
  geom_col(aes(x = employer_size, y = n))

### most frequent employer size is 250-499. Increasing employer size assoc with
### decreasing frequency

paygap %>% 
  group_by(year_due) %>% 
  count()

paygap %>% 
  group_by(year_due) %>% 
  count %>% 
  ggplot() +
  geom_col(aes(x = year_due, y = n)) 

### similar number of records for each year at around 10230-10533 
### there is a dip in 2020 - could this be pandemic related?
### there are 125 entries for 2023 which may not be valid

## continuous ----

### diff_hourly_percent ----

paygap_long %>% 
  filter(
    sex == 'difference' &
      metric %in% c('median_hourly_percent', 'mean_hourly_percent')
  ) %>%
  group_by(metric) %>% 
  summarise(
    min = min(value),
    median = median(value),
    mean = mean(value),
    max = max(value),
    sd = sd(value))

paygap_long %>% 
  filter(
    sex == 'difference' &
      metric %in% c('median_hourly_percent', 'mean_hourly_percent')
  ) %>% 
  ggplot() +
  geom_histogram(aes(value), binwidth = 5) +
  facet_wrap(~ metric, ncol = 1)

paygap_long %>%
  filter(
    sex == 'difference' &
      metric %in% c('median_hourly_percent', 'mean_hourly_percent')
  ) %>% 
  ggplot() +
  geom_boxplot(aes(x = metric, y = value))

## in view of extreme outliers then median may be most appropriate method to 
## measure the location of centre REVIEW this

### diff bonus percent ----

paygap_long %>% 
  drop_na(value) %>%  
  filter(
    sex == 'difference' &
      metric %in% c('median_bonus_percent', 'mean_bonus_percent')
  ) %>%
  group_by(metric) %>% 
  summarise(
    min = min(value),
    median = median(value),
    mean = mean(value),
    max = max(value),
    sd = sd(value))

paygap_long %>% 
  drop_na(value) %>%  
  filter(
    sex == 'difference' &
      metric %in% c('median_bonus_percent', 'mean_bonus_percent')
  ) %>%
  ggplot() +
  geom_histogram(aes(value), binwidth = 1000) +
  facet_wrap(~ metric, ncol = 1)

paygap_long %>% 
  drop_na(value) %>%  
  filter(
    sex == 'difference' &
      metric %in% c('median_bonus_percent', 'mean_bonus_percent')
  ) %>%
  ggplot() +
  geom_boxplot(aes(x = metric, y = value))

## difficult to visualise distribution due to extreme values

### quartile for hourly pay ----

# numerically
paygap_long %>% 
  drop_na(value) %>% 
  filter(
    sex %in% c('male', 'female') &
      metric != 'bonus_percent') %>%
  group_by(metric, sex) %>% 
  summarise(
    min = min(value),
    median = median(value),
    mean = mean(value),
    max = max(value),
    sd = sd(value))

# graphically

# produced 8 histograms from 1 block of code to explore distribution of % in 
# each quartile of men and women
paygap_long %>%
  drop_na(value) %>%
  filter(sex %in% c('male', 'female') &
           metric != 'bonus_percent') %>%
  mutate(metric = factor(
    metric,
    ordered = TRUE,
    levels = c(
      'lower_quartile',
      'lower_middle_quartile',
      'upper_middle_quartile',
      'top_quartile'
    ),
    labels = c('lower', 'lower_mid', 'upper_mid', 'top')
  )) %>% 
ggplot() +
  geom_histogram(aes(value, y = ..density..), binwidth = 10) +
  facet_grid(rows = vars(sex), cols = vars(metric))

### bonus percent ----

paygap_long %>% 
  drop_na(value) %>% 
  filter(
    sex %in% c('male', 'female') &
      metric == 'bonus_percent') %>%
  group_by(metric, sex) %>% 
  summarise(
    min = min(value),
    median = median(value),
    mean = mean(value),
    max = max(value),
    sd = sd(value))

paygap_long %>% 
  drop_na(value) %>% 
  filter(
    sex %in% c('male', 'female') &
      metric == 'bonus_percent') %>% 
  ggplot() +
  geom_histogram(aes(value), binwidth = 5) +
  facet_wrap(~ sex, ncol = 1)

## bivariate analyses ----

### categorical vs categorical

## employer size vs year_due

### cross tabulation
paygap %>% 
  xtabs(~ employer_size + year_due, data = .)

### not particularly helpful as total number of observations varies by year
### will need prop table

year_size <- 
  paygap %>% 
  xtabs(~ employer_size + year_due, data = .) %>% 
  prop.table(margin = 2) %>% 
  round(2)

year_size

### use this table to visualise distribution of employer size over time
class(year_size)        # matrix
attributes(year_size)   # explore matrix attributes

### will need to be a dataframe for visualisation

year_size <-
  as_tibble(year_size) %>%
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
           ),
         year_due = as.integer(year_due)) 

year_size %>%
  ggplot() +
  geom_col(aes(
    x = year_due,
    y = n,
    fill = fct_rev(employer_size)), 
    position = 'fill')+
  scale_fill_discrete()

year_size %>% 
  ggplot(aes(x = year_due, y = n, color = employer_size)) +
  geom_point() +
  geom_line() +
  scale_color_discrete()
  

## ended up with bivariate analysis

paygap_long %>%
  drop_na(value) %>%
  filter(sex %in% c('male', 'female') &
           metric != 'bonus_percent') %>%
  group_by(metric, sex) %>%
  summarise(median = median(value)) %>%
  mutate(quartile = factor(
    metric,
    ordered = TRUE,
    levels = c(
      'lower_quartile',
      'lower_middle_quartile',
      'upper_middle_quartile',
      'top_quartile'
    ),
    labels = c('lower', 'lower_mid', 'upper_mid', 'top')
  )) %>% 
  ggplot() +
  geom_col(aes(x = quartile, y = median, fill = sex), position = 'stack')



