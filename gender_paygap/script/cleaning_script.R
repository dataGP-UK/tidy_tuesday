# tidy tuesday: Data on UK gender pay gap

# simplified cleaning script

# author: Simon Hulme
# organisation: dataGP-UK

# date created: 15th September 2022

library(tidyverse)
library(lubridate)
library(here)

# upload csv files from gov.uk website ----

# using a function
source("gender_paygap/script/functions/download_paygap.R")
paygap_raw <- download_paygap()
rm(download_paygap)

## save raw data ----

write_csv(paygap_raw, "gender_paygap/data/raw/paygap_raw.csv")

# clean data ----

paygap <-
    paygap_raw %>%
    # create consistent clean variable names
    janitor::clean_names() %>%  
    mutate(
        # ensure dates in correct format
        across(c(due_date, date_submitted), as_datetime),
        # tidy up employer names
        employer_name = str_remove_all(employer_name, "\""),
        employer_name = str_replace_all(employer_name, ", |,", ", "),
        # convert employer_size into ordered factor with NAs
        employer_size = na_if(employer_size, 'Not Provided'),
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
            ),
        # year that data relates to ('snapshot' date)
        year = as.integer(year(due_date - years(1))),
        # calculate if submission late
        delay = date_submitted - due_date,
        delay = as.numeric(as.duration(delay), 'days'),
        late_submission = delay > 0
    ) %>% 
    # remove duplicates where data resubmitted for same year
    group_by(employer_id, year) %>%
    arrange(desc(date_submitted)) %>%
    slice(1) %>%
    ungroup()

## save clean data ----

write_csv(paygap, "gender_paygap/data/processed/paygap_clean.csv")
saveRDS(paygap, "gender_paygap/data/processed/paygap_clean.rda")


# tidy data ----

# convert into long format
paygap_long <-
    paygap %>%
    pivot_longer(
        data = .,
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
    mutate(sex = factor(
        sex,
        levels = c("male", "female", "diff"),
        labels = c("male", "female", "difference")
    ),
    metric = factor(metric))

## save tidy data ----

write_csv(paygap_long, "gender_paygap/data/processed/paygap_tidy.csv")
saveRDS(paygap_long, 'gender_paygap/data/processed/paygap_tidy.rda')
