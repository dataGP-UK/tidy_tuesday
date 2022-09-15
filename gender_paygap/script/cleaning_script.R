# tidy tuesday: Data on UK gender pay gap

# simplified cleaning script

# author: Simon Hulme
# organisation: dataGP-UK

# date created: 15th September 2022

library(tidyverse)
library(lubridate)
library(here)

# upload csv files from gov.uk website ----

# using for loops

## create vector of filepaths
years <- as.character(c(2017, 2018, 2019, 2020, 2021, 2022))
url <- "https://gender-pay-gap.service.gov.uk/viewing/download-data/"
filepath <- as.character()
for (i in years) {
    filepath[i] <- paste0(url, i)
}

## iterate over filepaths to create concatenated dataframe
paygap_raw <- tibble()
for (i in 1:length(filepath)) {
    new_data <- read_csv(filepath[i])
    paygap_raw <- rbind(paygap_raw, new_data)
}

## save raw data ----

write_csv(paygap_raw, "gender_paygap/data/raw/paygap_raw.csv")

# clean data ----

paygap <-
    paygap_raw %>%
    janitor::clean_names() %>%  
    mutate(
        across(c(due_date, date_submitted), as_datetime),
        employer_name = str_remove_all(employer_name, "\""),
        employer_name = str_replace_all(employer_name, ", |,", ", "),
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
        year_due = as.integer(year(due_date)),
        year_submitted = as.integer(year(date_submitted)),
        delay = date_submitted - due_date,
        delay = as.numeric(as.duration(delay), 'days'),
        submitted_after_the_deadline = delay > 0
    )

paygap <-
    paygap %>%
    select(-c(
        delay,
        address,
        company_number,
        company_link_to_gpg_info,
        responsible_person
    ))

paygap <-
    paygap %>%
    group_by(employer_id, year_due) %>%
    arrange(desc(date_submitted)) %>%
    slice(1) %>%
    ungroup()

## save clean data ----

write_csv(paygap, "gender_paygap/data/processed/paygap_clean.csv")
saveRDS(paygap, "gender_paygap/data/processed/paygap_clean.rda")


# tidy data ----

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


rm(new_data, filepath, i, url, years)
