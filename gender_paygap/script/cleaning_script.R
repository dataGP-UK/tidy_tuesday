# tidy tuesday: Data on UK gender pay gap

# simplified cleaning script

# author: Simon Hulme
# organisation: dataGP-UK

# date created: 15th September 2022

library(tidyverse)
library(lubridate)
library(here)

# upload csv files from gov.uk website ----

years <- as.character(c(2017, 2018, 2019, 2020, 2021, 2022))
url <- "https://gender-pay-gap.service.gov.uk/viewing/download-data/"

download_2017 <- read_csv(paste0(url, years[1]))
download_2018 <- read_csv(paste0(url, years[2]))
download_2019 <- read_csv(paste0(url, years[3]))
download_2020 <- read_csv(paste0(url, years[4]))
download_2021 <- read_csv(paste0(url, years[5]))
download_2022 <- read_csv(paste0(url, years[6]))

paygap_raw <- 
    do.call("rbind", list(download_2017, download_2018, download_2019, 
                          download_2020, download_2021, download_2022))

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



rm(
    download_2017,
    download_2018,
    download_2019,
    download_2020,
    download_2021,
    download_2022,
    years,
    url
)
