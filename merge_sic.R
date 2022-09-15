# tidy tuesday: Data on UK gender pay gap

# merging paygap data with SIC codes from companies house

# author: Simon Hulme
# organisation: dataGP-UK

# date created: 15th September 2022

library(tidyverse)

SIC <- read_csv("gender_paygap/data/raw/SIC.csv")

str(SIC)
head(SIC)

# use paygap CSV (clean dataset)

# aim to create a table that can be searched on then merged with paygap data
# using a filtering join

paygap <- read_csv("gender_paygap/data/processed/paygap_clean.csv")

test <- paygap %>%
    select(employer_id, current_name, sic_codes) %>%
    group_by(employer_id) %>%
    slice(1) %>% 
    ungroup()

# separate into rows

 by_row <- test %>%
    separate_rows(sic_codes, sep = ",\n") %>%
    left_join(SIC, by = c('sic_codes' = 'SIC Code'))

# explore missing values
 
missing_SIC <- 
    by_row %>% 
    filter(is.na(Description)) %>% 
    .$employer_id %>% 
    unique()
 
missing_description <- 
    by_row %>% 
    filter(employer_id %in% missing_SIC)
 