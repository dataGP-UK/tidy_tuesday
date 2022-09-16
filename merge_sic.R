# tidy tuesday: Data on UK gender pay gap

# merging paygap data with SIC codes from companies house

# author: Simon Hulme
# organisation: dataGP-UK

# date created: 15th September 2022

library(tidyverse)

# import SIC data (downloaded from companies house website)
SIC <- read_csv("gender_paygap/data/raw/SIC.csv")

# import cleaned version of paygap dataset
paygap <- read_csv("gender_paygap/data/processed/paygap_clean.csv")

# aim to create a table that can be queried then merged with paygap data
# using a filtering join. 
# adding this data to paygap will --> large ++ file due to some employers
# having multiple SIC codes

# create a dataframe (df) containing employer details and sic codes and
# separate sic codes

# can I use 1 observation per employer id or do sic_codes vary by year?

# separate data by row due to number of sic codes per employer varying

## no grouping (allowing for year on year variation)
paygap %>% 
    select(employer_id, current_name, sic_codes) %>% 
    separate_rows(sic_codes, sep = ",\n") %>% 
    n_distinct()  # 19303 observations

## grouped by employer_id
paygap %>%
    select(employer_id, current_name, sic_codes) %>%
    group_by(employer_id) %>%
    slice(1) %>% 
    ungroup() %>% 
    separate_rows(sic_codes, sep = ",\n") %>% 
    n_distinct()  # 18248 observations

## grouping would result in a loss of information where employers SIC code
## changed over time

## create working dataframe
df <- paygap %>%
    # select unique identifier as key variable plus others of interest
    select(employer_id, current_name, year_due, sic_codes) %>%
    # separate sic codes into multiple rows 
    separate_rows(sic_codes, sep = ",\n") %>% 
    # correct format of sic_codes so all are 5 digits by adding 0s to left 
    mutate(sic_codes = str_pad(sic_codes, 5, side = 'left', pad = '0')) %>% 
    # join with SIC code data
    left_join(SIC, by = c('sic_codes' = 'SIC Code'))

## explore number of codes per company ----
df %>% 
    group_by(employer_id, year_due) %>% 
    count(sort = TRUE) %>% 
    ungroup() %>% 
    ggplot(aes(x = n)) +
    geom_bar()

df %>% 
    group_by(employer_id, year_due) %>% 
    count(sort = TRUE) %>% 
    ungroup() %>% 
    summarise(mean = mean(n),
              median = median(n),
              max = max(n))

## explore missing descriptions ----

# filter for sic code present but description missing
df %>% 
    filter(is.na(Description) & !is.na(sic_codes)) %>% 
    count(sic_codes, sort = TRUE) %>% 
    ggplot(aes(x = fct_reorder(sic_codes, n), y = n)) +
    geom_col() +
    coord_flip()

# NAs for description are when sic code == 1 (does not exist)
# explore documentation to find out why this code used

# explore if other sic codes also submitted as well as 1 to assess whether
# these observations can be dropped from the dataset

# employer ids where sic code 1 submitted
code1_id <- 
    df %>% 
    filter(sic_codes == '00001') %>% 
    pull(employer_id) %>% 
    unique()

df %>% 
    filter(employer_id %in% code1_id) %>% 
    group_by(employer_id, year_due) %>% 
    count(sort = TRUE) %>% 
    ungroup() %>% 
    filter(n == 1)

# in 402 cases SIC == 1 is the only code entered

# can an appropriate code and description be imputed in these cases


  df %>% 
    mutate(
        sic_codes = na_if(sic_codes, '00001')) %>% view

by_row %>% 
    filter(employer_id %in% sic_1) %>% 
    group_by(employer_id) %>% 
    count(sort = TRUE) %>% 
    ungroup() %>% 
    ggplot(aes(x = n)) +
    geom_bar()



### correct 4 digit entries ----


df %>%
    filter(is.na(Description) &
               !is.na(sic_codes) & 
                    !employer_id %in% code1_id) %>% 
    mutate(
        sic_codes = str_pad(sic_codes, 5, side = 'left', pad = '0')
    ) %>% 
    select(-Description) %>% 
    left_join(SIC, by = c('sic_codes' = 'SIC Code'))

### use original data




df2 <- paygap %>%
    select(employer_id, current_name, sic_codes) %>%
    group_by(employer_id) %>%
    slice(1) %>% 
    ungroup() %>% 
    separate_rows(sic_codes, sep = ",\n") %>%
        mutate(sic_codes = if_else(
        !employer_id %in% code1_id,
        str_pad(sic_codes, 5, side = 'left', pad = '0'),
        sic_codes
    )) %>% 
    left_join(SIC, by = c('sic_codes' = 'SIC Code'))

### post mutate checks 
df2 %>% 
    filter(is.na(Description) & !is.na(sic_codes)) %>% 
    count(sic_codes, sort = TRUE) %>% 
    ggplot(aes(x = fct_reorder(sic_codes, n), y = n)) +
    geom_col() +
    coord_flip()





