# tidy tuesday: Data on UK gender pay gap

# simplified cleaning script

# author: Simon Hulme
# organisation: dataGP-UK

# date created: 15th September 2022

library(tidyverse)
library(lubridate)

## maybe access files directly from gov.uk website then use
## cleaning script from tidytuesday prior to my cleaning
## for now work with static version

paygap <- 
  read_csv(paste0(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master',
    '/data/2022/2022-06-28/paygap.csv'))

paygap <-
  paygap %>%
  mutate(
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
    submitted_after_the_deadline = delay > 0)
  
paygap <- 
  paygap %>% 
  select(-c(delay, address, company_number,
            company_link_to_gpg_info, responsible_person))

paygap <- 
  paygap %>% 
  group_by(employer_id, year_due) %>% 
  arrange(desc(date_submitted)) %>%
  slice(1) %>%
  ungroup()

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
  mutate(
    sex = factor(
      sex,
      levels = c("male", "female", "diff"),
      labels = c("male", "female", "difference")
    ),
    metric = factor(metric)
  ) 

## save as working data sets (Rdata)

saveRDS(paygap, 'gender_paygap/data/processed/paygap_clean.rda')
saveRDS(paygap_long, 'gender_paygap/data/processed/paygap_tidy.rda')



