library(tidyverse)

download_paygap <- 
    function() {
        years <- as.character(c(2017, 2018, 2019, 2020, 2021, 2022))
        url <- "https://gender-pay-gap.service.gov.uk/viewing/download-data/"
        filepath <- as.character()
        paygap_raw <- tibble()
        
        for (i in years) {
            filepath[i] <- paste0(url, i)
        }
        
        for (i in 1:length(filepath)) {
            new_data <- read_csv(filepath[i])
            paygap_raw <- rbind(paygap_raw, new_data)
        }
        
        paygap_raw
    }