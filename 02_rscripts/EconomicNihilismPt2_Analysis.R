
#Library Installation
library(devtools)
library(tidyverse)
library(lubridate)
library(readr)
library(janitor)
library(here)
library(haven)
library(tigris)
library(tidycensus)
library(purrr)
library(ipumsr)
library(data.table)
library(stringr)
library(httr2)
library(jsonlite)
library(dplyr)
library(slider)
library(fixest)

#Dataset Ingestion
CCES_Merged <- read_csv(here("01_tidydata", "CCESPanelData_Merged.csv"))


#Dropset for Analysis
analysis_model <- CCES_Merged %>%
  filter(
    !year %in% c(2006, 2007, 2010, 2011, 2015)
  ) %>%
  filter(
    !is.na(national_economy_raw),
    !is.na(personal_finance_raw),
    !is.na(churn_4q)
  )

table(analysis_model$national_economy_raw, useNA = "ifany")
table(analysis_model$personal_finance_raw, useNA = "ifany")

#Mean-Center Major Variables
analysis_model <- analysis_model %>%
  mutate(
    personal_c =
      personal_finance_raw -
      mean(personal_finance_raw, na.rm = TRUE),
    
    churn_c =
      churn_4q -
      mean(churn_4q, na.rm = TRUE)
  )

#Naive Interaction Model
m1 <- feols(
  national_economy_raw ~
    personal_c * churn_c |
    county_fips + year,
  data = analysis_model,
  cluster = ~ county_fips
)

etable(m1)
