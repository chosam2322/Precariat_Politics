
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
library(broom)

#Dataset Ingestion
CCES_Merged <- read_csv(here("01_tidydata", "CCESPanelData_Merged.csv"))


#Dropset for Analysis
CCES_Merged <- CCES_Merged %>%
  mutate(
    age = coalesce(age, year - birth_year)
  )

analysis_data <- CCES_Merged %>%
  filter(
    !year %in% c(2006, 2007, 2010, 2011, 2015)
  ) %>%
  filter(
    !is.na(national_economy_raw),
    !is.na(personal_finance_raw),
    !is.na(churn_4q)
  )

table(analysis_data$national_economy_raw, useNA = "ifany")
table(analysis_data$personal_finance_raw, useNA = "ifany")

#Mean-Center Major Variables
analysis_data <- analysis_data %>%
  mutate(
    personal_c =
      personal_finance_raw -
      mean(personal_finance_raw, na.rm = TRUE),
    
    churn_c =
      churn_4q -
      mean(churn_4q, na.rm = TRUE)
  )

#Descriptive Visualizations 
yearly_corr <- analysis_data %>%
  filter(
    !is.na(national_economy_raw),
    !is.na(personal_finance_raw)
  ) %>%
  group_by(year) %>%
  summarize(
    corr = cor(
      national_economy_raw,
      personal_finance_raw,
      use = "complete.obs"
    ),
    n = n()
  )

#Plotting Over Time Shifts in Terms of the Relationship between Personal and National Evaluations
ggplot(yearly_corr,
       aes(year, corr)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Year",
    y = "Correlation",
    title = "Relationship Between Personal and National Economic Evaluations"
  ) +
  theme_minimal()

#Year-to-Year Slopes
yearly_slopes <- analysis_data %>%
  filter(
    !is.na(national_economy_raw),
    !is.na(personal_finance_raw)
  ) %>%
  group_by(year) %>%
  do(
    tidy(
      lm(
        national_economy_raw ~ personal_finance_raw,
        data = .
      )
    )
  ) %>%
  filter(term == "personal_finance_raw")


ggplot(
  yearly_slopes,
  aes(x = year, y = estimate)
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_line() +
  geom_point(size = 2) +
  geom_errorbar(
    aes(
      ymin = estimate - 1.96 * std.error,
      ymax = estimate + 1.96 * std.error
    ),
    width = 0.2
  ) +
  labs(
    title = "Effect of Personal Economic Evaluations on National Economic Evaluations",
    subtitle = "Separate OLS estimate for each CES wave",
    x = "Survey Year",
    y = "Coefficient on Personal Economy"
  ) +
  theme_minimal()

#
m_dynamic <- feols(
  national_economy_raw ~
    i(year, personal_finance_raw, ref = 2012),
  data = analysis_data
)

iplot(m_dynamic)

county_corr <- analysis_data %>%
  group_by(county_fips, year) %>%
  summarize(
    corr = cor(
      national_economy_raw,
      personal_finance_raw,
      use = "complete.obs"
    ),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 20)

ggplot(
  county_corr,
  aes(year, county_fips, fill = corr)
) +
  geom_tile()

analysis_data <- analysis_data %>%
  mutate(
    administration = case_when(
      year <= 2008 ~ "Bush",
      year <= 2016 ~ "Obama",
      year <= 2020 ~ "Trump",
      TRUE ~ "Biden"
    )
  )

feols(
  national_economy_raw ~
    personal_finance_raw * administration,
  data = analysis_data
)

year_corr <- analysis_data %>%
  filter(
    !is.na(national_economy_raw),
    !is.na(personal_finance_raw)
  ) %>%
  group_by(year) %>%
  summarize(
    corr = cor(
      national_economy_raw,
      personal_finance_raw,
      use = "complete.obs"
    ),
    n = n(),
    .groups = "drop"
  )

ggplot(year_corr,
       aes(x = year,
           y = corr)) +
  geom_line() +
  geom_point(size = 2) +
  geom_hline(
    yintercept = mean(year_corr$corr),
    linetype = "dashed"
  ) +
  labs(
    title = "Correlation Between Personal and National Economic Evaluations",
    subtitle = "CES respondents by survey year",
    x = "Year",
    y = "Correlation"
  ) +
  theme_minimal()

m_year <- feols(
  national_economy_raw ~
    i(year, personal_finance_raw),
  data = analysis_data
)

iplot(m_year)


#Co-Partisan Model
analysis_data <- analysis_data %>%
  mutate(
    party_id_7 = ifelse(
      party_id_7 %in% c(8,9),
      NA,
      party_id_7
    )
  )

analysis_pid <- analysis_data %>%
  filter(!party_id_7 %in% c(8, 9))

m_pid_drop <- feols(
  national_economy_raw ~
    personal_finance_raw *
    factor(party_id_7),
  data = analysis_pid
)

etable(m_pid_drop)

m_pid_controls <- feols(
  national_economy_raw ~
    personal_finance_raw * factor(party_id_7) +
    age + income + education,
  data = analysis_pid
)
etable(m_pid_controls)

library(marginaleffects)

sl <- slopes(
  m_pid_controls,
  variables = "personal_finance_raw",
  by = "party_id_7"
)

plot(sl)

m_year_2 <- feols(
  national_economy_raw ~ personal_finance_raw * factor(year),
  data = analysis_data
)
year_slopes <- slopes(
  m_year,
  variables = "personal_finance_raw",
  by = "year"
)

analysis_data <- analysis_data %>%
  mutate(
    regime = case_when(
      year <= 2007 ~ "pre_crisis",
      year >= 2008 & year <= 2011 ~ "crisis",
      year >= 2012 & year <= 2015 ~ "recovery",
      year >= 2016 & year <= 2019 ~ "politicized_growth",
      year >= 2020 ~ "pandemic_inflation"
    )
  )

m_regime <- feols(
  national_economy_raw ~ personal_finance_raw * regime,
  data = analysis_data
)

m_full <- feols(
  national_economy_raw ~
    personal_finance_raw * regime * churn_4q,
  data = analysis_data
)
etable(m_full)

slopes(m_full,
       variables = "personal_finance_raw",
       by = c("regime", "churn_4q_quartile"))

analysis_data <- analysis_data %>%
  mutate(
    churn_4q_quartile = ntile(churn_4q, 4)
  )

m_full <- feols(
  national_economy_raw ~
    personal_finance_raw * regime * churn_4q,
  data = analysis_data
)

sl <- slopes(
  m_full,
  variables = "personal_finance_raw",
  by = c("regime", "churn_4q_quartile")
)

sl
slopes(m_full,
       variables = "personal_finance_raw",
       by = c("regime", "churn_4q_quartile"))


m_full_controls <- feols(
  national_economy_raw ~
    personal_finance_raw * regime * churn_4q +
    age + income + education + gender + race + political_interest_raw +
    factor(party_id_7) + ideology |
    year + county_fips,
  data = analysis_data,
  cluster = "county_fips"
)
etable(m_full_controls)
#Naive Interaction Model
m1 <- feols(
  national_economy_raw ~
    personal_c * churn_c |
    county_fips + year,
  data = analysis_data,
  cluster = ~ county_fips
)

etable(m1)

county_year_corr <- analysis_data %>%
  group_by(county_fips, year) %>%
  summarize(
    corr_np =
      cor(
        national_economy_raw,
        personal_finance_raw,
        use = "complete.obs"
      ),
    churn_4q = first(churn_4q),
    .groups = "drop"
  )

m2 <- feols(
  corr_np ~
    churn_4q +
    med_income +
    poverty_rate +
    unemployment_rate +
    pct_bachelors_plus +
    industry_diversity |
    county_fips + year,
  data = county_year_corr,
  cluster = ~county_fips
)
etable(m2)


#Visualizations
# coefficient values from your model
b0 <- -0.0421

b_recovery <- 0.4190
b_pandemic <- 0.5135
b_politicized <- 0.6619

b_churn <- -2.09e-8  # basically ~0

# churn grid (use meaningful range)
churn_grid <- seq(-2, 2, length.out = 50)

surface <- expand_grid(
  churn = churn_grid,
  regime = c("baseline", "recovery", "pandemic", "politicized")
) %>%
  mutate(
    slope = case_when(
      regime == "baseline" ~ b0 + b_churn * churn,
      regime == "recovery" ~ (b0 + b_recovery) + b_churn * churn,
      regime == "pandemic" ~ (b0 + b_pandemic) + b_churn * churn,
      regime == "politicized" ~ (b0 + b_politicized) + b_churn * churn
    )
  )

ggplot(surface, aes(x = churn, y = slope, color = regime)) +
  geom_line(linewidth = 1.2) +
  theme_minimal() +
  labs(
    x = "Job churn (standardized)",
    y = "Implied slope: Personal finance → National economy",
    color = "Regime"
  )

ggplot(surface, aes(x = churn, y = regime, fill = slope)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(
    x = "Job churn",
    y = "Regime",
    fill = "Slope"
  )
