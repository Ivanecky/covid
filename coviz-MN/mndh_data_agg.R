# Script to aggregate & maintain the data
library(tidymodels)
library(dplyr)
library(plotly)
library(RCurl)
library(rvest)
library(XML)
library(janitor)

# Get MN Dept of Health data
mndh <- read_html('https://www.health.state.mn.us/diseases/coronavirus/situation.html#ageg1') %>%
  html_table()

##################
### EXTRACT DATA
##################
# Total positive cases - summary
total_cases_sum <- as.data.frame(mndh[[1]])

# New cases
new_cases_sum <- as.data.frame(mndh[[2]])

# County summary data
county_new_summary <- as.data.frame(mndh[[3]])

# New reported deaths (single value)
new_deaths_val <- as.data.frame(mndh[[4]])

# Newly reported deaths
new_deaths_tbl <- as.data.frame(mndh[[5]])

# Newly reported deaths - residence type
new_deaths_res_type <- as.data.frame(mndh[[6]])

# Cumulative completed test
cum_comp_test <- as.data.frame(mndh[[7]])

# Completed tests daily
comp_test_daily <- as.data.frame(mndh[[8]])

# Confirmed cases - test type
conf_cases_test_type <- as.data.frame(mndh[[11]])

# Patients no longer needing isolation
patients_no_iso <- as.data.frame(mndh[[13]])

# Total deaths
deaths_total <- as.data.frame(mndh[[14]])

# Deaths by day
deaths_daily <- as.data.frame(mndh[[15]])

# Total hospitalizations
cum_hosp <- as.data.frame(mndh[[17]])

# Daily hospitalizations
hosp_daily <- as.data.frame(mndh[[18]])

# Death age table
death_age_tbl <- as.data.frame(mndh[[19]])

# Cases by gender
cases_gender <- as.data.frame(mndh[[20]])

# Cases &d deaths by race
race_sum <- as.data.frame(mndh[[21]])

# Cases by exposure
cases_exposure <- as.data.frame(mndh[[22]])

# County summary
county_summary <- as.data.frame(mndh[[23]])

# Cases by residence type
cases_res_type <- as.data.frame(mndh[[24]])

##################
### CLEAN DATA
##################
### FUNCTIONS
# Function for convert cases numbers
convComma <- function(values) {
  return(as.numeric(gsub(",", "", values)))
}

handleTranspose <- function(df) {
  df <- t(df)
  colnames(df) <- df[1,]
  df <- as.data.frame(df)
  df <- df[2:nrow(df),]
  return(df)
}

# Cases Exposure
names(cases_exposure) <- c("exposure", "cases")

cases_exposure <- cases_exposure %>%
  mutate(
    cases = convComma(cases)
  )

# Cases Gender
names(cases_gender) <- c("gender", "cases")

cases_gender <- cases_gender %>%
  mutate(
    cases = convComma(cases)
  )

# Cases Res Type
names(cases_res_type) <- c("residence_type", "cases")

cases_res_type <- cases_res_type %>%
  mutate(
    cases = convComma(cases)
  )

# Comp Test Daily
names(comp_test_daily) <- c("rpt_d", "pcr_mdh_lab", "pcr_ext_lab", "pcr_total", "antg_ext_lab", "antg_total", "total_tests")

comp_test_daily <- comp_test_daily %>%
  mutate(
    rpt_d = lubridate::mdy(gsub("/", "-", paste0(rpt_d, "-20"))),
    pcr_mdh_lab = convComma(pcr_mdh_lab),
    pcr_ext_lab = convComma(pcr_ext_lab),
    antg_ext_lab = case_when(
      antg_ext_lab == "-" ~ 0,
      T ~ convComma(antg_ext_lab)
    ),
    antg_total = case_when(
      antg_total == "-" ~ 0,
      T ~ convComma(antg_total)
    ),
    total_tests = convComma(total_tests)
  )

# Conf Cases Test Type
names(conf_cases_test_type) <- c("rpt_d", "cases_pcr", "cases_total", "prob_cases_antg", "prob_cases_total", "total_pos_cases")

conf_cases_test_type <- conf_cases_test_type %>%
  mutate(
    rpt_d = lubridate::mdy(gsub("/", "-", paste0(rpt_d, "-20"))),
    cases_pcr = convComma(cases_pcr),
    cases_total = convComma(cases_total),
    prob_cases_antg = case_when(
      prob_cases_antg == "-" ~ 0,
      T ~ convComma(prob_cases_antg)
    ),
    prob_cases_total = case_when(
      prob_cases_total == "-" ~ 0,
      T ~ convComma(prob_cases_total)
    ),
    total_pos_cases = convComma(total_pos_cases)
  )

# County new summary
names(county_new_summary) <- c("county", "new_conf_cases", "new_prob_cases")

county_new_summary <- county_new_summary %>%
  mutate(
    new_conf_cases = convComma(new_conf_cases),
    new_prob_cases = convComma(new_prob_cases)
  )

# County summary
names(county_summary) <-c("county", "cases_conf", "cases_prob", "cases_total", "deaths_total")

county_summary <- county_summary %>%
  mutate(
    cases_conf = convComma(cases_conf),
    cases_prob = convComma(cases_prob),
    cases_total = convComma(cases_total),
    deaths_total = convComma(deaths_total)
  )

# Cumulative completed tests
cum_comp_test <- handleTranspose(cum_comp_test)

names(cum_comp_test) <- c("tests_total", "tests_pcr", "tests_antg")

cum_comp_test <- cum_comp_test %>%
  mutate(
    tests_total = convComma(tests_total),
    tests_pcr = convComma(tests_pcr),
    tests_antg = convComma(tests_antg)
  )

# Cumulative hospitalizations
cum_hosp <- handleTranspose(cum_hosp)

names(cum_hosp) <- c("total_hosp", "total_icu")

cum_hosp <- cum_hosp %>%
  mutate(
    total_hosp = convComma(total_hosp),
    total_icu = convComma(total_icu)
  )

# Deaths Age Tbl
names(death_age_tbl) <- c("age_group", "cases", "deaths")

death_age_tbl <- death_age_tbl %>%
  mutate(
    cases = convComma(cases),
    deaths = convComma(deaths)
  )

# Deaths Daily
names(deaths_daily) <- c("rpt_d", "deaths_new", "deaths_total")

deaths_daily <- deaths_daily %>%
  mutate(
    rpt_d = lubridate::mdy(gsub("/", "-", paste0(rpt_d, "-20"))),
    deaths_new = convComma(deaths_new),
    deaths_total = convComma(deaths_total)
  )

# Deaths Total
deaths_total <- handleTranspose(deaths_total)

names(deaths_total) <- c("deaths_total", "deaths_conf", "deaths_prob", "deaths_long_term")

deaths_total <- deaths_total %>%
  mutate(
    deaths_total = convComma(deaths_total),
    deaths_conf = convComma(deaths_conf),
    deaths_prob = convComma(deaths_prob),
    deaths_long_term = convComma(deaths_long_term)
  )

# Hospitalizations Daily
names(hosp_daily) <- c("rpt_d", "cases_icu", "cases_hosp", "total_hosp", "total_icu")

hosp_daily <- hosp_daily %>%
  mutate(
    rpt_d = lubridate::mdy(gsub("/", "-", paste0(rpt_d, "-20"))),
    cases_icu = convComma(cases_icu),
    cases_hosp = convComma(cases_hosp),
    total_hosp = convComma(total_hosp),
    total_icu = convComma(total_icu)
  )

# New Cases Total
new_cases_sum <- handleTranspose(new_cases_sum)

names(new_cases_sum) <- c("cases_new", "cases_conf", "cases_prob")

new_cases_sum <- new_cases_sum %>%
  mutate(
    cases_new = convComma(cases_new),
    cases_conf = convComma(cases_conf),
    cases_prob = convComma(cases_prob)
  )

# New Deaths Residence Type
names(new_deaths_res_type) <- c("residence_type", "deaths")

new_deaths_res_type <- new_deaths_res_type %>%
  mutate(
    deaths = convComma(deaths)
  )

# New Deaths Table
names(new_deaths_tbl) <- c("county", "age_group", "deaths")

new_deaths_tbl <- new_deaths_tbl %>%
  mutate(
    deaths = convComma(deaths)
  )

# Patients No Isolation
patients_no_iso <- patients_no_iso$X2
patients_no_iso <- convComma(patients_no_iso)

# Race Summary
names(race_sum) <- c("race", "cases", "deaths")

race_sum <- race_sum %>%
  mutate(
    cases = convComma(cases),
    deaths = convComma(deaths)
  )
