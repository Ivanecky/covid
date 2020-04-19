# Libraries
library(ggplot2)
library(dplyr)
library(mongolite)
library(lubridate)

# Load data
us_c = read.csv("~/Github/COVID/covid-19-data/us-counties.csv")
states = read.csv("~/Github/COVID/covid-19-data/us-states.csv")

# Reformat data
us_c = us_c %>%
  mutate(
    date = lubridate::ymd(date)
  )

states = states %>%
  mutate(
    date = lubridate::ymd(date),
    death_rate = round(deaths / cases, 2)
  )

# Write to MongoDB table
# Counties
my_collection = mongo(collection = "covid", db = "counties")
my_collection$insert(us_c)
# States
my_collection = mongo(collection = "covid", db = "states")
my_collection$insert(states)