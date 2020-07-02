# Load libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidymodels)
library(dplyr)
library(plotly)
library(RCurl)
library(rvest)
library(XML)

# Get MN Dept of Health data
mndh <- read_html('https://www.health.state.mn.us/diseases/coronavirus/situation.html#ageg1') %>%
    html_table()

# New deaths w/ age group
newDeaths <- as.data.frame(mndh[[3]])

# New death residence type
newDeathsResType <- as.data.frame(mndh[[4]])

# Testing data
testing <- as.data.frame(mndh[[5]])
testing <- testing %>%
    mutate(
        `Date reported to MDH` = lubridate::ymd(paste0('2020/', testing$`Date reported to MDH`)),
        `Completed tests reported from the MDH Public Health Lab (daily)` = as.numeric(gsub(",", "", `Completed tests reported from the MDH Public Health Lab (daily)`)),
        `Completed tests reported from external laboratories (daily)` = as.numeric(gsub(",", "", `Completed tests reported from external laboratories (daily)`)),
        `Total approximate number of completed tests` = as.numeric(gsub(",", "", `Total approximate number of completed tests`))
    )

# Positive cases by day
cases <- as.data.frame(mndh[[6]])
cases <- cases %>%
    mutate(
        `Specimen collection date` = lubridate::ymd(paste0('2020/', cases$`Specimen collection date`)),
        `Positive cases` = as.numeric(gsub(",", "", `Positive cases`)),
        `Cumulative positive cases` = as.numeric(gsub(",", "", `Cumulative positive cases`))
    )

# Deaths by day
deaths <- as.data.frame(mndh[[7]])
deaths <- deaths %>%
    mutate(
        `Date reported` = lubridate::ymd(paste0('2020/', deaths$`Date reported`)),
        `Total deaths` = as.numeric(gsub(",", "", `Total deaths`)),
        `Newly reported deaths (daily)` = as.numeric(gsub(",", "", `Newly reported deaths (daily)`))
    )

# Hospitalizations
hospital <- as.data.frame(mndh[[8]])
hospital <- hospital %>%
    mutate(
        `Date reported` = lubridate::ymd(paste0('2020/', hospital$`Date reported`)),
        `Hospitalized in ICU (daily)` = as.numeric(gsub(",", "", `Hospitalized in ICU (daily)`)),
        `Hospitalized, not in ICU (daily)` = as.numeric(gsub(",", "", `Hospitalized, not in ICU (daily)`)),
        `Total hospitalizations` = as.numeric(gsub(",", "", `Total hospitalizations`)),
        `Total ICU hospitalizations` = as.numeric(gsub(",", "", `Total ICU hospitalizations`))
    )

# Age breakout
ageGroups <- as.data.frame(mndh[[9]])
ageGroups <- ageGroups %>%
    mutate(
        `Number of Cases` = as.numeric(gsub(',','',`Number of Cases`)),
        fatalityRate = round((`Number of Deaths` / `Number of Cases`)*100, 2)
        )

# Gender cases
gender <- as.data.frame(mndh[[10]])

# Race breakout
race <- as.data.frame(mndh[[11]])
race <- race[c(1:8,10,11), ]
race <- race %>%
    mutate(
        `Number of Cases` = as.numeric(gsub(',','',`Number of Cases`)),
        `Number of Deaths` = as.numeric(gsub(',','',`Number of Deaths`)),
        fatalityRate = round((`Number of Deaths` / `Number of Cases`)*100, 2)
    )

# Case sources
source <- as.data.frame(mndh[[12]])
source$`Number of Cases` = as.numeric(gsub(",", "", source$`Number of Cases`))

# Counties
counties <- as.data.frame(mndh[[13]])
counties <- counties %>%
    mutate(
        Cases = as.numeric(gsub(",", "", Cases)),
        Deaths = as.numeric(gsub(",", "", Deaths))
    )

# Cases by residence tyoe
resType <- as.data.frame(mndh[[14]])
resType$`Number of Cases` = as.numeric(gsub(",", "", resType$`Number of Cases`))

# Join daily tables into one table
daily <- cases %>%
    left_join(deaths, by = c("Specimen collection date" = "Date reported")) %>%
    left_join(testing, by = c("Specimen collection date" = "Date reported to MDH")) %>%
    left_join(hospital, by = c("Specimen collection date" = "Date reported"))

# Rename date column
daily <- daily %>%
    rename('Date' = 'Specimen collection date') %>%
    mutate(
        dailyTests = `Completed tests reported from the MDH Public Health Lab (daily)` + `Completed tests reported from external laboratories (daily)`
    ) %>%
    mutate(
        cumPosTestRate = round((`Cumulative positive cases` / `Total approximate number of completed tests`)*100, 2),
        dailyPosTestRate = round((`Positive cases` / dailyTests)*100, 2)
    )

# Variable for daily changes
dailyChanges <- daily %>%
    mutate(
        caseChange = `Positive cases` - lag(`Positive cases`),
        deathChange = `Newly reported deaths (daily)` - lag(`Newly reported deaths (daily)`),
        nonICUChange = `Hospitalized, not in ICU (daily)` - lag(`Hospitalized, not in ICU (daily)`),
        icuChange = `Hospitalized in ICU (daily)` - lag(`Hospitalized in ICU (daily)`)
    ) %>%
    select(Date, caseChange, deathChange, nonICUChange, icuChange)

# Get today's data from daily
today <- daily %>% filter(max(Date, na.rm = T) == Date)

shinyServer(function(input, output) {
    ###########################################################################################
    ############################### Daily Totals ##############################################
    ###########################################################################################
    ##############
    ### VALUE BOX
    ##############
    # Total cases
    output$totalCases <- renderValueBox({
        valueBox(max(daily$`Cumulative positive cases`), "Total Cases")
    })
    
    # New cases
    output$newCases <- renderValueBox({
        valueBox(today$`Positive cases`, "New Cases")
    })
    
    # Total deaths
    output$totalDeaths <- renderValueBox({
        valueBox(max(daily$`Total deaths`, na.rm = T), "Total Deaths", color = "red")
    })
    
    # New deaths
    output$newDeaths <- renderValueBox({
        valueBox(today$`Newly reported deaths (daily)`, "New Deaths", color = "red")
    })
    
    # Fatality rate
    output$fatalityRate <- renderValueBox({
        # Define rate
        rate = round((max(daily$`Total deaths`, na.rm = T) / max(daily$`Cumulative positive cases`, na.rm = T))*100, 2)
        rate = paste0(rate, "%")
        valueBox(rate, "Fatality Rate", color = "red")
    })
    
    # Last updated date
    output$lastUpdated <- renderValueBox({
        valueBox(today$Date, "Last Updated")
    })
    
    # Trend of 2 weeks vs previous 2 weeks
    output$twoWkTrend <- renderValueBox({
        twoWkAvg <- daily %>%
            filter(Date >= today$Date - 14) %>%
            summarise(avgCases = mean(`Positive cases`)) %>%
            select(avgCases)
        
        prevTwoWkAvg <- daily %>%
            filter(Date >= today$Date - 28 & Date < today$Date - 14) %>%
            summarise(avgCases = mean(`Positive cases`)) %>%
            select(avgCases)
        
        # Calc difference
        change = round(twoWkAvg$avgCases / prevTwoWkAvg$avgCases, 2)
        
        # Determine pos/neg change
        change = case_when(
            change < 1 ~ paste0("-", (1-change)*100, "%"),
            T ~ paste0("+", (change-1)*100, "%")
        )
        
        # Render UI
        valueBox(change, "2 Week Avg vs Previous 2 Week Avg")
    })
    
    
    ############
    ### PLOTS
    ############
    # Cases by day
    output$dailyCases <- renderPlotly({
        ggplot(daily, aes(`Date`, `Positive cases`)) +
            geom_bar(stat = "identity", colour = "black", fill = "lightblue") +
            ggtitle("Positive Cases by Day") +
            labs(x = "Date", y = "Confirmed Cases") -> p
        ggplotly(p)
    })
    
    # Deaths by day
    output$dailyDeaths <- renderPlotly({
        ggplot(daily, aes(`Date`, `Newly reported deaths (daily)`)) +
            geom_bar(stat = "identity", colour = "black", fill = "red") +
            ggtitle("Deaths from COVID by Day") +
            labs(x = "Date", y = "Confirmed Deaths") -> p
        ggplotly(p)
    })
    
    # Hospitalizations (non-ICU)
    output$dailyNonICU <- renderPlotly({
        ggplot(daily, aes(`Date`, `Hospitalized, not in ICU (daily)`)) +
            geom_bar(stat = "identity", colour = "black", fill = "lightblue") +
            ggtitle("Daily Hospitalizations (Non-ICU)") +
            labs(x = "Date", y = "Hospitalized Patients (Non-ICU)") -> p
        ggplotly(p)
    })
    
    # Hospitalizations (ICU)
    output$dailyICU <- renderPlotly({
        ggplot(daily, aes(`Date`, `Hospitalized in ICU (daily)`)) +
            geom_bar(stat = "identity", colour = "black", fill = "red") +
            ggtitle("Daily Hospitalizations (ICU)") +
            labs(x = "Date", y = "Hospitalized Patients (ICU)") -> p
        ggplotly(p)
    })
    
    ###########################################################################################
    ###################################### Testing ############################################
    ###########################################################################################
    ##############
    ### VALUE BOX
    ##############
    # Today's postitive test rate
    output$dailyPosRate <- renderValueBox({
        valueBox(paste0(today$dailyPosTestRate, "%"), "Daily Positive Test Rate")
    })
    
    # Total tests today
    output$dailyTests <- renderValueBox({
        valueBox(today$dailyTests, "Tests Performed Today")
    })
    
    # Total tests for COVID performed
    output$totalTests <- renderValueBox({
        valueBox(today$`Total approximate number of completed tests`, "Total COVID-19 Tests")
    })
    
    
    ###########
    ### PLOTS
    ###########
    # Daily tests administered
    output$dailyTesting <- renderPlotly({
        ggplot(daily, aes(`Date`, dailyTests)) +
            geom_bar(stat = "identity", colour = "blue", fill = "lightblue") +
            ggtitle("Daily Number of Tests Administered") +
            labs(x = "Date", y = "Number of Tests") -> p
            ggplotly(p)
    })
    
    # Daily positive rate
    output$dailyPositiveTests <- renderPlotly({
        ggplot(daily, aes(`Date`, dailyPosTestRate)) +
            geom_bar(stat = "identity", colour = "blue", fill = "lightblue") +
            ggtitle("Daily Positive Test Rate") +
            labs(x = "Date", y = "Rate of Positive Tests") -> p
        ggplotly(p)
    })
    
    ###########################################################################################
    ###################################### Age Groups #########################################
    ###########################################################################################
    ##############
    ### VALUE BOX
    ##############
    
    
    ##############
    ### PLOTS
    ##############
    output$casesByAge <- renderPlotly({
        ggplot(ageGroups, aes(reorder(`Age Group`, `Number of Cases`), `Number of Cases`, fill = `Age Group`)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            ggtitle("Cases by Age Breakout") +
            labs(x = "Number of Cases", y = "Age Group") +
            theme(legend.position = "none") -> p
        ggplotly(p)
    })
    
})
