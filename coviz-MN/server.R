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

source("mndh_data_agg.R")

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
