library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidymodels)
library(dplyr)
library(plotly)

# Load data
counties = read.csv("~/Github/COVID/covid-19-data/us-counties.csv")
states = read.csv("~/Github/COVID/covid-19-data/us-states.csv")

# Reassign date values
counties$date = lubridate::ymd(counties$date)
states$date = lubridate::ymd(states$date)

# Growth Rate var
counties = counties %>%
    group_by(state, county, fips, date) %>%
    arrange(date) %>%
    summarise(
        cases = sum(cases),
        deaths = sum(deaths),
        death_rate = round((deaths / cases)*100, 2)
    )

states = states %>%
    group_by(state, fips, date) %>%
    arrange(date) %>%
    summarise(
        cases = sum(cases),
        deaths = sum(deaths),
        death_rate = round((deaths / cases)*100, 2)
    )

shinyServer(function(input, output) {
    
    # Function to filter data
    state.df = eventReactive(input$updateData, {
        state.df = states %>% filter(state == input$stateName)
        state.df
    })
    
    # State plot
    output$state_plot = renderPlotly({
        df = state.df()
        # Create plot
        p = ggplot(df, aes(date)) +
            geom_point(aes(y = cases, colour = "cases")) +
            geom_line(aes(y = cases, colour = "cases")) +
            geom_point(aes(y = deaths, colour = "deaths")) +
            geom_line(aes(y = deaths, colour = "deaths")) +
            ggtitle("Cases vs Deaths in State") +
            labs(x = "Date", y = "Count")
        
        ggplotly(p)
    })
    
    # Value Box Outputs
    output$numConfirmed = renderValueBox({
        df = state.df()
        valueBox(max(df$cases), 
                 "Confirmed", color = "yellow")
    })
    
    output$numDied = renderValueBox({
        df = state.df()
        valueBox(max(df$deaths), 
                 "Deaths", color = "orange")
    })
    
    output$deathRate = renderValueBox({
        df = state.df()
        valueBox(paste0(round(((max(df$deaths) / max(df$cases))*100), 2),"%"), 
                 "Death Rate", color = "red")
    })

})
