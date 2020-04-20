library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidymodels)
library(dplyr)
library(plotly)
library(RCurl)

# Read COVID data from URL links
# Counties
county.link = getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
counties = read.csv(text = county.link)
# States
state.link = getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
states = read.csv(text = state.link)

# US Population data
pop = read.csv("nst-est2019-alldata.csv")

pop = pop %>%
    select(NAME, POPESTIMATE2019)

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

today.state = states %>%
    group_by(state) %>%
    summarise(date = max(date)) %>%
    left_join(states, by = c("state", "date")) %>%
    left_join(pop, by = c("state" = "NAME")) %>%
    mutate(
        casesPer100k = cases / (POPESTIMATE2019 / 100000),
        deathsPer100k = deaths / (POPESTIMATE2019 / 100000)
    )

today.county = counties %>%
    group_by(state, county) %>%
    summarise(date = max(date)) %>%
    left_join(counties, by = c("state", "county", "date"))


shinyServer(function(input, output) {
    ###########################################################################################
    ###################################### DATA SETUP #########################################
    ###########################################################################################
    # Function to filter data
    state.df = eventReactive(input$updateData, {
        state.df = states %>% filter(state == input$stateName)
        state.df
    })
    
    state.county.df = eventReactive(input$updateData, {
        state.county.df = today.county %>% filter(state == input$stateName)
        state.county.df
    })
    
    us.df = eventReactive(input$updateData, {
        us.df = states %>% group_by(date) %>% summarise(cases = sum(cases), deaths = sum(deaths))
        us.df
    })
    
    ###########################################################################################
    ###################################### STATE TAB ##########################################
    ###########################################################################################
    
    # State plot
    output$state_plot = renderPlotly({
        df = state.df()
        # Create plot
        p = ggplot(df, aes(date)) +
            geom_point(aes(y = cases, colour = "cases")) +
            geom_line(aes(y = cases, colour = "cases")) +
            geom_point(aes(y = deaths, colour = "deaths")) +
            geom_line(aes(y = deaths, colour = "deaths")) +
            ggtitle(paste0("Cases vs Deaths in ", input$stateName)) +
            labs(x = "Date", y = "Count")
        
        ggplotly(p)
    })
    
    # Growth rate plot
    output$growth_plot = renderPlotly({
        df = state.df()
        # Calculate growth rate
        df = df %>% arrange(date) %>% mutate(growthRate = cases - lag(cases))
        # Create plot
        p = ggplot(df, aes(date)) +
            geom_point(aes(y = growthRate, colour = "cases")) +
            geom_line(aes(y = growthRate, colour = "cases")) +
            ggtitle(paste0("Growth Rate in ", input$stateName)) +
            labs(x = "Date", y = "Growth")
        
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
    
    output$worstCounties = renderPlotly({
        df = state.county.df()
        p = ggplot(df, aes(
            x = reorder(county, cases),
            cases,
            fill = county
        )) +
            geom_bar(stat = 'identity') +
            theme(legend.position = "none",
                  axis.text.x = element_text(angle = 90, hjust = 1)) +
            labs(x = "County", y = "Confirmed Cases")
        
        ggplotly(p)
    })
    
    ###########################################################################################
    ###################################### US TAB #############################################
    ###########################################################################################
    # State plot
    output$usPlot = renderPlotly({
        df = us.df()
        # Create plot
        p = ggplot(df, aes(date)) +
            geom_point(aes(y = cases, colour = "cases")) +
            geom_line(aes(y = cases, colour = "cases")) +
            geom_point(aes(y = deaths, colour = "deaths")) +
            geom_line(aes(y = deaths, colour = "deaths")) +
            ggtitle("Cases vs Deaths in United States") +
            labs(x = "Date", y = "Count")
        
        ggplotly(p)
    })
    
    # Growth rate plot
    output$growthUSPlot = renderPlotly({
        df = us.df()
        # Calculate growth rate
        df = df %>% arrange(date) %>% mutate(growthRate = cases - lag(cases))
        # Create plot
        p = ggplot(df, aes(date)) +
            geom_point(aes(y = growthRate, colour = "cases")) +
            geom_line(aes(y = growthRate, colour = "cases")) +
            ggtitle("Growth Rate in United States") +
            labs(x = "Date", y = "Growth")
        
        ggplotly(p)
    })
    
    # Value Box Outputs
    output$numUSConfirmed = renderValueBox({
        df = us.df()
        valueBox(max(df$cases), 
                 "Confirmed", color = "yellow")
    })
    
    output$numUSDied = renderValueBox({
        df = us.df()
        valueBox(max(df$deaths), 
                 "Deaths", color = "orange")
    })
    
    output$deathRateUS = renderValueBox({
        df = us.df()
        valueBox(paste0(round(((max(df$deaths) / max(df$cases))*100), 2),"%"), 
                 "Death Rate", color = "red")
    })
    
    # Cases By State
    output$stateCasesPlot = renderPlotly({
        p = ggplot(today.state, aes(x = reorder(state, cases), cases, fill = state)) +
            geom_bar(stat = 'identity') +
            theme(legend.position="none", 
                  axis.text.x = element_text(angle = 90, hjust = 1)) +
            labs(x = "State", y = "Confirmed Cases")
        
        ggplotly(p)
    })
    
    # Cases Per 100k By State
    output$stateCases100kPlot = renderPlotly({
        p = ggplot(today.state, aes(x = reorder(state, casesPer100k), casesPer100k, fill = state)) +
            geom_bar(stat = 'identity') +
            theme(legend.position="none", 
                  axis.text.x = element_text(angle = 90, hjust = 1)) +
            labs(x = "State", y = "Confirmed Cases Per 100k")
        
        ggplotly(p)
    })
    
    # Deaths Per 100k By State
    output$stateDeaths100kPlot = renderPlotly({
        p = ggplot(today.state, aes(x = reorder(state, deathsPer100k), deathsPer100k, fill = state)) +
            geom_bar(stat = 'identity') +
            theme(legend.position="none", 
                  axis.text.x = element_text(angle = 90, hjust = 1)) +
            labs(x = "State", y = "Deaths Per 100k")
        
        ggplotly(p)
    })

})
