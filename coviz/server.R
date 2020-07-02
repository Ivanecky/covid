library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidymodels)
library(dplyr)
library(plotly)
library(RCurl)
library(rvest)
library(XML)

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
    select(NAME, POPESTIMATE2019) %>%
    mutate(
        NAME = as.character(NAME)
    )

# List of "states" not to include
remove_states = c("American Samoa", "Guam", "Northern Mariana Islands", "Virgin Islands")

# Filter out
states = states %>% filter(!(state %in% remove_states))

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
    ) %>%
    mutate(
        growth_rate = cases - lag(cases)
    )

states = states %>%
    group_by(state, fips, date) %>%
    arrange(date) %>%
    summarise(
        cases = sum(cases),
        deaths = sum(deaths),
        death_rate = round((deaths / cases)*100, 2)
    ) %>%
    mutate(
        growth_rate = cases - lag(cases),
        death_change = deaths - lag(deaths)
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

us <- states %>% 
    group_by(date) %>% 
    summarise(
        cases = sum(cases), 
        deaths = sum(deaths)
    ) %>%
    mutate(
        growth_rate = cases - lag(cases),
        death_rate = deaths / cases,
        death_change = deaths - lag(deaths)
    )

shinyServer(function(input, output) {
    ###########################################################################################
    ###################################### DATA SETUP #########################################
    ###########################################################################################
    # Functions to filter data
    state.df <- eventReactive(input$stateName, {
        state.df = states %>% filter(state == input$stateName)
        return(state.df)
    })
    
    state.county.df <- eventReactive(input$stateName, {
        state.county.df = today.county %>% filter(state == input$stateName)
        return(state.county.df)
    })
    
    growth.rank <- eventReactive(input$stateName, {
        df = today.state %>% arrange(-growth_rate) %>% mutate(rank = 1:nrow(today.state))
        rank = df[which(df$state == input$stateName), ]$rank
        return(rank)
    })
    
    ###########################################################################################
    ###################################### STATE TAB ##########################################
    ###########################################################################################
    ################
    ### VALUE BOX
    ################
    
    # Confirmed cases
    output$numConfirmed <- renderValueBox({
        df <- state.df()
        valueBox(max(df$cases), 
                 "Total Cases", color = "red")
    })
    
    # Deaths
    output$numDied <- renderValueBox({
        df <- state.df()
        valueBox(max(df$deaths), 
                 "Total Deaths", color = "red")
    })
    
    # Death rate
    output$deathRate <- renderValueBox({
        df <- state.df()
        valueBox(paste0(round(((max(df$deaths) / max(df$cases))*100), 2),"%"), 
                 "Fatality Rate", color = "red")
    })
    
    # Data last updated
    output$lastUpdated <- renderValueBox({
        df <- state.df()
        valueBox(max(df$date), "Last Updated", color = "blue")
    })
    
    # New cases
    output$newCases <- renderValueBox({
        df <- state.df()
        df = df %>% arrange(date)
        caseChange = df[nrow(df), ]$growth_rate
        valueBox(caseChange, "New Cases", color = "blue")
    })
    
    # New deaths
    output$newDeaths <- renderValueBox({
        df <- state.df()
        df = df %>% arrange(date)
        deathChange = df[nrow(df), ]$death_change
        valueBox(deathChange, "New Deaths", color = "blue")
    })
    
    ############
    ### PLOTS
    ############
    # Cases
    output$stateCases <- renderPlotly({
        df <- state.df()
        # Create plot
        p <- ggplot(df, aes(date, cases)) +
                geom_line() +
                labs(x = "Date", y = "Count") +
                scale_color_manual(values = c('blue'))
        # Plotly
        ggplotly(p)
    })
    
    # Deaths
    output$stateDeaths <- renderPlotly({
        df <- state.df()
        # Create plot
        p <- ggplot(df, aes(date, deaths, color = "blue")) +
            geom_line() +
            labs(x = "Date", y = "Count", colour = "")
        # Plotly
        ggplotly(p)
    })
    
    # Growth rate plot
    output$growth_plot <- renderPlotly({
        df <- state.df()
        # Calculate growth rate
        df = df %>% arrange(date)
        # Create plot
        p = ggplot(df, aes(date)) +
            geom_point(aes(y = growth_rate, colour = "cases")) +
            geom_line(aes(y = growth_rate, colour = "cases")) +
            labs(x = "Date", y = "Growth", colour = "")
        ggplotly(p)
    })
    
    # Worst Counties plot
    output$worstCounties <- renderPlotly({
        df <- state.county.df()
        p = ggplot(df, aes(
            x = reorder(county, cases),
            cases,
            fill = county
        )) +
            geom_bar(stat = 'identity') +
            theme(legend.position = "none",
                  axis.text.x = element_text(angle = 90, hjust = 1)) +
            labs(x = "County", y = "Confirmed Cases") +
            scale_color_gradientn(colours = rainbow(52))
        
        ggplotly(p)
    })
    
    ###########################################################################################
    ###################################### US TAB #############################################
    ###########################################################################################
    #############
    ### PLOTS
    #############
    # Case trends
    output$usCasesPlot <- renderPlotly({
        df <- us
        # Create plot
        p <- ggplot(df, aes(date,cases)) +
            geom_line() +
            ggtitle("Cases in United States") +
            labs(x = "Date", y = "Cases", colour = "")
        
        ggplotly(p)
    })
    
    # Death trends
    output$usDeathsPlot <- renderPlotly({
        df <- us
        # Create plot
        p <- ggplot(df, aes(date, deaths)) +
            geom_line() +
            ggtitle("Deaths in United States") +
            labs(x = "Date", y = "Deaths", colour = "")
        
        ggplotly(p)
    })
    
    # New daily cases
    output$growthUSPlot <- renderPlotly({
        df <- us
        # Calculate growth rate
        df <- df %>% arrange(date) %>% mutate(growthRate = cases - lag(cases))
        # Create plot
        p <- ggplot(df, aes(date)) +
            geom_point(aes(y = growthRate, colour = "cases")) +
            geom_line(aes(y = growthRate, colour = "cases")) +
            ggtitle("New Cases by Day in United States") +
            labs(x = "Date", y = "Growth", colour = "")
        
        ggplotly(p)
    })
    
    # Cases By State
    output$stateCasesPlot <- renderPlotly({
        p <- ggplot(today.state, aes(x = reorder(state, cases), cases, fill = state)) +
            geom_bar(stat = 'identity') +
            theme(legend.position="none", 
                  axis.text.x = element_text(angle = 90, hjust = 1)) +
            labs(x = "State", y = "Confirmed Cases")
        
        ggplotly(p)
    })
    
    # Cases Per 100k By State
    output$stateCases100kPlot <- renderPlotly({
        p <- ggplot(today.state, aes(x = reorder(state, casesPer100k), casesPer100k, fill = state)) +
            geom_bar(stat = 'identity') +
            theme(legend.position="none", 
                  axis.text.x = element_text(angle = 90, hjust = 1)) +
            labs(x = "State", y = "Confirmed Cases Per 100k")
        
        ggplotly(p)
    })
    
    # Deaths Per 100k By State
    output$stateDeaths100kPlot <- renderPlotly({
        p <- ggplot(today.state, aes(x = reorder(state, deathsPer100k), deathsPer100k, fill = state)) +
            geom_bar(stat = 'identity') +
            theme(legend.position="none", 
                  axis.text.x = element_text(angle = 90, hjust = 1)) +
            labs(x = "State", y = "Deaths Per 100k")
        
        ggplotly(p)
    })
    
    ###############
    ### VALUE BOX
    ###############
    # Value Box Outputs
    output$numUSConfirmed <- renderValueBox({
        valueBox(max(us$cases), 
                 "Confirmed", color = "red")
    })
    
    output$numUSDied <- renderValueBox({
        valueBox(max(us$deaths), 
                 "Deaths", color = "red")
    })
    
    output$deathRateUS <- renderValueBox({
        todayUS = us %>% filter(date == max(date)) %>% select(death_rate)
        valueBox(paste0(round(todayUS$death_rate*100, 2),"%"), 
                 "Death Rate", color = "red")
    })
    
    # Daily US Change
    output$newUSCases <- renderValueBox({
        df <- us %>% arrange(date)
        newCases = as.numeric(df[nrow(df), ]$growth_rate)
        valueBox(newCases, "New Cases", color = "blue")
    })
    
    # Daily Death Change
    output$newUSDeaths <- renderValueBox({
        df <- us %>% arrange(date)
        newDeaths = as.numeric(df[nrow(df), ]$death_change)
        valueBox(newDeaths, "New Deaths", color = "blue")
    })
    
    # Data last updated
    output$lastUpdatedUS <- renderValueBox({
        valueBox(max(us$date), "Last Updated", color = "blue")
    })
    
    ###########################################################################################
    ###################################### CHANGE #############################################
    ###########################################################################################
    ###############
    ### VALUE BOX
    ###############
    
    #############
    ### PLOTS
    #############
    # Change of states
    output$stateChanges <- renderPlotly({
        ggplot(today.state, aes(reorder(state, growth_rate), growth_rate, fill = state)) +
            geom_bar(stat = "identity") +
            ggtitle("Current Growth Rate by State") +
            labs(x = "State", y = "Growth Rate") +
            theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1)) -> p
        
        ggplotly(p)
    })
    
    # Change of states (as percent)
    output$stateChangePct <- renderPlotly({
        today.state$pctChange = round((today.state$growth_rate / today.state$cases)*100, 2)
        
        ggplot(today.state, aes(reorder(state, pctChange), pctChange, fill = state)) +
            geom_bar(stat = "identity") +
            ggtitle("Current Growth Rate by State") +
            labs(x = "State", y = "Growth Rate") +
            theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1)) -> p
        
        ggplotly(p)
    })
    
    # Pct change in cases over past 2 weeks
    output$twoWeekChange <- DT::renderDataTable({
        stateChanges <- states %>%
            filter(date == today.state$date-14) %>%
            select(state, cases, deaths) %>%
            rename(
                oldCases = cases,
                oldDeaths = deaths
            )
        
        df <- today.state %>%
            left_join(stateChanges, by = "state") %>%
            mutate(
                casePctGrowth = round(((cases / oldCases) - 1) * 100, 2),
                caseRawGrowth = cases - oldCases,
                deathsPctGrowth = round(((deaths / oldDeaths) - 1) * 100, 2),
                deathsRawGrowth = deaths - oldDeaths
            ) %>%
            select(state, cases, casePctGrowth, caseRawGrowth, deaths, deathsPctGrowth, deathsRawGrowth) %>%
            rename(
                State = state, 
                Cases = cases, 
                `Percent Growth of Cases` = casePctGrowth,
                `Raw Growth of Cases` = caseRawGrowth,
                Deaths = deaths, 
                `Percent Growth of Deaths` = deathsPctGrowth,
                `Raw Growth of Deaths` = deathsRawGrowth
            )
        
        df
    })

})
