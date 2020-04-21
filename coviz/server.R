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

# Shelter In Place data
sip = read.csv("sip.csv")

sip = sip %>% select(state, order_date)

pop = pop %>%
    select(NAME, POPESTIMATE2019)

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
    )

states = states %>%
    group_by(state, fips, date) %>%
    arrange(date) %>%
    summarise(
        cases = sum(cases),
        deaths = sum(deaths),
        death_rate = round((deaths / cases)*100, 2)
    ) %>%
    left_join(sip, by = c("state")) %>%
    mutate(
        order_date = lubridate::ymd(order_date)
    )

today.state = states %>%
    group_by(state) %>%
    summarise(date = max(date)) %>%
    left_join(states, by = c("state", "date")) %>%
    left_join(pop, by = c("state" = "NAME")) %>%
    mutate(
        casesPer100k = cases / (POPESTIMATE2019 / 100000),
        deathsPer100k = deaths / (POPESTIMATE2019 / 100000)
    ) %>%
    left_join(sip, by = c("state"))

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
        # Get start & end of SIP
        start = max(df$order_date)
        end = max(df$order_date) + 14
        # Create plot
        p = ggplot(df, aes(date)) +
            geom_point(aes(y = cases, colour = "cases")) +
            geom_line(aes(y = cases, colour = "cases")) +
            geom_point(aes(y = deaths, colour = "deaths")) +
            geom_line(aes(y = deaths, colour = "deaths")) +
            ggtitle(paste0("Cases vs Deaths in ", input$stateName)) +
            labs(x = "Date", y = "Count") +
            geom_vline(xintercept = as.numeric(start)) +
            geom_vline(xintercept = as.numeric(end))
        
        ggplotly(p)
    })
    
    # Growth rate plot
    output$growth_plot = renderPlotly({
        df = state.df()
        # Get start & end of SIP
        start = max(df$order_date)
        end = max(df$order_date) + 14
        # Calculate growth rate
        df = df %>% arrange(date) %>% mutate(growthRate = cases - lag(cases))
        # Create plot
        p = ggplot(df, aes(date)) +
            geom_point(aes(y = growthRate, colour = "cases")) +
            geom_line(aes(y = growthRate, colour = "cases")) +
            ggtitle(paste0("Growth Rate in ", input$stateName)) +
            labs(x = "Date", y = "Growth") +
            geom_vline(xintercept = as.numeric(start)) +
            geom_vline(xintercept = as.numeric(end))
        
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
    
    # Daily State Change
    output$dailyStateChange = renderValueBox({
        df = state.df()
        df = df %>% arrange(date) %>% mutate(growthRate = cases - lag(cases))
        valueBox(df[max(date), ]$growthRate, "Case Change", colour = "green")
    })
    
    # State Rank (Death Rate)
    output$deathRateRank = renderValueBox({
        valueBox("TESTING", "State Rank", colour = "pink")
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
    
    # Daily US Change
    output$dailyUSChange = renderValueBox({
        df = us.df()
        df = df %>% arrange(date) %>% mutate(growthRate = cases - lag(cases))
        valueBox(df[max(date), ]$growthRate, "Case Change", colour = "green")
    })
    
    # Fastest Growing State
    output$fastestGrowingState = renderValueBox({
        valueBox(today[max(today$death_rate), ]$state, "Deadliest State", colour = "red")
    })
    
    ###########################################################################################
    ###################################### GENERAL ############################################
    ###########################################################################################
    # Data Last Updated
    output$lastUpdated = renderValueBox({
        df = us.df()
        valueBox(max(df$date), "Data Updated On", colour = "blue")
    })

})
