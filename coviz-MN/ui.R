library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidymodels)
library(dplyr)
library(plotly)

shinyUI(# Define page
    dashboardPage(
        # Define page header
        dashboardHeader(title = "Minnesota COVID-19 Tracker"),
        # Define sidebar
        dashboardSidebar(
            # Sidebar Menu
            sidebarMenu(
                menuItem("Daily Totals", tabName = "dailyTotals"),
                menuItem("Testing", tabName = "testing"),
                menuItem("Age Groups", tabName = "ageGroups"),
                menuItem("About", tabName = "about")
            )
        ),
        # Define body
        dashboardBody(
            tabItems(
                #############################################
                ################ State Level ################
                #############################################
                tabItem(
                    "dailyTotals",
                    # Top summary row
                    fluidRow(
                        valueBoxOutput("totalCases", width = 3),
                        valueBoxOutput("newCases", width = 3),
                        valueBoxOutput("twoWkTrend", width = 3),
                        valueBoxOutput("lastUpdated", width = 3)
                    ),
                    # Second summary row
                    fluidRow(
                        valueBoxOutput("totalDeaths", width = 3),
                        valueBoxOutput("newDeaths", width = 3),
                        valueBoxOutput("fatalityRate", width = 3)
                    ),
                    # Main plot row
                    # Cases & Deaths
                    fluidRow(
                        box(
                            title = "Confirmed Cases",
                            solidHeader = T,
                            width = 6,
                            collapsible = T,
                            plotlyOutput("dailyCases")
                        ),
                        box(
                            title = "Deaths from COVID-19",
                            solidHeader = T,
                            width = 6,
                            collapsible = T,
                            plotlyOutput("dailyDeaths")
                        )
                    ),
                    # Hospitalizations
                    fluidRow(
                        box(
                            title = "Hospitalizations (Non-ICU)",
                            solidHeader = T,
                            width = 6,
                            collapsible = T,
                            plotlyOutput("dailyNonICU")
                        ),
                        box(
                            title = "Hospitalizations (ICU)",
                            solidHeader = T,
                            width = 6,
                            collapsible = T,
                            plotlyOutput("dailyICU")
                        )
                    )
                ),
                #############################################
                ################ Insights ###################
                #############################################
                tabItem(
                    "testing",
                    # Top summary row
                    fluidRow(
                        valueBoxOutput("dailyPosRate"),
                        valueBoxOutput("dailyTests"),
                        valueBoxOutput("totalTests")
                    ),
                    
                    # Main plot row
                    # Cases vs Deaths plot
                    fluidRow(
                        box(
                            title = "Daily Tests Administered",
                            solidHeader = T,
                            width = 6,
                            collapsible = T,
                            plotlyOutput("dailyTesting")
                        ),
                        box(
                            title = "Daily Positive Test Rate",
                            solidHeader = T,
                            width = 6,
                            collapsible = T,
                            plotlyOutput("dailyPositiveTests")
                        )
                    )
                ),
                #############################################
                ################ Age Groups #################
                #############################################
                tabItem(
                    "ageGroups",
                    # Top summary row
                    fluidRow(
                    ),
                    fluidRow(
                        box(
                            title = "Cases by Age Group",
                            solidHeader = T,
                            width = 6,
                            collapsible = T,
                            plotlyOutput("casesByAge")
                        )
                    )
                ),
                # About Page
                tabItem("about",
                        h1("About the COVID-19 Tracker"),
                        h2("Disclaimer"),
                        p("This tool should not be used to make any kind of medical decision(s). This is simply a visualization of COVID-19 data from the sources indicated below. Feel free to use this as a tracker or way to stay informed on spread, but DO NOT use this regarding any health or medical concerns."),
                        h2("Data Sources"),
                        p("Data utilized in this tracker comes from the New York Times which can be found here : https://raw.githubusercontent.com/nytimes/covid-19-data/master"),
                        p("State population estimates are as of 2019. These populations may differ from current actual populations but are used as the closest estimates.")
                        
                )
            ))
    ))
