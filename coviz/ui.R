library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidymodels)
library(dplyr)
library(plotly)

shinyUI(# Define page
    dashboardPage(
        # Define page header
        dashboardHeader(title = "COVID-19 Tracker"),
        # Define sidebar
        dashboardSidebar(
            # Some input method(s)
            textInput(
                "stateName",
                "State",
                value = "Minnesota",
                width = NULL,
                placeholder = "Minnesota"
            ),
            
            actionButton("updateData", "Update"),
            
            # Sidebar Menu
            sidebarMenu(
                menuItem("States", tabName = "states"),
                menuItem("United States", tabName = "us"),
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
                "states",
                # Top summary row
                fluidRow(
                    valueBoxOutput("numConfirmed"),
                    valueBoxOutput("numDied"),
                    valueBoxOutput("deathRate")
                ),
                fluidRow(
                    valueBoxOutput("lastUpdated"),
                    valueBoxOutput("dailyStateChange"),
                    valueBoxOutput("deathRateRank")
                ),
                # Main plot row
                # Cases vs Deaths plot
                fluidRow(
                    box(
                        title = "Tracking Cases within the State",
                        solidHeader = T,
                        width = 12,
                        collapsible = T,
                        plotlyOutput("state_plot")
                    )
                ),
                # Growth Rate plot
                fluidRow(
                    box(
                        title = "Tracking Growth Rate within the State",
                        solidHeader = T,
                        width = 12,
                        collapsible = T,
                        plotlyOutput("growth_plot")
                    )
                ),
                # Worst Counties plot
                fluidRow(
                    box(
                        title = "Tracking Worst Counties within the State",
                        solidHeader = T,
                        width = 12,
                        collapsible = T,
                        plotlyOutput("worstCounties")
                    )
                )
            ),
            #############################################
            ################### US Level ################
            #############################################
            tabItem(
                "us",
                # Top summary row
                fluidRow(
                    valueBoxOutput("numUSConfirmed"),
                    valueBoxOutput("numUSDied"),
                    valueBoxOutput("deathRateUS")
                ),
                # Main plot row
                # Cases vs Deaths plot
                fluidRow(
                    box(
                        title = "Tracking Cases within the United States",
                        solidHeader = T,
                        width = 12,
                        collapsible = T,
                        plotlyOutput("usPlot")
                    )
                ),
                # Growth Rate plot
                fluidRow(
                    box(
                        title = "Tracking Growth Rate in United States",
                        solidHeader = T,
                        width = 12,
                        collapsible = T,
                        plotlyOutput("growthUSPlot")
                    )
                ),
                # Top States plot
                fluidRow(
                    box(
                        title = "Confirmed Cases Across States",
                        solidHeader = T,
                        width = 12,
                        collapsible = T,
                        plotlyOutput("stateCasesPlot")
                    )
                ),
                # Cases Per 100k
                fluidRow(
                    box(
                        title = "Confirmed Cases (Per 100k) Across States",
                        solidHeader = T,
                        width = 12,
                        collapsible = T,
                        plotlyOutput("stateCases100kPlot")
                    )
                ),
                # Deaths Per 100k
                fluidRow(
                    box(
                        title = "Deaths (Per 100k) Across States",
                        solidHeader = T,
                        width = 12,
                        collapsible = T,
                        plotlyOutput("stateDeaths100kPlot")
                    )
                )
            ),
            # About Page
            tabItem("about",
                h1("About the COVID-19 Tracker")
            )
        ))
    ))
