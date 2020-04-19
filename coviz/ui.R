library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidymodels)
library(dplyr)
library(plotly)

shinyUI(
    # Define page
    dashboardPage(
        # Define page header
        dashboardHeader(title = "COVID-19 Tracker"),
        # Define sidebar
        dashboardSidebar(
            # Some input method(s)
            textInput("stateName", "State", value = "Minnesota", width = NULL,
                      placeholder = "Minnesota"),
            actionButton("updateData", "Update")
        ),
        # Define body
        dashboardBody(
            # Top summary row
            fluidRow(
                valueBoxOutput("numConfirmed"),
                valueBoxOutput("numDied"),
                valueBoxOutput("deathRate")
            ),
            # Main plot row
            fluidRow(
                box(title = "Tracking Cases within the State", solidHeader = T,
                    width = 12, collapsible = T,
                    plotlyOutput("state_plot"))
            )
        )
    )
)
