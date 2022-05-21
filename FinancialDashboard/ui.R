library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyquant)
library(tidyverse)
library(plotly)


watchlist <- c("QCOM","NVDA", "TSM", "SRPT", "MSFT", "AAPL", "AMZN")

# Header ----
header <- dashboardHeader(title = "Financial Dashboard")
# Sidebar ----
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Stocks", tabName = "stocks", icon = icon("chart-line")),
        menuItem("Cryptocurrency", tabName = "crypto", icon = icon("bitcoin"))
)
)
# Body ----
body <- dashboardBody(
    ## Home Tab ----
    tabItems(
    tabItem(tabName= "home",
    fluidRow(
        ### Index Box ----
        tabBox(
            title = "Indexes",
            id = "indexes",
            tabPanel("Dow Jones", plotlyOutput("dow")),
            tabPanel("S&P 500", plotlyOutput("sp500")),
            tabPanel("Dax", plotlyOutput("dax")),
            tabPanel("Hang Seng", plotlyOutput("hsi"))
        ),
        ### Watchlist Box ----
        box(
            title = "Watchlist", 
            id= "watchlist",
            dataTableOutput("watchlist")
        )
    ),
    fluidRow(
        ### Crypto Box ----
        tabBox(
            title = "Crypto",
            id= "crypto",
            tabPanel("Bitcoin", plotlyOutput("btc")),
            tabPanel("Ethereum", plotlyOutput("eth")),
            tabPanel("Bitcoin Cash", plotlyOutput("bch")),
            tabPanel("Litecoin", plotlyOutput("ltc")),
            tabPanel("Dogecoin", plotlyOutput("doge"))
        ),
        ### Minerals Box ----
        tabBox(
            title = "Minerals",
            id= "minerals",
            tabPanel("Gold", plotlyOutput("gold")),
            tabPanel("Silver", plotlyOutput("silver")),
            tabPanel("Brent Oil", plotlyOutput("oil"))
        )
    )
    ),
    tabItem(tabName = "stocks",
            fluidRow(infoBoxOutput("dowbox"),
                     infoBoxOutput("daxbox")),
            fluidRow(pickerInput("tickers",label= "Search", choices = watchlist, 
                                 multiple = F, selected = watchlist[2])),
            fluidRow(radioButtons("period", label = h4("Period"),
                                  choices = list("1 month" = 1, "3 months" = 2, "6 months" = 3, "12 months" = 4, "YTD" = 5), 
                                  selected = 4, inline=TRUE)),
            fluidRow(tabBox(title="Charts of selected stocks",
                            id= "charts",
                            tabPanel("Chart", plotlyOutput("chart")),
                            tabPanel("Candlechart", plotOutput("candlechart"))),
                     box(title = "Monthly returns",
                            id= "returns",
                            plotlyOutput("returns")))
            )
    ,
    tabItem(tabName = "crypto",
            fluidRow(tabBox(title="Charts of selected coins",
                            id= "coincharts",
                            tabPanel("Chart", plotlyOutput("coinchart")),
                            tabPanel("Candlechart", plotOutput("coincchart"))))
    )
    )
    
)
 



ui <- dashboardPage(header, sidebar, body)



