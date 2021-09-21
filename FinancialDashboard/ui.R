library(shiny)
library(shinydashboard)
library(shinyWidgets)
# Header ----
header <- dashboardHeader(title = "Financial Dashboard")
# Sidebar ----
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Stocks", tabName = "stocks", icon = icon("chart-line")),
        menuItem("ETFs", tabName = "etfs", icon = icon("chart-pie")),
        menuItem("Cryptocurrency", tabName = "crypto", icon = icon("bitcoin")),
        menuItem("Minerals", tabName = "minerals", icon = icon("tint")),
        menuItem("Currency", tabName = "minerals", icon = icon("dollar-sign"))
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
        ### Most Active Box ----
        tabBox(
            title = "Stocks: Most active", 
            id= "mostactive",
            selected = "Gainers",
            tabPanel("Gainers", dataTableOutput("gainers")),
            tabPanel("Losers", dataTableOutput("losers"))
        )
    ),
    fluidRow(
        ### Crypto Box ----
        tabBox(
            title = "Crypto",
            id= "crypto",
            tabPanel("Bitcoin", plotlyOutput("btc")),
            tabPanel("Ethereum", plotlyOutput("eth")),
            tabPanel("Bitcoin Catsh", plotlyOutput("bch")),
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
            fluidRow(pickerInput("tickers",label= "Search", choices = pull(dow_tickers), 
                                 multiple = TRUE, selected = pull(dow_tickers)[4]),
                     radioButtons("period", label = h4("Period"),
                                  choices = list("1 month" = 1, "3 months" = 2, "6 months" = 3, "12 months" = 4, "YTD" = 5), 
                                  selected = 4)),
            fluidRow(box("Chart of selected stock(s)",
                         plotlyOutput("chart")))
            )
    )
)  




ui <- dashboardPage(header, sidebar, body)
