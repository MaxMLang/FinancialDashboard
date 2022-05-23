library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(quantmod)
library(tidyquant)
library(tidyverse)
library(plotly)
library(DataCombine)
library(rvest)
options(warn= -1)

# Functions ----
get_yearly_data <- function(ticker){
    data <- tq_get(ticker, get= "stock.prices", complete_cases = TRUE)
    data %>% filter(date >= today()-months(12))
}

plot_index_graph <- function(data){
    ggplotly({data %>%
                 ggplot(aes(date, close)) +
                 geom_line(size = 0.4, alpha = .9) +
                 xlab("Time")+
                 ylab("Points")+
                 theme_minimal()}
    )
}

plot_standard_graph <- function(data, colour){
    ggplotly(data %>%
                 ggplot(aes(date, close)) +
                 geom_line(size = 0.4, alpha = .9, colour= colour) +
                 xlab("Time")+
                 ylab("Value in US-Dollars")+
                 theme_minimal()
    )
}

watchlist <- c("QCOM","NVDA", "TSM", "SRPT", "MSFT", "AAPL", "AMZN")
dow <- get_yearly_data("^DJI")
dax <- get_yearly_data("^GDAXI")
sp500 <- get_yearly_data("^GSPC")
hsi <- get_yearly_data("^HSI")

dax_open <- dax %>% filter(date== max(dax$date)) %>% pull(open)
dax_close <- dax %>% filter(date== max(dax$date)) %>% pull(close)
dow_open <- dow %>% filter(date== max(dow$date)) %>% pull(open)
dow_close <- dow %>% filter(date== max(dow$date)) %>% pull(close)

dow <- get_yearly_data("^DJI")
dax <- get_yearly_data("^GDAXI")
sp500 <- get_yearly_data("^GSPC")
hsi <- get_yearly_data("^HSI")

dax_open <- dax %>% filter(date== max(dax$date)) %>% pull(open)
dax_close <- dax %>% filter(date== max(dax$date)) %>% pull(close)
dow_open <- dow %>% filter(date== max(dow$date)) %>% pull(open)
dow_close <- dow %>% filter(date== max(dow$date)) %>% pull(close)
# Watchlists ----
watchlist <- c("QCOM","NVDA", "TSM", "SRPT", "MSFT", "AAPL", "AMZN")
watchlist_data <- tq_get(watchlist) %>% 
    filter(date>= today()-days(1))

# Crypto Data Import ----
btc <- get_yearly_data("BTC-USD")
eth <- get_yearly_data("ETH-USD")
bch <- get_yearly_data("BCH-USD")
ltc <- get_yearly_data("LTC-USD")
doge <- get_yearly_data("DOGE-USD")

# Minerals Data Import ----
gold <- get_yearly_data("GC=F")
silver <- get_yearly_data("SI=F")
oil <- get_yearly_data("BZ=F")

server <- function(input, output) { 
    showNotification("Booting up... Please wait.", type = "message", duration = 30)
    # Home Tab ----
    # Index Data Import ----
    dow <- get_yearly_data("^DJI")
    dax <- get_yearly_data("^GDAXI")
    sp500 <- get_yearly_data("^GSPC")
    hsi <- get_yearly_data("^HSI")
    
    dax_open <- dax %>% filter(date== max(dax$date)) %>% pull(open)
    dax_close <- dax %>% filter(date== max(dax$date)) %>% pull(close)
    dow_open <- dow %>% filter(date== max(dow$date)) %>% pull(open)
    dow_close <- dow %>% filter(date== max(dow$date)) %>% pull(close)
    
    # Watchlists ----
    watchlist <- c("QCOM","NVDA", "TSM", "SRPT", "MSFT", "AAPL", "AMZN")
    watchlist_data <- tq_get(watchlist) %>% 
        filter(date>= max(.$date))
    
    # Crypto Data Import ----
    btc <- get_yearly_data("BTC-USD")
    eth <- get_yearly_data("ETH-USD")
    bch <- get_yearly_data("BCH-USD")
    ltc <- get_yearly_data("LTC-USD")
    doge <- get_yearly_data("DOGE-USD")
    
    # Minerals Data Import ----
    gold <- get_yearly_data("GC=F")
    silver <- get_yearly_data("SI=F")
    oil <- get_yearly_data("BZ=F")
    
    # Index plots ----
    output$dow <- renderPlotly({
        print(plot_index_graph(dow))
    })
    output$sp500 <- renderPlotly({
        print(plot_index_graph(sp500))
    })
    
    output$dax <- renderPlotly({
        print(plot_index_graph(dax))
    })
    
    output$hsi <- renderPlotly({
        print(plot_index_graph(hsi))
    })
    # watchlist ----
    output$watchlist <- renderDataTable(watchlist_data)
    
    
    
    # Crypto plots ----
    output$btc <- renderPlotly({
        print(plot_standard_graph(btc, "#ff9900"))
    })
    
    output$eth <- renderPlotly({
        print(plot_standard_graph(eth, "#522e92"))
    })
    
    output$bch <- renderPlotly({
        print(plot_standard_graph(bch, "#8dc351"))
    })
    
    output$ltc <- renderPlotly({
        print(plot_standard_graph(ltc, "#A9A9A9"))
    })
    
    output$doge <- renderPlotly({
        print(plot_standard_graph(doge, "#E1C699"))
    })
    
    # Minerals plots ----
    output$gold <- renderPlotly({
        print(plot_standard_graph(gold, colour = "black"))
    })
    
    output$silver <- renderPlotly({
        print(plot_standard_graph(silver, colour = "black"))
    })
    
    output$oil <- renderPlotly({
        print(plot_standard_graph(oil, colour = "black")) 
    })
    
    # Stocks Tab ----

    
    observeEvent(c(input$period, input$tickers), {
                     stock_prices <- tq_get(watchlist, get= "stock.prices", complete_cases = TRUE)
                     stock_prices <- stock_prices %>% 
                         filter(symbol %in% input$tickers)
                     
                     crypto_tickers <- c("BTC-USD", "ETH-USD", "ALGO-USD", "BCH-USD", "XLM-USD",
                                         "BNB-USD", "DOGE-USD")
                     
                     crypto_prices <- tq_get(crypto_tickers, 
                                             get  = "stock.prices",
                                             from = today()-months(6),
                                             to   = today(),
                                             complete_cases = F)
                     
                     
                     if (input$period == 1) {
                         stock_prices <- stock_prices %>%
                             filter(
                                 date >= today()-months(1))
                     }
                     
                     if (input$period == 2) {
                         stock_prices <- stock_prices %>%
                             filter(
                                 date >= today()-months(2))
                     }
                     
                     if (input$period == 3) {
                         stock_prices <- stock_prices %>%
                             filter(
                                 date >= today()-months(3))
                     }
                     
                     if (input$period == 4) {
                         stock_prices <- stock_prices %>%
                             filter(
                                 date >= today()-months(12))
                     }
                     if (input$period == 5) {
                         stock_prices <- stock_prices
                     }
                     
                     
                     # Create plot
                     output$chart <- renderPlotly({
                         print(ggplotly(stock_prices %>%
                                   ggplot(aes(x = date, y = close)) +
                                   geom_line()+
                                   labs(y = "Closing Price", x = "Time") +
                                   theme_tq()))
                         
                     })
                     
                     output$candlechart <- renderPlot({
                         print(stock_prices %>% 
                                   ggplot(aes(x = date, y = close, open = open, high = high, low = low)) +
                                   geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
                                   labs(title = "Candlestick Chart", y = "Closing Price", x = "") +
                                   theme_tq())
                     })
                     
                     output$returns <- renderPlotly({
                         print(ggplotly(stock_prices %>%
                                   group_by(symbol) %>%
                                   tq_transmute(select=adjusted,
                                                mutate_fun=periodReturn,
                                                period="monthly",
                                                col_rename = "monthly_return") %>%
                                   ggplot(aes(date, monthly_return, color=symbol)) +
                                   xlab("Time")+
                                   geom_line()+
                                   theme_minimal()))
                     })
                     
                    
                    output$dowbox <- renderInfoBox({
                        infoBox("Dow Jones", round(dow_close - dow_open,2), 
                                icon = icon(ifelse(dow_close - dow_open>=0,"arrow-up",
                                              "arrow-down")),
                                color = ifelse(dow_close - dow_open>=0,"green", "red"))
                    })
                    output$daxbox <- renderInfoBox({
                        infoBox("DAX", round(dax_close - dax_open,2), icon= icon(ifelse(dax_close - dax_open>=0,"arrow-up",
                                                                          "arrow-down")),
                                color = ifelse(dax_close - dax_open>=0,"green", "red"))
                    })
                    
                    
                    output$coinchart <- renderPlotly({
                        print(ggplotly( crypto_prices %>%
                                                    ggplot(aes(date, close,colour = symbol)) +
                                                    geom_line(size = 0.4, alpha = .9) +
                                                    labs(colour="Cryptocurrency")+
                                                    xlab("Time")+
                                                    ylab("Value in $") +
                                           theme_tq()))
                        
                    })
                    
                    output$coincchart <- renderPlot({
                        print(crypto_prices %>% 
                                  ggplot(aes(x = date, y = close)) +
                                  geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
                                  labs(title = "Candlestick Chart", y = "Closing Price", x = "") +
                                  theme_tq())
                    })
                    
                    
    }
    )
}




