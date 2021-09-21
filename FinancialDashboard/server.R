library(shiny)
library(tidyquant)
library(tidyverse)
library(plotly)
library(DataCombine)
options(warn= -1)
# Functions ----
get_yearly_data <- function(ticker){
    tq_get(ticker, get= "stock.prices", complete_cases = FALSE,
           from = today()-months(12),
           to   = today())
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


server <- function(input, output, session) { 
    # Home Tab ----
    # Index Data Import ----
    dow <- get_yearly_data("^DJI")
    dax <- get_yearly_data("^GDAXI")
    sp500 <- get_yearly_data("^GSPC")
    hsi <- get_yearly_data("^HSI")
    # Most active Import and calculations ----
    dow_tickers <- tq_index("DOW") %>% 
        dplyr::select("symbol")
    
    
    dow_stocks_today <- tq_get(dow_tickers, get= "stock.prices", complete_cases = TRUE,
                               from= today()- days(1), to= today())
    
    dow_stocks_today <- dow_stocks_today %>% 
        mutate("Change" = 100 * (dow_stocks_today$close - dow_stocks_today$open) /
                   dow_stocks_today$open)
    
    gainers <- dplyr::arrange(dow_stocks_today, desc(Change)) %>% 
        dplyr::select(symbol, Change) %>% 
        dplyr::slice(1:10)
    losers <- dplyr::arrange(dow_stocks_today, Change) %>% 
        dplyr::select(symbol, Change) %>% 
        dplyr::slice(1:10)
    
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
    # Most active plots ----
    output$gainers <- renderDataTable(gainers)
    output$losers <- renderDataTable(losers)
    
    
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
    ## Data Import ----

    
    observeEvent(c(input$period, input$tickers), {
                     stock_prices <- tq_get(dow_tickers, get= "stock.prices", complete_cases = TRUE) %>% 
                         filter(symbol %in% input$tickers)
                     
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
                     
                     if (input$period == 5) {
                         stock_prices <- stock_prices %>%
                             filter(year(date) == year(today()))
                     }
                     
                     
                     # Create plot
                     output$chart <- renderPlotly({
                         print(
                             ggplotly(stock_prices %>%
                                          
                                          ggplot(aes(date, close,colour = symbol)) +
                                          geom_line(size = 0.4, alpha = .9) +
                                          labs(colour="Stock")+
                                          xlab("Time")+
                                          ylab("Value in $")+
                                          theme_minimal())
                         )
                     })
    }
    )
}



