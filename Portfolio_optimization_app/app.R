## app.R ##
library(shiny)
library(tidyverse)
library(tidyquant)
library(shinydashboard)
library(timetk)

theme_set(theme_bw())

source('function.R')


ui <- dashboardPage(
    dashboardHeader(title = "A simple portfolio optimization"),
    dashboardSidebar(
        dateRangeInput('dateRange',
                       label = 'Date range input: yyyy-mm-dd',
                       start = "2015-01-01", end = Sys.Date() - 2
        ),
        checkboxGroupInput("stock1", 
                    label = "Stocks", 
                    choices = c("AAPL", "GOOG", "FB", "JPM", 'TSLA'),
                    selected = c("AAPL", "GOOG", "FB")),
        
        sidebarMenu(
            menuItem("Dashboard 1", tabName = "dashboard1", icon = icon("dashboard"))
           # menuItem("Dashboard 2", tabName = "dashboard2", icon = icon("dashboard"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "dashboard1",
                    fluidRow(
                        
                        plotOutput("plot1")
                        
                    ),
                    
                    fluidRow(
                        
                        box(htmlOutput("text"),height = 150),
                        box(plotOutput("weights"))
                        
                    )
            ),
            tabItem(tabName = "dashboard2",
                    fluidRow(
                    )
            )
        )
    )
)


server <- function(input, output) {
    
    # Getting the data and the returns
    stocks_returns <- reactive({
        
        input$stock1 %>%
            tq_get(get  = "stock.prices",
                   from = input$dateRange[1], 
                   to = input$dateRange[2]) %>% 
            select(symbol, date, adjusted) %>% 
            filter(symbol %in% c(input$stock1)) %>% 
            pivot_wider(date, names_from = symbol, values_from = adjusted) %>% 
            modify_if(is.numeric, ~ (lead(.) / .) - 1) %>% 
            tk_xts(date_col = date)
        
    })
    
    # Plotting the efficient frontier
    output$plot1 <- renderPlot({
        
        Ra   <-  Return.annualized(stocks_returns(), scale = 252) %>% as.vector() 
        names(Ra) <- names(stocks_returns())
        cov_mat <-  var(stocks_returns(), na.rm = TRUE) * 252
        cov_mat_inv <- solve(cov_mat)
        
        
        w_t <- tangency_portfolio(Ra, cov_mat_inv)
        er_tangency_pf <- expected_return(Ra, w_t)
        sd_tangency_pf <- expected_sd(cov_mat, w_t)
        
        w_gmin <- global_min_portfolio(Ra, cov_mat)
        er_gmin <- expected_return(Ra, w_gmin)
        sd_gmin <- expected_sd(cov_mat, w_gmin)
        
        r <- efficient_frontier(Ra, cov_mat, nport = 20)
        sr.tan <-  (er_tangency_pf - 0.005)/ sd_tangency_pf
        
        
        ggplot(r) +
            geom_point(aes(x = sd_gmin, y = er_gmin), color = "red") +
            geom_point(aes(x = sd_tangency_pf, y = er_tangency_pf), color = "green") +
            geom_abline(intercept = 0.005, slope = sr.tan, color = "blue") + 
            geom_path(aes(x = sd_e, y = er_e)) +
            labs(title = "The Efficient Frontier",
                x = 'Standard Deviation', y = "Expected Return")
        
    })
    
    # The efficient portfolio weight's
    output$weights <-  renderPlot({
        
        Ra   <-  Return.annualized(stocks_returns(), scale = 252) %>% as.vector() 
        names(Ra) <- names(stocks_returns())
        cov_mat <-  var(stocks_returns(), na.rm = TRUE) * 252
        cov_mat_inv <- solve(cov_mat)
        
        w_t <- tangency_portfolio(Ra, cov_mat_inv)
        
        tibble(names = names(w_t), weights = w_t) %>%
            ggplot(aes(x = names, y = weights, fill = names)) +
            geom_col() +
            theme(legend.position = "none") +
            labs(title = "The Tangency Portfolio Weight's", x = "")
    })
    
    
    # Giving some key statistics
    output$text <- renderUI({
        
        Ra   <-  Return.annualized(stocks_returns(), scale = 252) %>% as.vector() 
        names(Ra) <- names(stocks_returns())
        cov_mat <-  var(stocks_returns(), na.rm = TRUE) * 252
        cov_mat_inv <- solve(cov_mat)
        
        
        w_t <- tangency_portfolio(Ra, cov_mat_inv)
        er_tangency_pf <- expected_return(Ra, w_t)
        sd_tangency_pf <- expected_sd(cov_mat, w_t)
        sharpe_ratio <-  (er_tangency_pf - 0.005) / sd_tangency_pf
        
        w_gmin <- global_min_portfolio(Ra, cov_mat)
        er_gmin <- expected_return(Ra, w_gmin)
        sd_gmin <- expected_sd(cov_mat, w_gmin)
        
        
        str1 <- paste("The expected return of the tangency portfolio is", round(er_tangency_pf, 3), ".")
        str2 <- paste("The expected standard deviation of the tangency portfolio is", round(sd_tangency_pf, 3), ".")
        str3 <- paste("The Sharpe ratio of the tangency portfolio is", round(sharpe_ratio, 3), ".")
        
        HTML(paste(str1, str2, str3, sep = '<br/>'))
        
        })
    
}


shinyApp(ui, server)



