## app.R ##
library(shiny)
library(tidyverse)
library(plotly)
library(shinydashboard)

#theme_set(theme_bw())

# Importing the data. This comes from the TidyTuesday project
bigmac <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-22/big-mac.csv')


ui <- dashboardPage(
  dashboardHeader(title = "Big Mac Index"),
  dashboardSidebar(
    
    # 3 inputs for the first Dashboard: date, a base currency and either adjusted or raw price
    selectInput('date', 'Select a date:', unique(bigmac$date), selected = "2020-07-01"),
    selectInput("base",
                label = "Select a base currency",
                choices = c("usd", "eur", "gbp", "jpy", 'cny'),
                selected = "usd"),
    selectInput("type",
                label = "Select adjusted or raw:",
                choices = c("adjusted", "raw"),
                selected = "raw"),
    
    # Input Dashboard 2
    selectInput("price",
                label = "Select local price, dollar price or adjusted price:",
                choices = c("local_price", "dollar_price", "adj_price"),
                selected = "local_price"),
    
    sidebarMenu(
      menuItem("Dashboard 1", tabName = "dashboard1", icon = icon("dashboard")),
      menuItem("Dashboard 2", tabName = "dashboard2", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard1",
              fluidRow(
                
                plotlyOutput("plot1")
                
              ),
              
              fluidRow(
                
                box(htmlOutput("text"),height = 150),
                
              )
      ),
      tabItem(tabName = "dashboard2",
              fluidRow(
                
                plotOutput("plot2")
              )
      )
    )
  )
)


server <- function(input, output) {
  
  # Filtering the data using 3 inputs: date, base, type (raw or adjusted)
  data <- reactive( {
    
      bigmac %>%
        filter(date == input$date) %>% 
        select(date, name, contains(input$type)) %>% 
        pivot_longer(!c(date, name), names_to = 'base', values_to = 'value') %>% 
        filter(base == paste0(input$base, '_', input$type)) %>% 
        mutate(overvalued = factor(ifelse(value > 0, 1, 0)),
               name = reorder(name, value),
               value = round(value, 2)) %>% 
        drop_na(value)
    
  } )
  
  # Is a currency is overvalued / undervalued compared to a based currency ?
  output$plot1 <- renderPlotly( {
    
  ggplotly(  
      
      data() %>% 
        ggplot(aes(x = name, y = value, color = overvalued)) +
        geom_point() +
        geom_hline(yintercept = 0) +
        geom_segment(aes(xend = name, yend = 0)) +
        #annotate(geom = 'text', x = 7, y = 0.15, size = 5, color = "blue",  label = 'Overvalued') +
        #annotate(geom = 'text', x = 7.5, y = -0.15, size = 5, color = "Red", label = 'Undervalued') +
        labs(title = 'How much a currency is overvalued/undervalued compared to the Big Mac price ?', x = '', y = '') +
        scale_y_continuous(labels = scales::percent) +
        theme_minimal() +
        theme(legend.position = "none", axis.text.x = element_text(angle = 45)) 
      
    )  

  } )
  
  # Plot text descriptions for the highest overvalued country
  output$text <- renderUI( {
    
    text <- function(df, base_curr, date = input$date) {
      
        
        loc <- df %>% 
          filter(date == input$date) %>% 
          filter(get(paste0(base_curr, '_', input$type)) == max(get(paste0(base_curr,"_", input$type)), na.rm = TRUE))
        
        country_loc <- loc %>% pull(name)
        local_price <- loc %>% pull(local_price)
        
        base <- df %>% 
          filter(date == input$date,
                 currency_code == str_to_upper(base_curr))
        
        base_local_price <- base %>% pull(local_price)
        base_country <- base %>% pull(name)
        
        
        implied_ex_rate <- df %>% 
          filter(date == input$date) %>% 
          mutate(implied_ex_rate = round(local_price / base_local_price, 2)) %>% 
          filter(get(paste0(base_curr, '_', input$type)) == max(get(paste0(base_curr, '_', input$type)), na.rm = TRUE)) %>% 
          pull(implied_ex_rate)
        
        diff_ex <- df %>% 
          filter(date == input$date,
                 name %in% c(country_loc, base_country)) %>% 
          summarise(ex = first(dollar_ex) / last(dollar_ex),
                    ex = round(ex, 2)) %>% pull()
        
        value_curr <- loc %>% 
          pull(get(paste0(base_curr, '_', input$type)))
        
        paste0("A Big Mac costs ", local_price, " in ", country_loc, 
               " and ", base_local_price, " in the ", base_country, ". ",
               "The implied exchange rate is ",implied_ex_rate, ". ", 
               "The difference between this and the actual exchange rate, ",
               diff_ex, ", suggests the ", country_loc, 
               " currency is  overvalued by ",round(value_curr, 3) * 100, "%")
     
    }
    
    text(bigmac, input$base, date = input$date)
    
    
  } )
  
  
  # Dashboard 2
  output$plot2 <- renderPlot( {
    
    plotting <- function(df, variable) {
      
      # We will restrict to countries which have a full number of observations
      country_to_remove <- df %>% 
        group_by(name) %>% 
        add_count(date) %>% 
        summarise(num_obs = sum(n)) %>% 
        filter(!num_obs == max(num_obs)) %>% 
        pull(name)
      
      df %>% 
        filter(!name %in% country_to_remove) %>% 
        group_by(name) %>% 
        drop_na(input$price) %>% 
        mutate(inflation = .data[[input$price]] / first(.data[[input$price]])) %>% 
        ggplot(aes(x = date, y = inflation)) +
        geom_line() +
        facet_wrap(~ name, scales = 'free_y') +
        labs(y = "Big Mac Inflation", x ="Date")
      
    }
    
    # 3 different inputs: local price, price in dollar, adjusted price
    # One way to observe inflation
    plotting(bigmac, variable =  input$price)
  } )
}


shinyApp(ui, server)
