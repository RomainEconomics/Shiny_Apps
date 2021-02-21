## app.R ##
library(shiny)
library(tidyverse)
library(COVID19)
library(shinydashboard)

# Building a simple App for my own use and in order to discover R Shiny

theme_set(theme_bw())



ui <- dashboardPage(
  dashboardHeader(title = "A simple App"),
  dashboardSidebar(
    dateRangeInput('dateRange',
                   label = 'Date range input:',
                   start = "2020-01-22", end = Sys.Date() 
    ),
    selectInput("y", "Y axis", c("rate per 100 000 hbt", "count")), 
    checkboxGroupInput("country1", 
                       label = "Country", 
                       choices = c("FRA", "PRT", "ITA", "RUS", "USA", "BEL", 'BRA', 'DEU', 'ESP', 'GBR', 'SWE'),
                       selected = c("FRA")),
    
    sidebarMenu(
      menuItem("Dashboard 1", tabName = "dashboard1", icon = icon("dashboard")),
      menuItem("Dashboard 2", tabName = "dashboard2", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard1",
              fluidRow(
                
                plotOutput("plot1")
                
              ),
              
              fluidRow(
                
                plotOutput("plot2")
                
              )
      ),
      tabItem(tabName = "dashboard2",
              fluidRow(
                
               # plotOutput("plot2")
              ),
              fluidRow(
                
                plotOutput("plot3")
              )
      )
    )
  )
)


server <- function(input, output) {
  
  # Getting the data, and computing the variables needed (cases, deaths, vaccines)
  # using simple moving average for 7 days
  data <- reactive({
    
    data <- covid19(input$country1, level = 1)
    
    data <- data %>% 
      mutate(new_cases = confirmed - lag(confirmed, 1),
             new_deaths= deaths - lag(deaths, 1),
             new_cases_100k_hbt = round( (new_cases / population) * 100000, 4),
             new_deaths_100k_hbt = round( (new_deaths / population) * 100000, 4)) %>% 
      
      mutate(death_07da = zoo::rollmean(new_deaths, k = 7, fill = NA),
             death_07da_100k_hbt = zoo::rollmean(new_deaths_100k_hbt, k = 7, fill = NA),
             case_07da = zoo::rollmean(new_cases, k = 7, fill = NA),
             case_07da_100k_hbt = zoo::rollmean(new_cases_100k_hbt, k = 7, fill = NA)) %>% 
      mutate(vaccines = vaccines - lag(vaccines, 1),
             vaccines_07da = zoo::rollmean(vaccines, k = 7, fill = NA),
             vaccines_07da_100k_hbt = (vaccines_07da / population) * 100000 ) %>% 
      filter(date >= input$dateRange[1],
             date <= input$dateRange[2])
    
  })
  
 
  output$plot1 <- renderPlot({
    
    if (input$y == "rate per 100 000 hbt") {
      
      data() %>%
        ggplot( aes(x = date, y = case_07da_100k_hbt, color = id)) +
        geom_line() +
        labs(title = "Rolling average for the number of cases per 100 000 hbt (7 days)",
             x = "")
      
    } else {
      
      data() %>%
        ggplot( aes(x = date, y = case_07da, color = id)) +
        geom_line() +
        labs(title = "Rolling average for the number of cases (7 days)",
             x = "")
      
    }
  })
  
  

  
  output$plot2 <- renderPlot({
    
    
    if (input$y == "rate per 100 000 hbt") {
      
        data() %>%
          ggplot( aes(x = date, y = vaccines_07da_100k_hbt, color = id)) +
          geom_line() +
          labs(title = "Rolling average for the number of vaccines per 100 000 hbt (7 days)",
               x = "")
      
    } else {
      
        data() %>%
          ggplot( aes(x = date, y = vaccines_07da, color = id)) +
          geom_line() +
          labs(title = "Rolling average for the number of vaccines (7 days)",
               x = "")
    }
  
  })
  
  
  output$plot3 <- renderPlot({
    
    if (input$y == "rate per 100 000 hbt") {
      
      data() %>%
        ggplot( aes(x = date, y = death_07da_100k_hbt, color = id)) +
        geom_line() +
        labs(title = "Rolling average for the number of deaths per 100 000 hbt (7 days)",
             x = "")
      
    } else {
      
      data() %>%
        ggplot( aes(x = date, y = death_07da, color = id)) +
        geom_line() +
        labs(title = "Rolling average for the number of deaths (7 days)",
             x = "")
      
    }
  })
  
}
  
shinyApp(ui, server)