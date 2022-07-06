
daily_sentiment_plot <- function(data, plotly = TRUE) {
  
  
  #confinements <- list(
  #  annotate("rect", 
  #           xmin = as.Date("2020-10-30"),
  #           xmax = as.Date("2020-12-15"),
  #           ymin = -Inf, ymax = +Inf,
  #           fill = "grey",
  #           alpha = 1, 
  #           color = "grey"),
  #  annotate("rect", 
  #           xmin = as.Date("2020-03-17"),
  #           xmax = as.Date("2020-05-12"),
  #           ymin = -Inf, ymax = +Inf,
  #           fill = "grey",
  #           alpha = 1, 
  #           color = "grey"),
  #  annotate("rect",
  #           xmin = as.Date("2021-04-05"),
  #           xmax = as.Date("2021-05-03"),
  #           ymin = -Inf, ymax = +Inf,
  #           fill = "grey",
  #           alpha = 1, 
  #           color = "grey"))
  
  #label <- data.frame(
  #  date = c(as.Date('2013-08-21'),
  #           as.Date('2016-11-27'),
  #           as.Date('2017-05-27'),
  #           as.Date('2019-11-27'),
  #           as.Date('2021-05-27'),
  #           as.Date('2020-10-01')), 
  #  sentiment_m1 = c(-0.05, -0.068, -0.045, 0.049, -0.059, 0.025), 
  #  label = c("Attentat\nCharlie Hebdo", 'Attentat\ndu Bataclan',
  #            "Attentat\nà Nice", 'Victoire à la\nCoupe du Monde', 
  #            'Invasion\nRusse\nen Ukraine', 'Confinements')
  #)
  
  #confinements <- list(
  #  annotate("rect", 
  #           xmin = as.Date("2020-10-30"),
  #           xmax = as.Date("2020-12-15"),
  #           ymin = -Inf, ymax = +Inf,
  #           fill = "grey",
  #           alpha = 1, 
  #           color = "grey"),
  #  annotate("rect", 
  #           xmin = as.Date("2020-03-17"),
  #           xmax = as.Date("2020-05-12"),
  #           ymin = -Inf, ymax = +Inf,
  #           fill = "grey",
  #           alpha = 1, 
  #           color = "grey"),
  #  annotate("rect",
  #           xmin = as.Date("2021-04-05"),
  #           xmax = as.Date("2021-05-03"),
  #           ymin = -Inf, ymax = +Inf,
  #           fill = "grey",
  #           alpha = 1, 
  #           color = "grey"))
  #
  #
  #GiletsJaunes <- list(
  #  annotate("rect", 
  #           xmin = as.Date("2018-11-15"),
  #           xmax = as.Date("2019-02-09"),
  #           ymin = -Inf, ymax = +Inf,
  #           fill = "grey",
  #           alpha = 1, 
  #           color = "grey")
  #)
  #
  #label <- data.frame(
  #  date = c(as.Date('2018-12-30'),
  #           as.Date('2020-11-01')), 
  #  value = c(0.1, 0.1), 
  #  label = c("Gilets\nJaunes", 'Confinements')
  #)
    
  theme_set(theme_light(base_size=16))
  
  #data <- read_excel("Functions/CEPREMAP/Fonctions/Data/graph3_daily_twbi.xlsx") %>% 
  #  mutate(date = as.Date(date),
  #         sentiment = round(sentiment, 3))
  
  label <- data.frame(
      Date = c(as.Date('2015-01-07'),
               as.Date('2015-01-08'),
               as.Date('2015-11-14'),
               as.Date('2016-07-15'),
               as.Date('2018-07-16'),
               as.Date('2022-02-24'),
               as.Date('2020-03-17')), 
      label = c("Attentat Charlie Hebdo", "Lendemain attentat Charlie Hebdo", 
                'Attentat du Bataclan',
                "Attentat à Nice", 
                'Victoire à la Coupe du Monde', 
                'Invasion Russe en Ukraine', 'Confinements')
  )
  
  

  
  g <- data %>% 
    full_join(label, by = 'Date') %>% 
    ggplot() +
    #confinements + 
    #GiletsJaunes +
    geom_line(aes(x = Date, y = Sentiment), alpha = 0.85, color = 'darkgray') +
    geom_smooth(aes(x = Date, y = Sentiment), color = '#084594') +
    #geom_label(data = label, 
    #           aes(x = date, y = value, label = label), 
    #           size = 5,
    #           vjust="inward") +
    labs(x = '', y = 'Sentiment',
         caption = "Source : Observatoire du bien-être") + 
    scale_x_date(date_breaks = "2 year", 
                 date_labels = "%Y")
  
  if (plotly == TRUE) {
    return(ggplotly(g))
  } else {
    return(g + ggtitle('Indicateur Twitter du moral des Français - Fréquence journalière'))
  }
  
    #geom_label(data = label, 
    #           aes(x=date, y=sentiment_m1,label = label), 
    #           size = 3.5)  +
    #annotate("segment", 
    #         x = as.Date('2014-07-05'), 
    #         xend = as.Date('2015-01-10'), 
    #         y = -0.045, 
    #         yend = -0.051, 
    #         colour = "black", size=0.5) +
    #annotate("segment", 
    #         x = as.Date('2016-03-02'), 
    #         xend = as.Date('2015-11-15'), 
    #         y = -0.065, 
    #         yend = -0.071, 
    #         colour = "black", size=0.5) + 
    #annotate("segment", 
    #         x = as.Date('2016-07-14'), 
    #         xend = as.Date('2016-11-22'), 
    #         y = -0.056, 
    #         yend = -0.049, 
    #         colour = "black", size=0.5) +
    #annotate("segment", 
    #         x = as.Date('2018-07-15'), 
    #         xend = as.Date('2018-11-25'), 
    #         y = 0.052, 
    #         yend = 0.046, 
    #         colour = "black", size=0.5) +
    #annotate("segment", 
    #         x = as.Date('2022-02-24'), 
    #         xend = as.Date('2021-12-01'), 
    #         y = -0.039, 
    #         yend = -0.049, 
    #         colour = "black", size=0.5) +
    
  
  
}


