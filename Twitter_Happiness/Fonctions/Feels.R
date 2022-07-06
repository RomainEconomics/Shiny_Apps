
feels_plot <- function(data, emotions) {
  
  data <- data %>% 
    dplyr::filter(
      Date >= '2015-01-01', 
      name %in% emotions
    ) 
  
  confinements <- list(
    annotate("rect", 
             xmin = as.Date("2020-10-30"),
             xmax = as.Date("2020-12-15"),
             ymin = -Inf, ymax = +Inf,
             fill = "grey",
             alpha = 1, 
             color = "grey"),
    annotate("rect", 
             xmin = as.Date("2020-03-17"),
             xmax = as.Date("2020-05-12"),
             ymin = -Inf, ymax = +Inf,
             fill = "grey",
             alpha = 1, 
             color = "grey"),
    annotate("rect",
             xmin = as.Date("2021-04-05"),
             xmax = as.Date("2021-05-03"),
             ymin = -Inf, ymax = +Inf,
             fill = "grey",
             alpha = 1, 
             color = "grey"))
  
  
  GiletsJaunes <- list(
    annotate("rect", 
             xmin = as.Date("2018-11-15"),
             xmax = as.Date("2019-02-09"),
             ymin = -Inf, ymax = +Inf,
             fill = "grey",
             alpha = 1, 
             color = "grey")
  )
  

  max_value_gilets_jaunes <- data %>% 
    filter(Date >= '2018-01-30',
           Date <= '2019-12-30') %>% 
    summarise(max_value = max(value) * 1.5) %>% 
    pull(max_value)
  
  
  max_value_confinements <- data %>% 
    filter(Date >= '2020-02-28',
           Date <= '2021-09-01') %>% 
    summarise(max_value = max(value) * 1.4) %>% 
    pull(max_value)
  
  
  
  label <- data.frame(
    Date = c(as.Date('2018-12-30'),
             as.Date('2020-09-01')), 
    value = c(max_value_gilets_jaunes, max_value_confinements), 
    label = c("Gilets\nJaunes", 'Confinements')
  )
  
  
  
  data %>%
    ggplot() +
    GiletsJaunes + 
    confinements + 
    geom_line(aes(x = Date, y = value, fill = NULL,
                  color=name), size = 1.1) +
    geom_label(data = label, 
               aes(x = Date, y = value, label = label), 
               size = 5,
               vjust="inward")  +
    #scale_color_manual(values=c('#848884',"#808080",
    #                            '#696969',"#A9A9A9", "#36454F")) +
    scale_x_date(date_breaks = "1 year", 
                 date_labels = "%Y") +
    labs(x = '', y = 'Sentiment',
         caption = "Source : Observatoire du bien-être") +
    theme(legend.position="bottom",
          legend.margin=margin(t=-25),
          legend.title= element_blank(),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 14)) 
    #annotate("segment", 
    #         x = as.Date('2018-08-10'), 
    #         xend = as.Date('2018-11-15'), 
    #         y = 2, 
    #         yend = 2.2, 
    #         colour = "black", size=0.5)
}




