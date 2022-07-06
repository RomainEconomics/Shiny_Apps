
genre_plot <- function(data) {
  
  
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
  

  coupe_du_monde <- list(
    annotate("rect", 
             xmin = as.Date("2018-06-01"),
             xmax = as.Date("2018-07-10"),
             ymin = -Inf, ymax = +Inf,
             fill = "grey",
             alpha = 1, 
             color = "grey"))
  
  
  metoo <- list(
    annotate("rect", 
             xmin = as.Date("2017-11-15"),
             xmax = as.Date("2017-11-22"),
             ymin = -Inf, ymax = +Inf,
             fill = "grey",
             alpha = 1, 
             color = "grey")
  )
  
  
  max_value_cdm <- data %>% 
    filter(Date >= '2018-01-01',
           Date <= '2018-12-30') %>% 
    summarise(max_value = max(value) * 1.2) %>% 
    pull(max_value)
  
  max_value_metoo <- data %>% 
    filter(Date >= '2017-02-28',
           Date <= '2018-05-01') %>% 
    summarise(max_value = min(value) * 0.7) %>% 
    pull(max_value)
  
  max_value_confinements <- data %>% 
    filter(Date >= '2020-02-28',
           Date <= '2021-09-01') %>% 
    summarise(max_value = max(value) * 1.2) %>% 
    pull(max_value)
  
  
  
  label <- data.frame(
    date = c(as.Date('2018-06-30'),
             as.Date('2017-10-01'),
             as.Date('2020-09-30')), 
    value = c(max_value_cdm, max_value_metoo, max_value_confinements), 
    label = c("Coupe du\nMonde de \nFootball", 'Début du \nMouvement\n#MeToo',
              'Confinements')
  )
  
  
  
  data %>% 
    ggplot() +
    coupe_du_monde +
    metoo +
    confinements +
    geom_line(aes(x = Date, y = value, color = name), size = 1.1) + 
    scale_x_date(date_breaks = "2 year", 
                 date_labels = "%Y") +
    geom_label(data = label, 
               aes(x = date, y = value, label = label), 
               size = 5,
               vjust="inward")  +
    labs(x = '', y = 'Sentiment',
         caption = "Source : Observatoire du bien-être") +
    theme(legend.position="bottom",
          legend.title= element_blank(),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14),
          legend.margin=margin(t=-25))  

}


