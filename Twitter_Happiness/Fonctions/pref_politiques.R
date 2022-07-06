


sentiment_pref_pol_plot <- function(data, parti_pol) {
  
  
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
 
  
  
  data <- data %>% 
    filter(name %in% parti_pol) %>% 
    mutate(name = factor(name, level = parti_pol))
  
  
  party_colors <- tibble('Extrême Gauche' = "red",
                         'Gauche' = "deeppink1",
                         'Centre' = "lightskyblue4",
                         'Droite' = 'dodgerblue1',
                         'Extrême Droite' = 'dodgerblue4')
  
  # Ggplot demande d'avoir un "named vector"
  # On filtre en ne retenant que les partis choisis par l'utilisateur.
  # Deframe transforme un dataframe en un named vector
  party_colors_filtered <- map_chr(party_colors[parti_pol], deframe)
  
  
  max_value_gilets_jaunes <- data %>% 
    filter(Date >= '2017-10-30',
           Date <= '2018-09-30') %>% 
    summarise(max_value = max(value) * 1.1) %>% 
    pull(max_value)
  
  
  max_value_confinements <- data %>% 
    filter(Date >= '2020-02-28',
           Date <= '2021-09-01') %>% 
    summarise(max_value = max(value) * 1.4) %>% 
    pull(max_value)
  
  
  
  label <- data.frame(
    date = c(as.Date('2018-12-30'),
             as.Date('2020-11-01')), 
    value = c(max_value_gilets_jaunes, max_value_confinements), 
    label = c("Gilets\nJaunes", 'Confinements')
  )
  
  
  
  data %>% 
    ggplot() +
    GiletsJaunes + 
    confinements + 
    geom_line(aes(x = Date, y = value, color = name), 
              alpha = 0.7, size = 1.1) +
    scale_x_date(date_breaks = "2 year", 
                 date_labels = "%Y") +
    labs(x = '', y = 'Sentiment',
         caption = "Source : Observatoire du bien-être") +
    geom_label(data = label, 
               aes(x = date, y = value, label = label), 
               size = 4.5,
               vjust="inward")  +
    scale_color_manual(values = party_colors_filtered) +
    theme(legend.position="bottom",
          legend.title= element_blank(),
          legend.margin = margin(t = -25),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 12)) 
  
  
  
}







