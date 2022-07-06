

tri_sentiment_medias_plot <- function(data, medias) {
  

  data <- data %>%
    filter(Media %in% medias) 
    
    
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
    summarise(max_value = max(Sentiment) * 1.2) %>% 
    pull(max_value)
  
  
  max_value_confinements <- data %>% 
    filter(Date >= '2020-02-28',
           Date <= '2021-09-01') %>% 
    summarise(max_value = max(Sentiment) * 1.2) %>% 
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
    geom_line(aes(x = Date, y = Sentiment, color = Media), size = 1.1) +
    geom_label(data = label, 
               aes(x = date, y = value, label = label), 
               size = 5,
               vjust="inward") +
    scale_x_date(date_breaks = "years", date_labels = "%Y") +
    theme(legend.position = "bottom", 
          legend.title = element_blank(),
          text = element_text(size = 16),
          axis.title.x=element_blank()) +
    labs(#title = "Sélection de médias",
      x = "", 
      y = "Émotion moyenne par trimestre",
      caption = "Source : Observatoire du bien-être",
      color = 'Type',
      linetype = 'Type') 
}



medias_barplot <- function(data, medias) {
  
  data <- data %>% 
    filter(Media %in% medias)
  
  types <- data %>% distinct(Type) %>% pull()
  
  
  # For the colors see: brewer.pal(n = 8, name = "Blues")
  type_colors <- tibble('Hebdomadaire loisirs' = '#084594',
                        'Quotidien national' = "#DEEBF7",
                        'Hebdomadaire info' = "#9ECAE1",
                        'Web' = "#4292C6")
  
  # Ggplot demande d'avoir un "named vector"
  # On filtre en ne retenant que les partis choisis par l'utilisateur.
  # Deframe transforme un dataframe en un named vector
  type_colors_filtered <- map_chr(type_colors[types], deframe)
  
  
  # data %>% 
  #   ggplot(aes(x = reorder(Media, -Moyenne), 
  #              y = Moyenne,
  #              ymin = Moyenne - se,
  #              ymax = Moyenne + se,
  #              fill = Type)) + 
  #   theme_minimal() +
  #   geom_bar(stat = "identity") +
  #   scale_fill_manual(values = type_colors_filtered) +
  #   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), 
  #         legend.position = "bottom",
  #         text = element_text(size = 16),
  #         axis.title.x=element_blank()) +
  #   labs(#title = "Indice moyen de sentiment", 
  #     x = "", 
  #     y = "",
  #     fill='',
  #     caption = "Source : Observatoire du bien-être") 
  
  data %>% 
    ggplot() + 
    theme_minimal() +
    geom_bar(aes(x = reorder(Media, -Moyenne), y = Moyenne, fill = Type), stat = "identity") +
    geom_errorbar(aes(x = reorder(Media, -Moyenne), ymin = Moyenne - se, ymax = Moyenne + se), width=0.4, colour="#e30202", alpha=0.9, size=1.1) +
    scale_fill_manual(values = type_colors_filtered) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), 
          legend.position = "bottom",
          text = element_text(size = 16),
          axis.title.x=element_blank()) +
    labs(#title = "Indice moyen de sentiment", 
      x = "", 
      y = "",
      fill='',
      caption = "Source : Observatoire du bien-être") 
  
  
}




medias_type_barplot <- function(data) {
  
  
  types <- data %>% distinct(Type) %>% pull()
  
  
  # For the colors see: brewer.pal(n = 8, name = "Blues")
  type_colors <- tibble('Hebdomadaire loisirs' = '#084594',
                        'Quotidien national' = "#DEEBF7",
                        'Hebdomadaire info' = "#9ECAE1",
                        'Web' = "#4292C6") %>% 
    map_chr(deframe)
  
  data %>% 
    ggplot(aes(x = reorder(Type, -Moyenne), 
             y = Moyenne,
             ymin = Moyenne - se,
             ymax = Moyenne + se,
             fill = Type)) + 
    theme_minimal() +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), 
          legend.position = "none",
          text = element_text(size = 16)) +
    scale_fill_manual(values = type_colors) +
    labs(#title = "Indice moyen de sentiment", 
      x = "", 
      y = "",
      caption = "Source : Observatoire du bien-être")
  
  
}


