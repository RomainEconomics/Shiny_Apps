


# -------------------------------------------------------------------------

# Script partagé par les scripts UI et Server 
# Importe les packages, les données et effectue quelques filtrages 
source('share_load.R')


# -------------------------------------------------------------------------


# Importe les fonctions utilisées pour créer les graphiques
source('Fonctions/sentiment.R')
source('Fonctions/Feels.R')
source('Fonctions/medias.R')
source('Fonctions/genre.R')
source('Fonctions/pref_politiques.R')


# -------------------------------------------------------------------------


# Fonction pour créer un bouton permettant le téléchargemet des graphiques
# 2 paramètres : un graphique et le nom du fichier voulu
download_helper_plot <- function(render_plot_output, file_name) {
    
    downloadHandler(
        filename = function() { paste0(file_name, '.png') },
        content = function(file) {
            ggsave(file, plot = render_plot_output,
                   width = 12, height = 8,
                   device = "png")
        }
    )
}


# Fonction pour créer un bouton permettant le téléchargemet des données d'un graphique
# 2 paramètres : un graphique et le nom du fichier voulu
download_helper_dataset <- function(dataset, file_name) {
    
    downloadHandler(
        filename = function() { paste0(file_name, '.csv') },
        content = function(file) {
            write.csv(dataset, file, row.names = FALSE)
        }
    )
}



# -------------------------------------------------------------------------




shinyServer(function(input, output, session) {

  

# -------------------------------------------------------------------------

    # La même logique est utilisée à chaque fois 
    #
    # 1. Création de l'output (un graphique, soit avec plotly pour le premier, ensuite ggplot)
    #    L'output est ensuite utilisé dans le fichier ui.R pour définir l'endroit ou celui-ci sera affiché.
    # 2. Deux boutons sont créés à chaque fois pour mettre le téléchargement des graphiques et des données.
    #    De la même façon, un output est créé pour chaque bouton et ensuite utilisé dans le fichier ui.R          
  
# -------------------------------------------------------------------------

  
  

# Onglet Sentiment --------------------------------------------------------


  
    # Noter l'utilisation de Plotly (graph "interactif") pour le rendu Shiny
    output$daily_sentiment_plot <- renderPlotly({
        daily_sentiment_plot(data_sentiment)
    })
    
    # Noter l'utilisation de l'argument "plotly = False" lors du téléchargement
    # du graphique afin d'obtenir un graphique provenant non plus de plotly mais de ggplot
    output$downloadPlot_sentiment <- download_helper_plot(daily_sentiment_plot(data_sentiment, plotly = FALSE),'plot_sentiment')
    output$downloadDataset_sentiment <- download_helper_dataset(data_sentiment, 'data_sentiment')
    
    


# Onglet Émotions ---------------------------------------------------------


    output$feels_plot <- renderPlot({
        feels_plot(data_emotions, input$emotion_input)
    })
    
    
    output$downloadPlot_emotions <- download_helper_plot(feels_plot(data_emotions, input$emotion_input),'plot_emotions')
    output$downloadDataset_emotions <- download_helper_dataset(data_emotions, 'data_emotions')
    




# Onglet Genre ------------------------------------------------------------

    
    output$genre_plot <- renderPlot({
        genre_plot(data_genre)
    })
    
    
    output$downloadPlot_genre <- download_helper_plot(genre_plot(data_genre),'plot_genre')
    output$downloadDataset_genre <- download_helper_dataset(data_genre, 'data_genre')

    


# Onglet Préférences Politiques -------------------------------------------


    output$sentiment_pref_pol_plot <- renderPlot({
        sentiment_pref_pol_plot(data_pref_pol, input$parti_pol_input)
    })
    
    
    output$downloadPlot_pref_pol <- download_helper_plot(sentiment_pref_pol_plot(data_pref_pol, input$parti_pol_input),'plot_pref_pol')
    output$downloadDataset_pref_pol <- download_helper_dataset(data_pref_pol, 'data_pref_pol')    
    
  


# -------------------------------------------------------------------------


# Onglet Médias -----------------------------------------------------------



# Sous-Onglet Sentiment ---------------------------------------------------

    

    output$tri_sentiment_medias_plot <- renderPlot({
        tri_sentiment_medias_plot(data_medias, 
                                  c(input$medias_web,
                                    input$medias_national,
                                    input$medias_hebdo))

})

output$downloadPlot_medias <- download_helper_plot(tri_sentiment_medias_plot(data_medias, input$medias_input_web),'plot_medias')
output$downloadDataset_medias <- download_helper_dataset(data_medias, 'data_medias')




# Sous-Onglet Barplot -----------------------------------------------------



output$medias_barplot <- renderPlot({
  medias_barplot(data_medias_barplot, c(input$barplot_medias_web,
                                        input$barplot_medias_national,
                                        input$barplot_medias_hebdo))
})

output$downloadPlot_medias_barplot <- download_helper_plot(medias_barplot(data_medias_barplot),'barplot_medias')
output$downloadDataset_medias_barplot <- download_helper_dataset(data_medias_barplot, 'data_medias_barplot')




output$medias_type_barplot <- renderPlot({
  medias_type_barplot(data_medias_type_barplot)
})

output$downloadPlot_medias_type_barplot <- download_helper_plot(medias_type_barplot(data_medias_type_barplot),'barplot_type_medias')
output$downloadDataset_medias_type_barplot <- download_helper_dataset(data_medias_type_barplot, 'data_medias_type_barplot')

    
}
)

