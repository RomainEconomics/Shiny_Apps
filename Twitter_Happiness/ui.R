

# -------------------------------------------------------------------------


# UI  ---------------------------------------------------------------------


# -------------------------------------------------------------------------




# -------------------------------------------------------------------------

# Script partagé par les scripts UI et Server 
# Importe les packages, les données et effectue quelques filtrages 
source('share_load.R')


# -------------------------------------------------------------------------

# Identifie les médias par type et les range par ordre alphabétique
# Utilisé pour l'onglet média.
unique_medias <- function(data, type) {
    
    res <- unique(data %>% 
                      filter(Type == type) %>% 
                      pull(Media) %>% 
                      str_sort())
    return(res)
}



ui <- navbarPage(
    # Titre de l'application
    title = "Tableau de bord du bien-être en France",   

# Onglet Sentiment --------------------------------------------------------
    
    tabPanel(title = "Sentiment", value = 'sentiment',
             fluidPage(
                 sidebarLayout(
                     
                     # Rectangle à gauche contenant les boutons de téléchargement
                     sidebarPanel(
                         # Chaque graphique possède deux boutons de téléchargement, pour obtenir le graphique 
                         # et les données. Le premier élément de la fonction renvoie à l'output générer dans le 
                         # script server.R
                         downloadButton('downloadPlot_sentiment', label = 'Télécharger le graphique'),
                         br(), # br = code HTML pour créer un espace vectical entre deux éléments
                         br(),
                         downloadButton('downloadDataset_sentiment', label = 'Télécharger les données')),
                     
                     # Page principale
                     mainPanel(
                         h3('Indicateur Twitter du moral des Français - Fréquence journalière'),
                         # Noter ici l'utilisation de Plotly et non ggplot
                         plotlyOutput("daily_sentiment_plot"),
                         # Partie texte et explication
                         h3("Principaux Résultats"),
                         p("À partir de l'analyse du score de sentiment moyen journalier de notre échantillon, et en considérant ici l'écart à la moyenne, 
                           on peut mettre en évidence deux grandes périodes. Entre 2015 et le premier semestre 2018,
                           le sentiment moyen est supérieur à la moyenne. A partir du deuxième semestre 2018 (date du déclenchement
                           du mouvement des gilets jaunes) et jusqu'à aujourd'hui, ce score reste inférieur à la moyenne de notre échantillon."),
                         p("On peut également noter l'impact des attentats (ceux de Charlie Hebdo, du Bataclan et de Nice notamment) 
                           sur le score de sentiment. Ceux-ci correspondent aux scores les plus faibles de notre échantillon. Le score 
                           de sentiment le plus élevé, quant à lui, correspond à la victoire de l'équipe de France lors de la coupe du monde
                           de Football en 2018."),
                         h3("Construction de l'indicateur"),
                         p("La méthode VADER, spécifique à l’analyse de sentiment des messages publiés sur les réseaux 
                                 sociaux et dont une implémentation pour l’analyse de tweets en français existe depuis mai 2020.
                                 Cette méthode présente l’avantage de prendre en compte les emojis, les négations, la casse et 
                                 la ponctuation – en plus des mots contenus dans les tweets – afin de calculer un score de sentiment 
                                 pour chaque message. Pour chaque tweet, nous utilisons le score de sentiment de VADER afin de 
                                 calculer pour chaque message un score entre -1 et +1 (de très négatif à très positif)."),
                         # ul : unordered list
                         # Permet de créer des bullet points
                         tags$ul(
                             tags$li("Package VADER"),
                             tags$li("C. Hutto et E. Gilbert, « VADER : A parsimonious rule-based model for senti- ment analysis of social media text », 2014."),
                             tags$li('https://pypi.org/project/vaderSentiment-fr/'))
                     )
                 )
             )
    ),

# Onglet Émotions ---------------------------------------------------------

    tabPanel(title = "Émotions", 
             fluidPage(
                 sidebarLayout(
                     
                     sidebarPanel(
                         # checkboxGroupButtons est une fonction du package : shinyWidgets
                         checkboxGroupButtons("emotion_input", "Choisissez les émotions à afficher :", 
                                     unique(data_emotions$name), selected = c('Tristesse', 'Joie')),
                         downloadButton('downloadPlot_emotions', label = 'Télécharger le graphique'),
                         br(),
                         br(),
                         downloadButton('downloadDataset_emotions', label = 'Télécharger les données')),
                     
                     mainPanel(
                         h3('Évolution des indicateurs d’émotion - Fréquence mensuelle'),
                         plotOutput("feels_plot"),
                         h3("Principaux Résultats"),
                         p("Le sentiment de joie atteint son niveau maximum au début de l'année 2017 et connait
                           ensuite une diminution nette au cours du temps pour atteindre un minimum pendant la periode du Covid-19."),
                         p("Le sentiment de tristesse suit une tendance parfaitement inverse. Un minimum est atteint
                           au cours de l'année 2016 pour ensuite augmenter et atteindre un maximum lors du premier confinement."),
                         h3("Construction de l'indicateur"),
                         p("La méthode FEEL – French Expended Emotions Lexicon – permet de classifier chaque message selon les six émotions : la joie, la colère, la peur, la tristesse, le dégoût et la surprise."),
                         tags$ul(
                                tags$li("Package Feels"),
                                tags$li("A. Abdaoui, J. Azé, S. Bringay et P. Poncelet, « FEEL : A French expanded emotion lexicon », Language Resources & Evaluation, 51 (1), 2017."),
                                tags$li('https://github.com/AdilZouitine/pyFeel'))
                     )
                 )
             )
    ),

# Onglet Genre ------------------------------------------------------------


    tabPanel(title = "Genre", 
             fluidPage(
                 sidebarLayout(
                     
                     sidebarPanel(
                         downloadButton('downloadPlot_genre', label = 'Télécharger le graphique'),
                         br(),
                         br(),
                         downloadButton('downloadDataset_genre', label = 'Télécharger les données')),
                     
                     mainPanel(
                         h3('Indicateur Twitter du moral des Français selon le genre - Fréquence mensuelle'),
                         plotOutput("genre_plot"),
                         h3("Principaux Résultats"),
                         p("Avant 2018, le score de sentiment pour les hommes et les femmes est sensiblement identique.
                           Cependant, après cette date, qui correspond aussi à la période du déclenchement du mouvement #MeToo sur Twitter
                           , on observe un score de sentiment plus élevé pour les hommes que pour les femmes. En outre, les femmes semblent 
                           avoir été plus affectées par les deux premiers confinements que les hommes. Leur sentiment déclaré se détériore plus que celui de ces derniers.
                           On peut d'ailleurs noter que le point le plus haut de notre échantillon se produit durant la période
                           qui suit la victoire en 2018 de l'équipe de France à la coupe du monde de Football."),
                         h3("Construction de l'indicateur"),
                         p("Une nouvelle fois, la méthode Vader a été utilisée pour calculer le sentiment moyen. 
                           Pour identifier le genre des utilisateurs de Twitter, nous nous sommes limités aux personnes
                           déclarant un prénom, et avons classifié celui-ci à l’aide d’une liste de 11 627 prénoms identifiés 
                           comme féminins et masculins."),
                         tags$ul(
                             tags$li("Package VADER"),
                             tags$li("La base des prénoms utilisée : https://www.data.gouv.fr/fr/datasets/liste-de-prenoms/"),
                             tags$li("C. Hutto et E. Gilbert, « VADER : A parsimonious rule-based model for senti- ment analysis of social media text », 2014."),
                             tags$li('https://pypi.org/project/vaderSentiment-fr/'))
                     )
                 )
             )
    ),


# Onglet Préférences Politiques -------------------------------------------


    tabPanel(title = "Préférences Politiques", 
             fluidPage(
                 sidebarLayout(
                     
                     sidebarPanel(
                         checkboxGroupButtons("parti_pol_input", "Choisissez les orientations politiques à afficher :", 
                                              c("Extrême Gauche", "Gauche", 'Centre', 'Droite', 'Extrême Droite'), 
                                              selected = 'Droite'),
                         downloadButton('downloadPlot_pref_pol', label = 'Télécharger le graphique'),
                         br(),
                         br(),
                         downloadButton('downloadDataset_pref_pol', label = 'Télécharger les données')),
                     
                     mainPanel(
                         h3("Indicateur Twitter du moral des Français selon l'orientation politique - Fréquence mensuelle"),
                         plotOutput("sentiment_pref_pol_plot"),
                         h3("Principaux Résultats"),
                         p(''),
                         h3("Construction de l'indicateur"),
                         p("Notre indicateur est construit en analysant rétrospectivement les messages des personnes suivant 
                            – aujourd’hui – un ensemble de comptes politiques. Une limite de cette méthode est que nous classifions 
                            un utilisateur, et donc l’ensemble de ses messages passés, en fonction de ses abonnements actuels.
                            Cela nous conduit certainement à mal classifier les personnes dont l’orientation politique a évolué au
                            cours du temps, mais à cette échelle de temps, les orientations des utilisateurs sont certainement 
                            suffisamment stables pour que les abonnements actuels reflètent  une orientation politique latente.
                            Nous sommes évidemment conscients que le fait de suivre un compte
                            n’implique pas toujours une adhésion aux idées exprimées par ce compte. Il semble cependant plausible 
                            que les personnes abonnées au compte officiel d’une personnalité politique soient majoritairement des 
                            personnes proches des idées de cette dernière. Nous nous concentrons uniquement sur les utilisateurs 
                            qui suivent au moins deux politiciens afin de diminuer le bruit lié, par exemple, au fait que plusieurs
                            millions d’utilisateurs suivent le compte d’Emmanuel Macron sans nécessairement soutenir ce dernier. 
                            Nous relions ensuite un utilisateur à une préférence politique seulement si, parmi l’ensemble des 
                            comptes politiques qu’il suit, au moins 50 % appartiennent à une même tendance politique.
                            Cette approche permet de représenter la manière dont, au sein de la population française, 
                            les émotions exprimées diffèrent en fonction des préférences politiques identifiées à partir des comptes suivis."),
                         tags$ul(
                             tags$li("Package VADER"),
                             tags$li("C. Hutto et E. Gilbert, « VADER : A parsimonious rule-based model for sentiment analysis of social media text », 2014."),
                             tags$li('https://pypi.org/project/vaderSentiment-fr/'))
                     )
                 )
             )
    ),

# Onglet Médias -----------------------------------------------------------


    navbarMenu("Médias", 
               tabPanel(title = "Sentiment", 
                        fluidPage(
                            sidebarLayout(
                                
                                sidebarPanel(
                                    # unique_medias est une fonction créée pour identifier les médias par type (voir début du script)
                                    checkboxGroupButtons("medias_web", "Choisissez les médias web à afficher :", 
                                                         unique_medias(data_medias, 'Web'), 
                                                         selected = 'Mediapart'),
                                    checkboxGroupButtons("medias_national", "Choisissez les médias nationaux à afficher :", 
                                                         unique_medias(data_medias, 'Quotidien national'), 
                                                         selected = 'Le Monde'),
                                    checkboxGroupButtons("medias_hebdo", "Choisissez les médias hebdomaires à afficher :", 
                                                         unique_medias(data_medias, 'Hebdomadaire info'), 
                                                         selected = 'Valeurs actuelles'),
                                    downloadButton('downloadPlot_medias', label = 'Télécharger le graphique'),
                                    br(),
                                    br(),
                                    downloadButton('downloadDataset_medias', label = 'Télécharger les données')),
                                
                                mainPanel(
                                    h3('Évolution des indicateurs de sentiment par média - Fréquence mensuelle'),
                                    plotOutput("tri_sentiment_medias_plot"),
                                    h3("Principaux Résultats"),
                                    p(''),
                                    h3("Construction de l'indicateur"),
                                    p("La méthode VADER, spécifique à l’analyse de sentiment des messages publiés sur les réseaux 
                                      sociaux et dont une implémentation pour l’analyse de tweets en français existe depuis mai 2020.
                                      Cette méthode présente l’avantage de prendre en compte les emojis, les négations, la casse et 
                                      la ponctuation – en plus des mots contenus dans les tweets – afin de calculer un score de sentiment 
                                      pour chaque message. Pour chaque tweet, nous utilisons le score de sentiment de VADER afin de 
                                      calculer pour chaque message un score entre -1 et +1 (de très négatif à très positif)."),
                                    tags$ul(
                                        tags$li("Package VADER"),
                                        tags$li("C. Hutto et E. Gilbert, « VADER : A parsimonious rule-based model for senti- ment analysis of social media text », 2014."),
                                        tags$li('https://pypi.org/project/vaderSentiment-fr/'))
                                )
                            )
                        )
               ),
               tabPanel(title = "Barplot", 
                        fluidPage(
                            
                            fluidRow(
                            sidebarLayout(
                                
                                sidebarPanel(
                                    checkboxGroupButtons("barplot_medias_web", "Choisissez les médias web à afficher :", 
                                                         unique_medias(data_medias, 'Web'), 
                                                         selected = unique_medias(data_medias, 'Web')),
                                    checkboxGroupButtons("barplot_medias_national", "Choisissez les médias nationaux à afficher :", 
                                                         unique_medias(data_medias, 'Quotidien national'),
                                                         selected = unique_medias(data_medias, 'Quotidien national')),
                                    checkboxGroupButtons("barplot_medias_hebdo", "Choisissez les médias hebdomaires à afficher :", 
                                                         unique_medias(data_medias, 'Hebdomadaire info'), 
                                                         selected = unique_medias(data_medias, 'Hebdomadaire info')),
                                    downloadButton('downloadPlot_medias_barplot', label = 'Télécharger le graphique'),
                                    br(),
                                    br(),
                                    downloadButton('downloadDataset_medias_barplot', label = 'Télécharger les données')),
                                
                                mainPanel(
                                    h3('Indice moyen de sentiment par média'),
                                    plotOutput("medias_barplot")
                                )
                            )
                            
                            ),
                            br(),
                            br(),
                            fluidRow(
                                
                                tabPanel(title = "Barplot par type de médias", 
                                         fluidPage(
                                             sidebarLayout(
                                                 
                                                 sidebarPanel(
                                                     downloadButton('downloadPlot_medias_type_barplot', label = 'Télécharger le graphique'),
                                                     br(),
                                                     br(),
                                                     downloadButton('downloadDataset_medias_type_barplot', label = 'Télécharger les données')),
                                                 
                                                 mainPanel(
                                                     h3('Indice moyen de sentiment par type de média'),
                                                     plotOutput("medias_type_barplot"),
                                                     h3("Principaux Résultats"),
                                                     p(''),
                                                     h3("Construction de l'indicateur"),
                                                     p("La méthode VADER, spécifique à l’analyse de sentiment des messages publiés sur les réseaux 
                                                       sociaux et dont une implémentation pour l’analyse de tweets en français existe depuis mai 2020.
                                                       Cette méthode présente l’avantage de prendre en compte les emojis, les négations, la casse et 
                                                       la ponctuation – en plus des mots contenus dans les tweets – afin de calculer un score de sentiment 
                                                       pour chaque message. Pour chaque tweet, nous utilisons le score de sentiment de VADER afin de 
                                                       calculer pour chaque message un score entre -1 et +1 (de très négatif à très positif)."),
                                                     tags$ul(
                                                         tags$li("Package VADER"),
                                                         tags$li("C. Hutto et E. Gilbert, « VADER : A parsimonious rule-based model for senti- ment analysis of social media text », 2014."),
                                                         tags$li('https://pypi.org/project/vaderSentiment-fr/'))
                                                 )
                                             )
                                         )
                                )
                                
                            )
                        )
               )
    )
)
