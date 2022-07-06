library(rdbnomics)
library(shiny)
library(tidyverse)
library(RColorBrewer)
library(readxl)
library(extrafont)
library(scales)
library(tsibble)
library(plotly)
library(shinyWidgets)
library(plotrix)
library(zoo)
library(rsconnect)




# Importation des données -------------------------------------------------


path <- 'Data/'

files_list <- list.files(path = path, pattern = ".tsv")

extract_series <- function(file_names) {
  file_names %>% 
    map(~read_tsv(paste0(path, .), 
                  col_names = c('Date', str_extract(., '(?<=SENT\\.).+(?=\\.tsv)')), 
                  skip = 1,
                  show_col_types = FALSE)) %>% 
    reduce(left_join, by = 'Date') 
}




# Sentiment ---------------------------------------------------------------

data_sentiment <- read_tsv(paste0(path, "D.SENT.ALL.tsv"), show_col_types = FALSE) %>% 
  rename(Date = PERIOD, Sentiment = VALUE) %>% 
  mutate(Sentiment = round(Sentiment, 3))


# Émotions ----------------------------------------------------------------

émotions <- c('JOY|SADNESS|FEAR|ANGER')

data_emotions <- files_list[str_detect(files_list, '^M') & str_detect(files_list, émotions)] %>% 
  map(~read_tsv(paste0(path, .), 
                col_names = c('Date', str_extract(., '(?<=M\\.).+(?=\\.ALL\\.tsv)')), 
                skip = 1,
                show_col_types = FALSE)) %>% 
  reduce( left_join, by = 'Date') %>% 
  rename(Tristesse = SADNESS, Peur = FEAR, 
         Joie = JOY, Colère = ANGER) %>% 
  pivot_longer(-Date)


# Orientations Politiques -------------------------------------------------


orientations_pol <- c('EXTREMEGAUCHE|GAUCHE|CENTRE|DROITE|EXTREMEDROITE')

data_pref_pol <- files_list[str_detect(files_list, orientations_pol)] %>% 
  extract_series() %>% 
  rename_with(str_to_title) %>% 
  rename(`Extrême Droite` = Extremedroite,
         `Extrême Gauche` = Extremegauche) %>% 
  mutate_if(is.double, ~ rollmean(., k = 3, fill = NA)) %>% 
  pivot_longer(-Date) %>%
  drop_na() 






# Genre -------------------------------------------------------------------

genre <- c('WOMEN|MEN')

data_genre <- files_list[str_detect(files_list, genre)] %>% 
  extract_series() %>%
  rename(Femme = WOMEN,
         Homme = MEN) %>% 
  pivot_longer(-Date) 


# Médias ------------------------------------------------------------------

#path <- 'Functions/CEPREMAP/Fonctions/TWTEMO/'

#files_list <- list.files(path = path, pattern = ".tsv")


data_medias <- files_list[!( str_detect(files_list, paste0(orientations_pol, c('|ALL|WOMEN|MEN'))))] %>% 
  extract_series() %>% 
  filter(Date >= '2016-01-01') %>% 
  pivot_longer(-Date, values_to = "Sentiment", names_to = "Media") %>% 
  mutate(
    Type = case_when(
      Media %in% c("HUMANITEFR",
                   "LEFIGARO",
                   "LEMONDEFR",
                   "lequipe",
                   "LESECHOS",
                   "LIBE",
                   "LACROIX"
      ) ~ "Quotidien national",
      Media %in% c("laprovence",
                   "lavoixdunord",
                   "le_parisien",
                   "ouestfrance", 
                   'lamontagne_fr',
                   "sudouest",
                   'ladepechedumidi',
                   'nice_matin'
      ) ~ "PQR",
      Media %in% c("canardenchaine",
                   "challenges",
                   "courrierinter",
                   "lejdd",                       
                   "lepoint",
                   "lexpress",
                   "lobs",
                   "VALEURS",
                   'mariannelemag'
      ) ~ "Hebdomadaire info",
      Media %in% c("ellefrance",
                   "madamefigaro", 
                   "parismatch",
                   "teleloisirs", 
                   'voici',
                   'mariefrancemag',
                   "telerama"
      ) ~ "Hebdomadaire loisirs",
      Media %in% c("HUGODECRYPTE",
                   "THINKERVIEW",
                   "KONBINI",
                   'MEDIAPART',
                   'lesjoursfr') ~ "Web",
      Media %in% c("sciences_avenir", "sofoot", "voguefrance", "lesinrocks") ~ "Mensuel", 
      Media %in% c("qofficiel") ~ "TV")) %>% 
  mutate(Media = recode(Media, 
                        "HUMANITEFR" = "L'Humanité",
                        "LEFIGARO" = "Le Figaro",
                        "LEMONDEFR" = "Le Monde",
                        "lequipe" = "L'Équipe",
                        "LESECHOS" = "Les Échos",
                        "LIBE" = "Libération",
                        'LACROIX' = 'La Croix', 
                        "laprovence" = "La Provence",
                        "lavoixdunord" = "La Voix du Nord",
                        "le_parisien" = "Le Parisien",
                        "ouestfrance" = "Ouest-France", 
                        "sudouest" = "Sud-Ouest",
                        'lamontagne_fr' = 'La Montagne',
                        'lestrepublicain' = "L'Est Républicain",
                        'ladepechedumidi' = 'La Dépêche du Midi',
                        "canardenchaine" = "Le Canard enchaîné",
                        'nice_matin' = 'Nice Matin',
                        "challenges" = "Challenges",
                        "courrierinter" = "Courrier International",
                        "lejdd" = "Le JdD",              
                        "lepoint" = "Le Point",
                        "lexpress" = "L'Express",
                        "lobs" = "L'Obs",
                        "VALEURS" = "Valeurs actuelles",
                        'tele7' = 'Télé 7 Jours',
                        "ellefrance" = "Elle",
                        'voici' = 'Voici',
                        'mariefrancemag' = 'Marie France',
                        "madamefigaro" = "Figaro Madame", 
                        "parismatch" = "Paris Match",
                        "teleloisirs" = "Télé loisirs", 
                        "telerama" = "Télérama", 
                        "HUGODECRYPTE" = "Hugo décrypte", 
                        "THINKERVIEW" = "Thinker View", 
                        "sciences_avenir" = "Sciences et avenir", 
                        "sofoot" = "So Foot", 
                        "voguefrance" = "Vogue", 
                        "lesinrocks" = "Les Inrockuptibles", 
                        "qofficiel" = "Quotidien",
                        'MEDIAPART' = 'Mediapart',
                        'lesjoursfr' = 'Les Jours',
                        'mariannelemag' = 'Marianne',
                        'KONBINI' = 'Konbini')) %>% 
  drop_na() 


Moyenne <- data_medias %>% 
  group_by(Media) %>% 
  summarise(Moyenne = mean(Sentiment))

data_medias <- data_medias %>% 
  full_join(Moyenne, by = "Media")


data_medias_barplot <- data_medias %>% 
  group_by(Media) %>% 
  summarise(Moyenne = mean(Sentiment), 
            se = std.error(Sentiment)) %>% 
  left_join(data_medias %>% select(Media, Type) %>% distinct(), by = 'Media')


data_medias_type_barplot <- data_medias %>% 
  group_by(Type) %>% 
  summarise(Moyenne = mean(Sentiment), 
            se = std.error(Sentiment))

