
# Script utilisé pour déployer l'application la premiere fois.
# Ensuite, pour la mettre à jour, aller sur le fichier ui.R ou server.R
# et utiliser le petit bouton bleu à coter du bouton "Run app" (ou "Reload App")

library(rsconnect)
rsconnect::deployApp()
