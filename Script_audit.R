# Script pour générer le rapport d'audit sur les données de l'éducation - data.education.gouv.fr

# Librairies
library(tidyverse)
library(summarytools)
library(explore) #install.packages("explore")

# Données
catalogue <- read_delim("https://data.education.gouv.fr/api/explore/v2.1/catalog/exports/csv?delimiter=%3B&lang=fr") |> 
  select(default.title, datasetid)
data <- read_delim("https://data.education.gouv.fr/api/explore/v2.1/catalog/datasets/fr-en-calendrier-scolaire/exports/csv?lang=fr&timezone=Europe%2FParis&use_labels=true&delimiter=%3B")
test <- read_delim("https://data.education.gouv.fr/api/explore/v2.1/catalog/datasets/fr-en-annuaire-education/exports/csv?lang=fr&timezone=Europe%2FParis&use_labels=true&delimiter=%3B")

# Exploration
data |> 
  select(where(is.character)) %>% 
  explore_all() 

explore(data)

data |> 
  report(output_file = "rapport_data.html", output_dir = here::here()) 

view(dfSummary(data))
