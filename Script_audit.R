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



#--- Sélection 30 JDD pour l'audit

 catalogue_stat <- read_delim("https://data.education.gouv.fr/api/explore/v2.1/monitoring/datasets/ods-datasets-monitoring/exports/csv/?delimiter=%3B&lang=fr&timezone=Europe%2FParis&use_labels=true")# |> 
#   filter(grepl("2025|2026", modified) == TRUE) |> 
#   mutate(score = `API call count` + `Download count`) |> 
#   arrange(desc(score)) |> 
#   slice_head(n = 35) |> 
#   select(dataset_id,title,modified,`API call count`,`Download count`,score) |> 
#   filter_out(dataset_id == "fr-en-situation_nationale_covid") |> 
#   slice_head(n = 30)
# rio::export(catalogue_stat, "data/selection_30-JDD.csv")



#--- Stats rapport

sub_cat <- catalogue_stat |> 
  select(dataset_id, publisher, license, keyword, `API call count`,`Download count`, records_count) |> 
  mutate(nb_keyword = str_count(keyword, ",") + 1, .after = keyword)

# Mot description
str_count("Ce jeu de données contient les résultats au baccalauréat par origine sociale et par voie du baccalauréat.  Le baccalauréat est un diplôme national qui sanctionne la fin des études secondaires générale, technologique ou professionnelle. 

Ce jeu de données regroupe l'ensemble des candidats au baccalauréat des sessions de juin et de septembre, quels que soient leur origine scolaire (candidats individuels compris) et le ministère délivrant le diplôme (éducation nationale ou agriculture). 

La date d'observation est définie au 30 juin de chaque année même si la date des épreuves varie d'une année à l'autre. 

Dernière observation : Session 2025 du baccalauréat 

Pour en savoir plus :

Thomas F., 2026, « Résultats définitifs du baccalauréat 2025 : moins de bacheliers dans les voies générale et technologique, plus dans la voie professionnelle », Note d'Information, n° 26-05, DEPP. 

Thomas F., 2025, Résultats définitifs de la session 2024 du baccalauréat : un taux de réussite en hausse dans chacune des voies, Note d'Information, n° 25-05, DEPP. 
https://doi.org/10.48464/ni-25-05 

Thomas F., 2024,Résultats définitifs de la session 2023 du baccalauréat : stabilisation des résultats après la crise sanitaire et la mise en place du nouveau baccalauréat général, Note d'Information, n° 24.07, DEPP.  
https://doi.org/10.48464/ni-24-07

Série chronologique : La réussite au baccalauréat", " ") + 1


