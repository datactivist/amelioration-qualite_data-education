# Script pour explorer les données pour le rapport d'audit sur les données de l'éducation

# Librairies
library(tidyverse)
library(summarytools)
library(explore) 

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

catalogue_stat <- read_delim("https://data.education.gouv.fr/api/explore/v2.1/monitoring/datasets/ods-datasets-monitoring/exports/csv/?delimiter=%3B&lang=fr&timezone=Europe%2FParis&use_labels=true") |> 
  filter(grepl("2025|2026", modified) == TRUE) |>
  mutate(score = `API call count` + `Download count`) |>
  arrange(desc(score)) |>
  slice_head(n = 35) |>
  select(dataset_id,title,modified,`API call count`,`Download count`,score) |>
  filter_out(dataset_id == "fr-en-situation_nationale_covid") |>
  slice_head(n = 30)
rio::export(catalogue_stat, "data/selection_30-JDD.csv")



#--- Stats rapport

# Import catalogue
catalogue_stat <- read_delim("https://data.education.gouv.fr/api/explore/v2.1/monitoring/datasets/ods-datasets-monitoring/exports/csv/?delimiter=%3B&lang=fr&timezone=Europe%2FParis&use_labels=true")

# Sous df du catalogue
sub_cat <- catalogue_stat |> 
  select(dataset_id, publisher, license, keyword, `API call count`,`Download count`, records_count) |> 
  mutate(nb_keyword = str_count(keyword, ",") + 1, .after = keyword)

# Compte du nombre de mots dans la description
str_count("Listes des sections sportives scolaires implantées dans les établissements du 2nd degré à la rentrée scolaire 2025.", " ") + 1



#--- Données des 30 JDD sélectionnés

# Sélection via les identifiants
jdd30 <- catalogue_stat |> 
  filter(dataset_id %in% c("fr-en-annuaire-education","fr-en-adresse-et-geolocalisation-etablissements-premier-et-second-degre","fr-en-etablissements-fermes","fr-en-ecoles-effectifs-nb_classes","fr-en-college-effectifs-niveau-sexe-lv","fr-en-lycee_gt-effectifs-niveau-sexe-lv","fr-en-lycee_pro-effectifs-niveau-sexe-lv","fr-en-lycee_pro-effectifs-niveau-sexe-mef","fr-en-mode-hebergement-eleves-etablissements-2d","fr-en-baccalaureat-par-academie","fr-en-baccalaureat-par-departement","fr-en-reussite-au-baccalaureat-origine-sociale","fr-en-proportion-de-bacheliers-dans-une-generation","fr-en-dnb-par-departement","fr-en-resultats-detailles-au-dnb","fr-en-cap-par-departement","fr-en-cap_specialite_sexe_statut_departement","fr-en-indicateur_segregation_sociale_colleges","fr-en-moyens_enseignants_2d_public","fr-en-moyens_enseignants_2d_prive","fr-en-offre-langues-2d","fr-en-sections-internationales","fr-en-pix_resultats_des_campagnes_de_rentree_par_eple","fr-en-effectifs-specialites-triplettes-1ere-generale","fr-en-carte-scolaire-colleges-publics","fr-en-etablissements-labellises-euroscol","fr-en-sections-sportives-scolaires","fr-en-dnma-par-uai-profils","fr-en-dnma-par-uai-services","fr-en-dnma-par-uai-appareils"))

sub <- jdd30 |> 
  select(dataset_id, title, publisher, `API call count`,`Download count`)
#rio::export(sub, "data/30jdd_final_dataviz.csv") #à compléter à la main sur le nombre de reco associées

# Exploration
tabyl(jdd30$domain_id)
tabyl(jdd30$publisher)
tabyl(jdd30$license)

# Thématiques des jeux retenus
library(stringr)
theme30 <- jdd30 |> 
  mutate(theme2 = str_replace_all(theme, 
                             c(",Etablissements" = ";Etablissements",
                               ",Organisation" = ";Organisation",
                               ",Sport" = ";Sport",
                               ",Politique" = ";Politique",
                               ",Diplômes" = ";Diplômes"))) |> 
  mutate(theme2 = strsplit(as.character(theme2), ";")) |> 
  unnest(theme2) |> 
  tabyl(theme2)


