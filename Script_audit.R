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
str_count("Listes des sections sportives scolaires implantées dans les établissements du 2nd degré à la rentrée scolaire 2025.", " ") + 1


# Catalogue pour les 30 JDD sélectionnés
jdd30 <- catalogue_stat |> 
  filter(dataset_id %in% c("fr-en-annuaire-education","fr-en-adresse-et-geolocalisation-etablissements-premier-et-second-degre","fr-en-etablissements-fermes","fr-en-ecoles-effectifs-nb_classes","fr-en-college-effectifs-niveau-sexe-lv","fr-en-lycee_gt-effectifs-niveau-sexe-lv","fr-en-lycee_pro-effectifs-niveau-sexe-lv","fr-en-lycee_pro-effectifs-niveau-sexe-mef","fr-en-mode-hebergement-eleves-etablissements-2d","fr-en-baccalaureat-par-academie","fr-en-baccalaureat-par-departement","fr-en-reussite-au-baccalaureat-origine-sociale","fr-en-proportion-de-bacheliers-dans-une-generation","fr-en-dnb-par-departement","fr-en-resultats-detailles-au-dnb","fr-en-cap-par-departement","fr-en-cap_specialite_sexe_statut_departement","fr-en-indicateur_segregation_sociale_colleges","fr-en-moyens_enseignants_2d_public","fr-en-moyens_enseignants_2d_prive","fr-en-offre-langues-2d","fr-en-sections-internationales","fr-en-pix_resultats_des_campagnes_de_rentree_par_eple","fr-en-effectifs-specialites-triplettes-1ere-generale","fr-en-carte-scolaire-colleges-publics","fr-en-etablissements-labellises-euroscol","fr-en-sections-sportives-scolaires","fr-en-dnma-par-uai-profils","fr-en-dnma-par-uai-services","fr-en-dnma-par-uai-appareils"))

test <- jdd30 |> 
  select(dataset_id, title, publisher, `API call count`,`Download count`)
#rio::export(test, "data/30jdd_final_dataviz.csv") #à compléter à la main sur le nombre de reco associées

tabyl(jdd30$domain_id)
tabyl(jdd30$publisher)
tabyl(jdd30$license)
library(stringr)
test <- jdd30 |> 
  mutate(theme2 = str_replace_all(theme, 
                             c(",Etablissements" = ";Etablissements",
                               ",Organisation" = ";Organisation",
                               ",Sport" = ";Sport",
                               ",Politique" = ";Politique",
                               ",Diplômes" = ";Diplômes"))) |> 
  mutate(theme2 = strsplit(as.character(theme2), ";")) |> 
  unnest(theme2) |> 
  tabyl(theme2)


# Dataviz finale
data_final_dataviz <- read_csv("data/30jdd_final_dataviz.csv") |> 
  mutate(publisher = ifelse(publisher != "Groupement d’intérêt public « Pix »", str_extract(publisher, "^.*?(?=\\s[-–]\\s)"), publisher))

library(ggforce)
final_viz <- data_final_dataviz |> 
  ggplot() +
  geom_point_interactive(aes(x = `Download count`, y = nb_reco, color = `publisher`,
                             tooltip = paste0(title, "\n", nb_reco, " recommandations et ", 
                                              `Download count`, " téléchargements")), 
                         size = 2) +
  labs(title = "Des améliorations à fort impact potentiel : les données les plus \ntéléchargées cumulent jusqu'à 6 recommandations", 
       subtitle = "Nombre de recommandations croisé au nombre de téléchargements des 30 jeux de données audités",
       x = "Nombre de téléchargements", y = "Nombre de recommandations identifiées") +
  scale_x_continuous(labels = scales::comma) +
  scale_color_manual(values = c("DNE" = "#411F6B",
                                "DGESCO" = "#b85ee5",
                                "DEPP" = "#af1b76",
                                "DREIC" = "#DB928A",
                                "Groupement d’intérêt public « Pix »" = "#f4d9ba")) +
  facet_zoom(x = `Download count` < 50000, split = TRUE) +
  guides(color = guide_legend(title = "Producteur", nrow = 2)) +
  theme_classic() +
  theme(legend.position = "top",
        legend.background = element_rect(fill="transparent", color=NA),
        plot.title = ggplot2::element_text(size = 15, face = "bold", color = "#222222"), 
        plot.subtitle = ggplot2::element_text(face = "italic", color = "#222222"), 
        axis.text = ggplot2::element_text(size = 9, color = "#222222"), 
        axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5,b = 10)), 
        axis.title = ggplot2::element_text(size = 11, color = "#222222"),
        strip.background = element_rect(fill = "grey", colour = "#999999", linetype = 2),
        panel.grid.minor = ggplot2::element_line(color = "#cbcbcb"),
        panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
        panel.grid.major.x = ggplot2::element_line(color = "#cbcbcb"),
        strip.text = ggplot2::element_text(size = 22, hjust = 0, face = "bold"),
        text = element_text(family = "Open Sans"),
        panel.background = element_rect(fill="transparent", color=NA),
        plot.background = element_rect(fill="transparent", color=NA))
final_viz
ggsave(file = "files/dataviz_finale_audit.png", plot = final_viz, width = 8, height = 6, bg="transparent")


library(ggiraph)
girafe(print(final_viz), width_svg = 8, height_svg = 6)


