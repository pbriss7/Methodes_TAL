##################################
#### Cartographie littéraire  ####
##################################

# Le script ci-dessous est autonome, c'est-à-dire qu'il importera les objets traités ailleurs et ne devrait pas renvoyer d'erreur.
# Cependant, le travail de géoanalyse (geoparsing) et de géoréférencement (geocoding) a été fait avec Python et la bibliothèque Perdido.

# La géoanalyse de texte ("geoparsing") est une tâche associée à la reconnaissance d'entités nommées ("named entity recognition"). 
# Cette tâche, qui relève du TAL, consiste à identifier et à classifier automatiquement les entités mentionnées dans un texte en catégories prédéfinies, telles que les noms de personnes, de lieux, d'organismes, les dates, les adresses électroniques, etc. 


source("fonctions.R")


extensions <- c("data.table",
                "text2vec", 
                "tidygeocoder",
                "mapview", 
                "sf",
                "tmap")

sapply(extensions, inst_ext_f)

# Nettoyage de l'environnement
rm(list = setdiff(ls(), c("pretraitement_f")))

xyz_enrichi_dt <- readRDS("donnees/xyz_enrichie_rich_lex.RDS")

# Choix d'un texte

xyz_dupre_dt <- xyz_enrichi_dt[auteur == "Louise Dupré", .(doc_id, titre, texte)][1]

# Prétraitement
xyz_dupre_dt[, texte := gsub("’", "'", fixed = TRUE, texte)] # Ce type d'apostrophe n'est pas saisi par les motifs de ponctuation des expressions régulières.
xyz_dupre_dt[, texte := gsub("|", " ", fixed = TRUE, texte)] # Dans la table d'origine, le symbole "|" a été utilisé pour signaler un saut de paragraphe.

# Exportation du tableau de données pour géoréférencement avec Python / Spacy
# fwrite(xyz_dupre_dt, "donnees/xyz_dupre.csv")



#### L'annotation est faite avec Python dans VSCode ----




# Importation du tableau de données annoté
# fwrite(xyz_loc_dt, "donnees/dupre_lieux.csv")
xyz_loc_dt <- fread("donnees/dupre_lieux.csv")

xyz_loc_dt

# Retrait des entités entités qui ne correspondent pas à des lieux
xyz_dupre_lonlat_dt <- xyz_loc_dt[!entity %in% c("Sofia",
                                                 "Babel",
                                                 "Moi",
                                                 "Christos",
                                                 "Xavier",
                                                 "Mes",
                                                 "Minotaure",
                                                 "Loth",
                                                 "Alzheimer",
                                                 "Allons")]


# Ajoutons une variable catégorielle
xyz_dupre_lonlat_dt[, temporalite:=ifelse(entity %in% c("New York", 
                                                        "Montréal",
                                                        "Cuba",
                                                        "Îles - de - la - Madeleine",
                                                        "Québec")
                                          , "passe", "present")]

# On pourrait ainsi ajouter des variables en fonction des thèmes associés aux lieux


# Ces dernières années, plutôt que de travailler directement avec les latitudes et longitudes, on construit des objets appelés "Simple Feature" ou "sf" pour une manipulation plus aisée
sf_dt <- st_as_sf(
  xyz_dupre_lonlat_dt,
  coords = c("latitude", "longitude"),  # Spécifier les colonnes pour les coordonnées
  crs = 4326                            # Définir le système de coordonnées WGS84
)


# Nous pouvons regrouper les mentions de lieux et ajouter une colonne pour la fréquence
sf_dt_freq <- sf_dt |> group_by(entity, geometry, temporalite) %>%  
  summarize(frequence = n(), .groups = "drop") %>%
  ungroup()

# On crée une couche géographique
data("World")
# Définir la bounding box pour l'Europe de l'Ouest, Toronto à l'ouest et Cuba au sud
bbox_custom <- st_bbox(c(xmin = -110, ymin = 0, xmax = 40, ymax = 70), crs = st_crs(World))

# Afficher la carte avec l'étendue personnalisée
tmap_mode("plot")
tm_shape(World, bbox = bbox_custom) +
  tm_polygons() +
  tm_shape(sf_dt_freq) + tm_bubbles(size = "frequence", col = "red")


# Le mode "view" permet d'utiliser des couches de base variées:
tmap_mode("view")

# Modification de paramètres
tm_shape(World, bbox = bbox_custom) +
  tm_polygons() +
  tm_shape(sf_dt_freq) +
  tm_bubbles(size = "frequence", 
             col = "temporalite", 
             palette = c("passe" = "blue", 
                         "present" = "darkgreen")) +
  tm_layout(main.title = "Lieux évoqués dans Le Labirynthe de L. Dupré (2022)")











