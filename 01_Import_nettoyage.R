############################################
#### Importation et traitement du texte ####
############################################

# Dans ce premier script, nous allons importer le texte d'un roman français depuis le Projet Gutemberg, puis traiter (ou prétraiter) ce texte pour pouvoir ensuite explorer son contenu lexical (script 2).
# Le prétraitement implique:
# 1) l'élimination du péritexte et des titres de chapitres (ex. "CHAPITRE I") qui sont intégrés au texte du roman;
# 2) l'élimination des mots fonctionnels;
# 3) la segmentation du texte en jetons (ou "tokens");
# 4) l'annotation automatique et l'extraction des lemmes (formes neutres des mots);

# Les différentes versions du texte seront emmagasinées dans un tableau de données (data frame).

# Des exercices sur les expressions régulières sont proposés dans le script. Les réponses aux questions sont fournies à la toute fin du script.




#### Installation et activation des modules supplémentaires ----

inst_ext_f <- function(extension) {
  if(!extension %in% rownames(installed.packages())) {
    install.packages(extension, dependencies = TRUE)
  }
  require(extension, character.only = TRUE)
}

extensions <- c("stringr",                            
                "data.table",
                "lsa",
                "tm",
                "ggplot2",
                "wordcloud2",
                "tibble",
                "dplyr",
                "tm",
                "Matrix",
                "text2vec",
                "tidyr",
                "udpipe")                             

sapply(extensions, inst_ext_f)

# Nettoyage de l'environnement:
rm(list = ls())

#### Création d'un dossier pour sauvegarder le texte et les données produites ----
if(!dir.exists("donnees")) {dir.create("donnees")}


#### Chargement du texte du roman Maria Chapdelaine ----
text_v <- if(!file.exists("donnees/maria_raw.txt")){
  scan("https://www.gutenberg.org/cache/epub/13525/pg13525.txt", 
       what = "character", 
       sep = "\n")
  } else {
    readLines("donnees/maria_raw.txt")
    }

#### Sauvegarde de la structure importée ----
# write(text_v, "donnees/maria_raw.txt")                  



# Exploration de la structure de données:
head(text_v, n = 100)                                  # Impression des 100 premières lignes dans la console

tail(text_v, n=400)                                    # Impression des 400 dernières lignes dans la console



#### Élimination de l'épitexte ----
start_v <- which(text_v == "CHAPITRE I")               # Première ligne du roman
end_v <- which(text_v == "bois pour les semailles.")   # Dernière ligne du roman

maria_lignes_v <- text_v[start_v:end_v]                # Élimination des parties inutiles

# write(maria_lignes_v, "donnees/maria_sans_epitexte.txt")


#### Assemblage du roman en une seule chaine de caractères ----
maria_v <- paste(maria_lignes_v, collapse = " ")       # Collage de toutes les lignes du roman en une seule longue chaine de caractères




#### Statistiques lexicales ----
# Première étape: créer une copie du texte. Les titres des chapitres seront ensuite retirés du contenu textuel.
maria_copie_v <- maria_v


# ** Exercice 1: créer une expression régulière qui saisira tous les titres de chapitres, de "CHAPITRE I" à "CHAPITRE XVI".
# Testez votre expression dans regex101.com.
# Une fois que vous êtes satisfait·e du résultat, insérez l'expression dans les guillemets de la ligne suivante, et exécutez le code!
maria_copie_v <- str_remove_all(maria_copie_v, "")

# Nous allons également mettre toutes les lettres en minuscules.
maria_copie_v <- tolower(maria_copie_v)

# Nous allons remplacer toutes les ponctuations par une espace.
# Exercice 2: trouvez le motif pour séparer une chaine sur tout ce qui n'est pas un caractère appartenant à un mot.
maria_copie_l <- strsplit(maria_copie_v, "")

# La fonction strsplit() génère une liste. Nous allons transformer cette liste en simple vecteur:
maria_copie_v <- unlist(maria_copie_l)

# Observez les 100 premiers éléments de la liste générée:
maria_copie_v[1:100]

# On peut supprimer les éléments sans contenu ("") de différentes manières:
not_blanks_v <- which(maria_copie_v != "")
maria_copie_v <- maria_copie_v[not_blanks_v]


# Avec cette structure, nous allons créer une table.
# Exemple avec les 100 premiers mots:
table(maria_copie_v[1:100])

maria_t <- table(maria_copie_v)

# On peut inspecter les 25 premiers éléments de cette table:
maria_t[1:25]

# Il semble que, dans le fichier original, des mots en italiques aient été orthographiés comme suit: _[mot]_
# Exercice 3: insérez dans le code ci-dessous une expression régulière qui permettra d'éliminer "_" ou "_[mot]_"
# Un indice: il y a trois cas de figures.

maria_copie_v <- str_remove_all(maria_copie_v, "")

# On reprend maintenant les opérations ci-dessous pour éliminer les éléments sans contenu et pour créer une table propre
not_blanks_v <- which(maria_copie_v != "")
maria_copie_v <- maria_copie_v[not_blanks_v]
maria_t <- table(maria_copie_v)
maria_t[1:25]

# Il convient d'ordonner la table en fonction des valeurs décroissantes:
maria_t <- sort(maria_t, decreasing = TRUE)

# Exploration de cette table de fréquence:
maria_t[1:100]

# Exploration sous la forme d'un tableau de données (data.frame):
data.frame(maria_t[1:100])

# Que remarquez-vous?


#### Élimination des mots fonctionnels ----
# Voici deux antidictionnaires (de simples vecteurs de mots fonctionnels) différents:

antidictionnaire_long <- lsa::stopwords_fr
antidictionnaire_court <- tm::stopwords(kind = "fr")

antidictionnaire_long
antidictionnaire_court


# Inspectez les deux antidictionnaires, puis insérez-les, l'un après l'autre, dans la fonction ci-dessous.

table_filtree_f <- function(table_non_filtree = maria_t, antidictionnaire, nombre_mots = 25) {
  table_filtree_t <- table_non_filtree[!names(table_non_filtree) %in% antidictionnaire]
  table_filtree_df <- data.frame(
    mot = names(table_filtree_t),
    frequence = as.vector(table_filtree_t)
  )
  table_filtree_reduite_df <- table_filtree_df[1:nombre_mots, ]
  return(table_filtree_reduite_df)
}

table_filtree_f(antidictionnaire = antidictionnaire_court)        # 



# Exercice 4 (optionnel): composez votre propre antidictionnaire en complétant le code ci-dessous, puis insérez-le dans la fonction pour voir le résultat.

mon_dictionnaire_personnel <- c("plus", "comme", #etc.              # Ajoutez autant de mots que vous le souhaitez.
                                )


table_filtree_f(mon_dictionnaire_personnel)                       # Insérez votre antidictionnaire dans la fonction, puis exécutez le code.



#### Visualisation des mots sous la forme d'un graphique ----

diagramme_f <- function(x) {
  x |> ggplot(aes(x = reorder(mot, frequence), y = frequence)) +
    geom_jitter() + 
    coord_flip() + 
    theme_linedraw()
}


ma_table <- table_filtree_f(antidictionnaire = antidictionnaire_long)

diagramme_f(ma_table)


#### Distribution des fréquences lexicales par chapitre ----
# Une table de fréquence offre un aperçu des thèmes et du contenu sémantique d'un texte.
# Nous allons refaire l'exercice, mais en divisant d'abord le texte en chapitre.
# Cela permettra de comparer les chapitres les uns avec les autres.

# Tout d'abord, reprenons la structure initiale et, au lieu d'éliminer les titres de chapitres du texte,
# servons-nous de ces titres pour structurer le contenu textuel.

# Fonction pour créer le tableau de données (data frame) à partir des titres de chapitres:
dataframe_chapitres_f <- function(lignes_v) {
  
  # Repérer les indices des titres des chapitres:
  titres_indices <- grep("^CHAPITRE", lignes_v)
  
  # Extraire les titres des chapitres:
  titres_chapitres <- lignes_v[titres_indices]
  
  # Initialiser une liste pour stocker le texte de chaque chapitre:
  texte_chapitres <- list()
  
  # Parcourir les chapitres et assembler le texte:
  for (i in seq_along(titres_indices)) {
    
    # Début du chapitre (excluant le titre lui-même):
    debut <- titres_indices[i] + 1
    
    # Fin du chapitre (soit le prochain titre, soit la fin du vecteur):
    fin <- if (i < length(titres_indices)) {
      titres_indices[i + 1] - 1
    } else {
      length(lignes_v)
    }
    
    # Coller les lignes du chapitre en un seul texte:
    texte_chapitres[[i]] <- paste(lignes_v[debut:fin], collapse = " ")
  }
  
  # Créer le tableau de données:
  df_chapitres <- data.frame(
    texte = unlist(texte_chapitres), 
    row.names = titres_chapitres,
    stringsAsFactors = FALSE
  )
  
  # Retourner le résultat
  return(df_chapitres)
}

# Application de la fonction sur le vecteur 'maria_lignes_v':
maria_df <- dataframe_chapitres_f(maria_lignes_v)

# Obtenir un aperçu du tableau de données:
# View(maria_df)

# Nous allons maintenant créer une deuxième colonne dans ce talbeau.
# Nous allons y insérer le texte traité.

# Fonction pour traiter le texte:
traitement_texte_f <- function(texte, antidictionnaire) {
  
  # Passer tout le texte en minuscules:
  texte_minuscule <- tolower(texte)
  
  # Supprimer la ponctuation et les symboles:
  texte_sans_ponctuation <- gsub("\\W", " ", texte_minuscule)
  
  # Supprimer les mots fonctionnels
  # Créer une expression qui corresponde à chaque mot de l'antidictionnaire
  pattern <- paste0("\\b(", paste(antidictionnaire, collapse = "|"), ")\\b")
  
  # Remplacer les mots fonctionnels par une chaîne vide:
  texte_traite <- gsub(pattern, "", texte_sans_ponctuation)
  
  # Supprimer les espaces multiples générés précédemment:
  texte_traite <- gsub("\\s+", " ", texte_traite)
  
  # Retourner le texte traité sans espaces superflus au début et à la fin de la chaine:
  return(trimws(texte_traite))
}


# Application du traitement sur la colonne 'texte' pour créer la colonne 'texte_traite':
antidictionnaire_long <- lsa::stopwords_fr
maria_df$texte_traite <- sapply(maria_df$texte, traitement_texte_f, antidictionnaire = antidictionnaire_long)

# Aperçu du tableau avec la nouvelle colonne:
# View(maria_df)

# La prochaine étape consiste à ajouter une colonne au tableau où nous emmagasinerons tous les lexèmes (tokens).
tokenisation_f <- function(x) {
  mots_l <- strsplit(x, " ")
  mots_v <- unlist(mots_l)
  return(mots_v)
}

maria_df$token <- sapply(maria_df$texte_traite, tokenisation_f)


# Exploration du tableau  avec la nouvelle colonne:
View(maria_df)


#### Création de lemmes par annotation automatique ----
# Avec un texte qui comporte un grand nombre de mots dont la graphie calque la langue orale, l'annotation automatique est risquée. 
# Il faudrait idéalement surentrainer un modèle de language, opération qui dépasse le cadre de cet atelier.
# Nous allons utiliser un modèle de language général et tout de même observer les résultats.
# Nous pouvons faire cette annotation avec quatre modèles de français: french-gsd, french-partut, french-sequoia, french-spoken

# Importons d'abord deux des quatre modèles disponibles dans un dossier appelé "modele_language":
modeles <- c("french-gsd", "french-spoken")
sapply(modeles, udpipe_download_model, model_dir = "modele_language")

mes_modeles <- list.files("modele_language", full.names = TRUE)

udmodel_gsd <- udpipe_load_model(mes_modeles[1])
udmodel_spoken <- udpipe_load_model(mes_modeles[2])

# Tester les modèles sur un très court chapitre:
maria_df$texte[16]

# Ci-dessous, on utilise chaque modèle pour annoter automatiquement le court chapitre, puis on le visualise.
# Les sigles d'annotation (Universal Part of Speech Tags) sont définis dans cette page: https://universaldependencies.org/u/pos/index.html
udpipe_annotate(udmodel_gsd, maria_df$texte[16]) |> as.data.frame() |> View()
udpipe_annotate(udmodel_spoken, maria_df$texte[16]) |> as.data.frame() |> View()

# Utilisation du modèle french-gsd pour annoter l'ensemble du roman. 
# Les lemmes seront ensuite emmagasinés dans notre structure de données principale.
for(i in 1:nrow(maria_df)) {
  udpipe_annotation <- udpipe_annotate(udmodel_gsd, maria_df$texte[i]) |> as.data.frame()
  sous_groupe_lemmes <- subset(udpipe_annotation, upos %in% c("NOUN", "PROP", "PROPN", "VERB", "ADJ"))
  extraction_lemmes_v <- paste.data.frame(sous_groupe_lemmes, term = "lemma", group = "doc_id")
  maria_df$lemma[i] <- extraction_lemmes_v$lemma
}

View(maria_df)


saveRDS(maria_df, "donnees/maria_traite.RDS")








#### Réponses aux exercices ----
# Exercice 1: "CHAPITRE\\s[XVI]+\\s?"
# Exercice 2: "\\W"
# Exercice 3: "_(?=\\w)|(?<=\\w)_|^_$"

#### Bibliographie ----
# Matthew L. Jockers et Rosamond Thalken, Text Analysis With R For Students in Literature, Springer, 2020.




