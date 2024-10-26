################################
#### Exploration du texte   ####
################################

# Maintenant que nous avons traité et transformé notre texte en un jeu de données, nous pouvons commencer à l'explorer.
# L'analyse la plus simple à laquelle on puisse se livrer, dans le domaine du TAL, est celle de la fréquence des mots.
# L'analyse de la fréquence lexicale permet d'obtenir un aperçu des thèmes qui traversent des documents, de la présence/absence de personnages dans des chapitres donnés, etc.
# La fréquence peut être brute (nombre de fois que tel mot apparaît dans tel chapitre) ou elle peut être pondérée et, possiblement, normalisée.
# La pondération peut par exemple accorder un poids plus important à certains mots selon qu'ils sont ou non largement distribués à travers tous les documents. Cette pondération est appelée TF-IDF (Term Frequency / Inversed Document Frequency)
# L'exploration du lexique se fera sur trois types de matrices:
# 1) Une matrice Documents-mots avec les fréquences brutes utilisant le texte traité du tableau `maria_df`;
# 2) Une matrice Documents-mots avec les fréquences pondérées (TF-IDF) et normalisées le texte traité du tableau `maria_df`";
# 3) Une matrice Documents-mots avec les fréquences pondérées (TF-IDF) et normalisées utilisant les lemmes du tableau `maria_df`.
# Ces matrices permettront de visualiser les résultats sous la forme de nuages de mots et de diagrammes.


#### Chargement des modules supplémentaires ----
inst_ext_f <- function(extension) {
  if(!extension %in% rownames(installed.packages())) {
    install.packages(extension, dependencies = TRUE)
  }
  require(extension, character.only = TRUE)
}

extensions <- c("stringr",                              
                "tibble",
                "quanteda",
                "quanteda.textplots",
                "quanteda.textstats",
                "gt",
                "wordcloud2",
                "htmlwidgets",
                "webshot",
                "patchwork")                                    

sapply(extensions, inst_ext_f)
webshot::install_phantomjs(force = TRUE)

# Nettoyage de l'environnement
rm(list = ls())


#### Importation de la table des données traitées ----
maria_df <- readRDS("donnees/maria_traite.RDS")

#### Ajout d'une colonne avec les nos de chapitres pour simplifier la manipulation ultérieure ----
maria_df$doc_id <- rownames(maria_df) 
maria_df$chap <- 1:16


#### Création d'une matrice Documents-mots avec le texte traité ----
maria_dfm <- maria_df |> corpus(docid_field = "doc_id",
                   text_field = "texte_traite",
                   docvars = "chap") |> 
  tokens() |> 
  dfm()

# On peut observer une partie de cette "fameuse" matrice Documents-mots!
maria_dfm

# Sauvegarde de la structure de données
saveRDS(maria_dfm, "donnees/maria_dfm.RDS")


#### Création d'une deuxième matrice Documents-mots avec le même texte traité. La matrice sera pondérée et normalisée ----
maria_tfidf_dfm <- maria_dfm |> dfm_tfidf(scheme_tf = "propmax", scheme_df = "inversemax")

maria_tfidf_dfm

saveRDS(maria_tfidf_dfm, "donnees/maria_tfidf_dfm.RDS")


#### Création d'une troisième matrice Documents-mots avec les lemmes
maria_lemma_dfm <- maria_df |> corpus(docid_field = "doc_id",
                                text_field = "lemma",
                                docvars = "chap") |> 
  tokens() |> 
  dfm() |> dfm_tfidf(scheme_tf = "propmax", scheme_df = "inversemax")

maria_lemma_dfm

saveRDS(maria_lemma_dfm, "donnees/maria_lemma_dfm.RDS")


#### Visualisation des mots par chapitre ----

# Choisissez un chapitre en particulier et le nombre de mots que vous souhaitez examiner (fréquence brute)
maria_dfm |> dfm_subset(chap == 1) |> textstat_frequency(n=10)

# Choisissez un chapitre en particulier et le nombre de mots que vous souhaitez examiner (fréquence pondérée)
maria_tfidf_dfm |>dfm_subset(chap==1) |> textstat_frequency(n=10, force=TRUE)

# Choisissez un chapitre en particulier et le nombre de mots que vous souhaitez examiner (fréquence pondérée)
maria_lemma_dfm |> dfm_subset(chap==1) |> textstat_frequency(n=10, force=TRUE)


# On crée ci-dessous une petite fonction qui permettra ensuite de comparer les vocabulaires dominants selon la méthode de calcul
comparer_poids_f <- function(chapitre, n=10) {
  freq_brute <- maria_dfm |> dfm_subset(chap == chapitre) |> textstat_frequency(n=n)
  freq_ponderee <- maria_tfidf_dfm |>dfm_subset(chap == chapitre) |> textstat_frequency(n=n, force=TRUE)
  freq_lemma_ponderee <- maria_lemma_dfm |> dfm_subset(chap == chapitre) |> textstat_frequency(n=n, force=TRUE)
  df <- tibble(mots_simple = freq_brute$feature,
               n_simple = freq_brute$frequency,
               mots_tfidf = freq_ponderee$feature,
               n_tfidf = freq_ponderee$frequency,
               lemma_tfidf = freq_lemma_ponderee$feature,
               n_lemma = freq_lemma_ponderee$frequency)
  my_gt = df |> gt() |> 
    tab_header(
      title = paste("Distribution des mots et lemma du chapitre", chapitre),
      subtitle = "Valeurs brutes et pondérées"
    ) |> tab_style(
      style = cell_fill(color = "lightblue"),
      locations = cells_body(columns = c("mots_simple", "mots_tfidf", "lemma_tfidf"))) |> 
    cols_label(
      mots_simple = "Mot",
      n_simple = "Fréquence",
      mots_tfidf = "Mot",
      n_tfidf = "Fréq. pondérée",
      lemma_tfidf = "Lemma",
      n_lemma = "Fréq. pondérée"
    )
    
  return(my_gt)
}


# Vous n'avez maintenant qu'à indiquer, dans l'appel de fonction ci-dessous, le numéro du chapitre et le nombre de mots voulus
# Un tableau prêt à être exporté sous forme d'image apparaîtra à droite, dans le "Visualiseur".
comparer_poids_f(chapitre = 2, n = 25)





# On peut visualiser le même résultat sous la forme d'un nuage de mots
# Fonction pour comparer les nuages de mots

comparer_nuages_f <- function(chapitre, n = 100) {
  # Extraire les fréquences brutes et pondérées TF-IDF
  freq_brute <- maria_dfm |> dfm_subset(chap == chapitre) |> textstat_frequency(n = n)
  freq_ponderee <- maria_tfidf_dfm |> dfm_subset(chap == chapitre) |> textstat_frequency(n = n, force = TRUE)
  freq_lemma <- maria_lemma_dfm |> dfm_subset(chap == chapitre) |> textstat_frequency(n = n, force = TRUE)
  
  # Créer des dataframes pour les nuages de mots
  df_brute <- tibble(word = freq_brute$feature,
                     freq = freq_brute$frequency)
  df_ponderee <- tibble(word = freq_ponderee$feature,
                        freq = freq_ponderee$frequency)
  df_lemma <- tibble(word = freq_lemma$feature,
                     freq = freq_lemma$frequency)
  
  # Créer les nuages de mots avec wordcloud2
  nuage_brut <- wordcloud2(df_brute, size = 0.7, color = "random-light", backgroundColor = "#333333")
  nuage_tfidf <- wordcloud2(df_ponderee, size = 0.7, color = "random-light", backgroundColor = "#333333")
  nuage_lemma <- wordcloud2(df_lemma, size = 0.7, color = "random-light", backgroundColor = "#333333")
  
  # Sauvegarder les nuages de mots comme fichiers HTML temporaires
  saveWidget(nuage_brut, "nuage_brut.html", selfcontained = TRUE)
  saveWidget(nuage_tfidf, "nuage_tfidf.html", selfcontained = TRUE)
  saveWidget(nuage_lemma, "nuage_lemma.html", selfcontained = TRUE)
  
  # Convertir les fichiers HTML en images (nécessite webshot et PhantomJS)
  webshot("nuage_brut.html", "nuage_brut.png", delay = 0.5)
  webshot("nuage_tfidf.html", "nuage_tfidf.png", delay = 0.5)
  webshot("nuage_lemma.html", "nuage_lemma.png", delay = 0.5)
  
  # Afficher les deux nuages côte à côte
  img_brut <- grid::rasterGrob(png::readPNG("nuage_brut.png"), interpolate = TRUE)
  img_tfidf <- grid::rasterGrob(png::readPNG("nuage_tfidf.png"), interpolate = TRUE)
  img_lemma <- grid::rasterGrob(png::readPNG("nuage_lemma.png"), interpolate = TRUE)
  
  # Combiner les images avec patchwork
  plot_brut <- ggplot2::ggplot() + ggplot2::annotation_custom(img_brut) + ggplot2::theme_void() + ggplot2::ggtitle("Nuage de mots - Fréquence brute")
  plot_tfidf <- ggplot2::ggplot() + ggplot2::annotation_custom(img_tfidf) + ggplot2::theme_void() + ggplot2::ggtitle("Nuage de mots - TF-IDF")
  plot_lemma <- ggplot2::ggplot() + ggplot2::annotation_custom(img_lemma) + ggplot2::theme_void() + ggplot2::ggtitle("Nuage des lemmes - TF-IDF")
  
  # Affichage côte à côte
  plot_brut + plot_tfidf + plot_lemma + plot_layout(ncol = 1)
}

comparer_nuages_f(3, 100)


# Autre opération possible et fréquente:
# Composition de n-grammes avant la segmentation du texte en jetons (tokens). Ex.: "françois_paradis", "maria_chapdelaine", "père_chapdelaine", "da_bé"
# Cette opération peut être menée de manière automatique (ex.: assemblage de tous les termes qui se trouvent très souvent ensemble (collocation) sur une base statistique)
# ou encore être faite de manière supervisée, à partir d'un dictionnaire composé avec soin par le chercheur et selon la question de recherche. Ex.: "sous_sol_église", "mauvais_temps", etc.


# La segmentation d'un texte (tokenisation), l'allègement de ses dimensions et sa mise en forme dans une structure de données (tableau ou matrice) sont des opérations de base qui appellent des décisions importantes de la part de la chercheuse.
# La visualisation sous la forme de diagramme, de tableau ou de nuages de mots n'est qu'une étape préliminaire relevant de l'exploration des données. On peut ensuite envisager plusieurs autres opérations et analyses.
# À suivre...








#### Bibliographie ----
# Matthew L. Jockers et Rosamond Thalken, Text Analysis With R For Students in Literature, Springer, 2020.

