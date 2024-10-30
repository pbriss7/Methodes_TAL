#### Fonctions ----

inst_ext_f <- function(extension) {
  if(!extension %in% rownames(installed.packages())) {
    install.packages(extension, dependencies = TRUE)
  }
  require(extension, character.only = TRUE)
}


pretraitement_f <- function(chaine, 
                            minuscules = TRUE, 
                            enlever_nombres = TRUE, 
                            enlever_ponctuation = TRUE, 
                            antidictionnaire = NULL) {
  
  # Vérification que l'input est bien une chaîne de caractères
  if (!is.character(chaine) || length(chaine) != 1) {
    stop("L'entrée doit être une chaîne de caractères unique.")
  }
  
  # Conversion en minuscules si demandé
  if (minuscules) {
    chaine <- tolower(chaine)
  }
  
  # Suppression de la ponctuation si demandé
  if (enlever_ponctuation) {
    chaine <- gsub(pattern = "[[:punct:]]+", replacement = " ", chaine, perl = TRUE)
  } 
  
  chaine <- gsub(pattern = "’", " ", fixed = TRUE, chaine)
  
  # Suppression des nombres si demandé
  if (enlever_nombres) {
    chaine <- gsub(pattern = "[0-9]+", replacement = " ", chaine, perl = TRUE)
  } 
  
  # Segmentation du texte en unités lexicales (mots)
  mots <- unlist(strsplit(chaine, "\\s+"))
  
  # Suppression des mots fonctionnels si un antidictionnaire est fourni
  if (!is.null(antidictionnaire)) {
    mots <- mots[!mots %in% antidictionnaire]
  }
  
  # Reconstruction de la chaîne de caractères à partir des mots restants
  chaine <- paste(mots, collapse = " ")
  
  # Avertissement si aucun mot n'est retenu après le prétraitement
  if (nchar(chaine) == 0) {
    warning("Le texte traité ne contient plus aucun mot après le prétraitement.")
  }
  
  return(chaine)
}


calcul_tfidf_f <- function(term_matrix) {
  # Nombre de documents
  num_docs <- nrow(term_matrix)
  
  # Calcul de la fréquence des termes (TF)
  term_frequencies <- term_matrix / rowSums(term_matrix) # Diviser chaque ligne par le total de termes dans le document
  
  # Calcul de l'inverse de la fréquence documentaire (IDF)
  # IDF = log((N + 1) / (df + 1)) + 1, où df est le nombre de documents contenant le terme
  doc_frequencies <- colSums(term_matrix > 0) # Nombre de documents contenant chaque terme
  idf <- log((num_docs + 1) / (doc_frequencies + 1)) + 1 # Ajout de 1 pour éviter log(0)
  
  # Calcul de TF-IDF
  tfidf <- term_frequencies * idf
  
  # Retourner la matrice sparse TF-IDF
  return(tfidf)
}



