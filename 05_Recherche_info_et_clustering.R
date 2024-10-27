###########################################################################
#### Recherche d'information et regroupement hiérarchique de documents ####
##########################################################################

# On a déjà parlé du modèle vectoriel et de son importance dans la recherche d'information et les tâches de classification.
# L'atelier qui suit vise à implémenter un tel espace avec les documents composant le corpus XYZ, et à voir ce qu'on peut en tirer.

#### L'espace vectoriel ----
# En contexte d'analyse de données textuelles, le modèle d'espace vectoriel est un concept mathématique qui permet de représenter chaque document d'une collection comme un vecteur numérique.
# Ces vecteurs sont situés les uns par rapport aux autres en fonction de leurs valeurs.
# Soit, par exemple, les "documents" suivants:
# 1. "Je mange"; 2. "Je marche"; 3. "Il marche".
# Si j'utilise le modèle simple du BOW, chaque document sera représenté comme un vecteur comportant chacun quatre nombres entiers.
# [1, 1, 0, 0] pour "Je mange", [1, 0, 1, 0] pour "Je marche" et [0, 0, 1, 1] pour "Il marche"
# Dans l'espace vectoriel, les trois documents seront situés les uns par rapport aux autres en fonction de ces quatre dimensions ("Je", "mange", "marche" "Il").
# L'espace vectoriel peut contenir des dizaines, voire des centaines de milliers de dimensions.
# L'humain n'est pas capable de se représenter un tel espace, mais d'un point de vue mathématique, cela ne pose aucun problème.

# Pourquoi se donner tant de mal et recourir à ce concept mathématique? 
# Comme tout espace vectoriel, celui qui représente une collection de documents est régi par l'algèbre linéaire, donc par des lois mathématiques.
# La "proximité" entre les documents, dans l'espace vectoriel, est utilisée comme une mesure de leur similarité sémantique.
# Les mesures qui peuvent être utilisées pour évaluer cette proximité sont multiples (cosinus, euclidienne, jaccard, Manhattan, etc.).
# La plus utilisée est la mesure cosinus, qui utilise l'angle entre deux vecteurs pour déterminer leur degré de similitude. 
# Une telle mesure neutralise le biais introduit par la longueur des documents.

# Passons à un exemple concret. Nous allons traiter, puis vectoriser la collection de documents du corpus XYZ.
# Rappelons pour mémoire que la table où est emmagasinée cette collection comprend 564 nouvelles littéraires produites par 318 écrivains entre 2012 et 2022.
# Aussi, rappelons que le long travail de préparation des données a été mené par Julien Vallières-Gingras et par Yuchen Shi.


#### Chargement des fonctions personnelles ----
# L'exécution de la commande suivante importera dans l'environnement deux fonctions contenues dans le script "fonctions.R".
# Ces fonctions sont "inst_ext_f()" et "pretraitement_f()".
# Pour voir leur composition, ouvrez le fichier "fonctions.R" ou exécutez le nom des fonctions.

#### Installation et activation des extensions 
source("fonctions.R")
extensions <- c("stringr",
                "data.table",
                "text2vec", 
                "udpipe",
                "ggplot2",
                "wordcloud2",
                "proxy",
                "Matrix", 
                "dendextend",
                "splitstackshape",
                "umap")                             

sapply(extensions, inst_ext_f)

# Nettoyage de l'environnement
rm(list = setdiff(ls(), c("pretraitement_f", "calcul_tfidf_f")))

#### Prétraitement ----
# La méthode de vectorisation avec l'extension text2vec est expliquée dans la vignette suivante: https://text2vec.org/vectorization.html.

# Importation du jeu de données
xyz_dt <- readRDS("donnees/xyz_enrichie_rich_lex.RDS")

# Petit nettoyage des textes
xyz_dt[, texte := gsub("’", "'", fixed = TRUE, texte)]
xyz_dt[, texte := gsub("|", " ", fixed = TRUE, texte)]

# Annotation morphosyntaxique du jeu de données avec udpipe
chemin_modele <- "modele_langage/french-gsd-ud-2.5-191206.udpipe"
if(!file.exists(chemin_modele)) {
  udpipe_download_model(chemin_modele)
}

modele <- udpipe_load_model(chemin_modele)


# L'opération d'annotation qui suit prend plusieurs minutes et on va donc importer le jeu de donnée déjà annoté et enregistré dans le sous-dossier `donnees/`
# xyz_pos <-
#   udpipe_annotate(modele,
#                   x = xyz_dt$texte,
#                   doc_id = xyz_dt$doc_id,
#                   parallel.cores = 8) |>
#   data.table::as.data.table()

# saveRDS(xyz_pos, "donnees/xyz_pos.RDS")
xyz_pos <- readRDS("donnees/xyz_pos.RDS")

# On allège la structure de l'objet en ne conservant que les colonnes d'intérêt pour notre tâche
xyz_pos_annote <- xyz_pos[, .(doc_id, token, lemma, upos)]

xyz_pos_annote[1]

# On sélectionne les lexèmes d'intérêt selon les catégories grammaticales issues de l'annotation
xyz_pos_annote <- xyz_pos_annote[upos %in% c('VERB','NOUN','ADJ','ADV','PROPN')]


# Ci-dessous, on transforme la structure udpipe en une table de 564 lignes, une ligne par document, avec trois colonnes. 
# Ces colonnes correspondent à: 
# 1) l'identifiant unique de chaque document;
# 2) une liste comprenant tous les mots (tokens) de ce document;
# 3) une liste comprenant tous les lemmes correspondants.

xyz_pos_annote <- xyz_pos_annote[,
                                 .(token = list(token), 
                                   lemma = list(lemma)
                                   ),
                                 by = .(doc_id)]

# Observons et sauvegardons cette structure de données nettoyée.
xyz_pos_annote

saveRDS(xyz_pos_annote, "donnees/xyz_pos_annote.RDS")



#### Vectorisation ----

# Utilisation de l'extension text2vec pour créer une matrice de type documents-mots
iter <- text2vec::itoken(xyz_pos_annote$lemma,                   # Utilisation des lemmes.
                         
                         ids = xyz_pos_annote$doc_id, 
                         
                         progressbar = FALSE)

vocab <- text2vec::create_vocabulary(iter,                       # Création d'une table comprenant tous les lemmes et leur fréquence.
                                     ngram=c(ngram_min = 1,
                                             ngram_max = 1))     # On précise qu'on ne veut que des unigrammes.

vocab <- text2vec::prune_vocabulary(vocab,                       # On allège le vocabulaire en fournissant des seuils minimums et maximums de fréquences et de proportions documentaires.
                                    term_count_min = 5,
                                    term_count_max = Inf,
                                    doc_proportion_min = 0.0001,
                                    doc_proportion_max = 0.5,
                                    doc_count_min = 2,
                                    doc_count_max = Inf,
                                    vocab_term_max = Inf)

vocab <- data.table::setDT(vocab)                                # On transforme la structure en un tableau de type data.table.
vocab

vocab <- vocab[!term %in% c("A.")]                               # Après avoir observé le tableau de fréquences, on élimine un résidu.
vocab


# Avec cette structure, nous allons créer la matrice documents-mots.
vectorizer <- text2vec::vocab_vectorizer(vocab)

xyz_dtm <- text2vec::create_dtm(iter,
                                vectorizer,
                                type = "dgCMatrix")

saveRDS(xyz_dtm, 'donnees/xyz_dtm.RDS')
xyz_dtm <- readRDS('donnees/xyz_dtm.RDS')

# L'objet produit est une matrice compressée ("dgC" pour "digital", "general" et "compressed, column oriented"), moins lourde qu'une matrice de base.
str(xyz_dtm)


#### Similarité sémantique ----
# La première chose qu'on peut faire avec une telle matrice est la recherche du ou des documents correspondant à une requête d'information donnée.
# Pour effectuer cette recherche, nous allons créer une "matrice requête" comportant une seule ligne et ayant la même longueur que la matrice documents-mots.
# Ses dimensions seront donc de [1, 8736]. Le chiffre 8736 correspond au nombre de colonnes (lemmes) de la matrice documents-mots
# Les noms de colonnes de cette matrice requête seront les mêmes que ceux de la matrice documents-mots. Ces noms, on s'en souvient, sont les lemmes uniques du vocabulaire des documents.
# Nous allons d'abord remplir cette matrice requête de zéros, puis nous indiquerons la valeur "1" seulement là où les mots de la requête coïncident avec le lemme (nom de colonne) correspondant de la matrice documents-mots.


# Créons tout d'abord la matrice requête de même longueur que notre matrice documents-mots.

matriceRequete <- matrix(data = 0,
                         nrow = 1, 
                         ncol =  ncol(xyz_dtm)
                         )  


# On ajoute maintenant à ce vecteur les noms de colonnes de la matrice documents-mots.
colnames(matriceRequete) <- colnames(xyz_dtm)

# Examinons les dix premiers éléments de la matrice requête.
matriceRequete[1, 1:10]


# On donne à la ligne unique de cette matrice le nom "requete".
rownames(matriceRequete) <- "requete"

# Examinons la structure de cette matrice requête.
str(matriceRequete)


# Dans l'étape qui suit, nous allons construire une requête, c'est-à-dire que nous allons choisir les mots d'une requête qui nous permettront de trouver, parmi les documents de la matrice documents-mots, ceux qui correspondent le mieux à cette requête.
# Il faut que les termes de notre requête soient présents dans la matrice documents-mots, qu'ils figurent quelque part dans les noms de colonnes.
# Pour construire notre requête, nous allons utiliser un motif, une expression régulière qui pourrait saisir ou attraper plus d'un nom de colonne à la fois.

# Notre première requête sera simple: on attrapera les colonnes de la matrice correspondant à une variation de "trahi".
motifRequete <- regex("trahi", ignore_case = TRUE, dotall = TRUE)

# On ajoute maintenant un "1" là où, dans notre vecteur-requête, on trouve une variante de "trahi"
matriceRequete[1, grep(motifRequete, colnames(matriceRequete))] <- 1

# On peut examiner les mots (noms de colonnes) ainsi attrapés:
matriceRequete[1, matriceRequete[1,] > 0]

# Pour comprendre la prochaine étape, il faut se représenter chaque document de la matrice documents-mots comme un vecteur dont les valeurs (pondérées et/ou normalisées) déterminent une direction dans l'espace vectoriel de 8736 dimensions.
# Les vecteurs peuvent être de différentes longueurs, mais ils ont tous la même origine, partent tous du même point.
# En créant une matrice requête, nous construisons un vecteur qui prendra position dans l'espace vectoriel à partir du même point d'origine.
# La mesure de similarité cosinus utilisée pour déterminer la proximité sémantique de la requête et des documents utilise l'angle cosinus formé depuis le point d'origine entre le vecteur requête et un vecteur document (une ligne de la matrice documents-mots).
# La fonction ci-dessous, tirée de l'extension ProxyC, permet de calculer la valeur de chaque angle. 
# L'objet retourné par cette fonction sera transformé en un tableau de données où nous trouverons, pour chaque document de la matrice documents-mots, la valeur de l'angle.

similarite_cosinus <- proxyC::simil(x = xyz_dtm, 
                                    y = matriceRequete, 
                                    method = "cosine") |> as.data.frame.matrix()


# Avec le tableau de données (data frame), nous allons fabriquer une table de type data.table, plus commode à manipuler.
simil_cosinus_dt <- data.table(
  doc_id = as.integer(rownames(similarite_cosinus)),
  cosine = similarite_cosinus$requete
)


# Nous disposons maintenant, pour chaque document de la matrice documents-mots, d'une mesure de similarité sémantique.
# Une valeur de "0" indique que le vocabulaire du document lié ne comporte pas une déclinaison de "trahi".
# Plus la valeur est élevée et proche de 1, plus le document se rapproche, sémantiquement, du vecteur requête.

summary(simil_cosinus_dt$cosine)

# On voit que la valeur de similarité varie entre 0 et 0.05786.
# On peut donc filtrer nos documents sur la base de ces valeurs.

# Si on cherche seulement le document le plus pertinent, ce sera celui ayant cette valeur maximale de 0.05786.
document_plus_similaire <- simil_cosinus_dt[cosine == max(simil_cosinus_dt$cosine)]

# Grâce à l'identifiant unique de ce document, on peut obtenir le texte et toutes les métadonnées voulues du document.
xyz_dt[doc_id == document_plus_similaire$doc_id, .(texte)]

# Si on souhaite obtenir plusieurs documents pertinents, classés selon leurs seuils de similarité, on peut le faire comme suit:

seuilMinSimil <- 0.01

# On ordonne ensuite le résultat et on le filtre en fonction du seuil:
docs_significatifs_dt <- simil_cosinus_dt[cosine > seuilMinSimil][order(-cosine)]

# À cette étape, on peut à nouveau diminuer le nombre de documents.
n_docs_significatifs <- 10

docs_significatifs_dt <- docs_significatifs_dt[1:n_docs_significatifs, ]

# On peut ajouter à ces documents toutes leurs métadonnées, y compris les textes.
docs_significatifs_dt <- merge.data.table(docs_significatifs_dt, xyz_dt, by="doc_id")

View(docs_significatifs_dt)


# Maintenant, on pourrait vouloir observer, dans les textes retenus, les mots qui précèdent et suivent immédiatement les variantes de "trahi".
# On peut construire d'abord une expression régulière:
pattern <- regex(".{0,50}trahir?.{0,50}", ignore_case = TRUE, dotall = TRUE)

# Avec la boucle suivante, on crée un concordancier très simple utilisant cette expression.
for (i in nrow(docs_significatifs_dt)) {

  kwic_temp <- unlist(str_extract_all(docs_significatifs_dt$texte, pattern))

  print(kwic_temp)
}

# On pourrait également observer, à l'aide d'un nuage de mots, les termes les plus importants des textes correspondant à la requête.

# À l'aide de notre fonction de prétraitement, nous pouvons nettoyer tous les textes de notre table.
for(i in 1:nrow(docs_significatifs_dt)) {
  docs_significatifs_dt$texte[i] <- pretraitement_f(docs_significatifs_dt$texte[i], antidictionnaire = lsa::stopwords_fr)
}

# On assemble maintenant les textes de nos documents en une seule table de fréquence.
mots_docs_significatifs_v <- strsplit(paste(docs_significatifs_dt$texte, collapse = " "), "\\W") |> unlist()
mots_docs_significatifs_v <- mots_docs_significatifs_v[!mots_docs_significatifs_v == ""]
mots_docs_significatifs_t <- sort(table(mots_docs_significatifs_v), decreasing = TRUE)

# On peut observer les 10 mots les plus fréquents de cette table:
mots_docs_significatifs_t[1:10]

# Et avec cette table, on crée notre nuage:
wordcloud2(data.frame(
  word = names(mots_docs_significatifs_t),
  freq = as.vector(mots_docs_significatifs_t)
), size = 0.8)


# On pourrait assembler en une seule fonction les étapes qui précèdent, fonction qui prendrait en entrée un motif, un seuil de pertinence cosinus et un nombre maximal de documents.
# C'est un exercice que vous pourrez faire pour vous assimiler la méthode.



#### Similarité documentaire ----

# Nettoyons d'abord notre environnement:
rm(list = setdiff(ls(), c("xyz_dtm", "pretraitement_f", "calcul_tfidf_f", "xyz_dt")))

# Nous allons appliquer une pondération TF-IDF aux valeurs de la matrice:
xyz_dtm_tfidf <- calcul_tfidf_f(as.matrix(xyz_dtm))

# Aperçu du résultat:
xyz_dtm_tfidf[2, xyz_dtm_tfidf[ 2, ] > 0 ]


# Normaliser la matrice pour neutraliser la longueur des textes:
normalize_L2 <- function(x) {
  return(x / sqrt(sum(x^2)))
}

xyz_dtm_tfidf_norm <- t(apply(as.matrix(xyz_dtm_tfidf), 1, normalize_L2))

# Aperçu du résultat
xyz_dtm_tfidf_norm[1, xyz_dtm_tfidf_norm[ 1, ] > 0 ]


# Calculer les valeurs de similarité entre chaque paire de documents:
xyz_sim_m <- as.matrix(simil(xyz_dtm_tfidf_norm, method = "cosine"))

# Aperçu du résultat:
xyz_sim_m[1:3, 1:3]

# Fonction pour obtenir l'indice du document le plus similaire
obtenir_doc_simil_f <- function(doc_index, matrice_simil) {
  
  # Extraire la ligne correspondant au document donné:
  similarities <- matrice_simil[doc_index, ]
  
  # Remplacer la similarité avec lui-même par -Inf (ou NA) pour éviter de se comparer avec lui-même:
  similarities[doc_index] <- -Inf
  
  # Trouver l'indice du document ayant la plus grande similarité:
  most_similar_doc <- which.max(similarities)
  
  # Retourner l'indice et la similarité maximale:
  return(list(
    doc_index = most_similar_doc,
    similarity = similarities[most_similar_doc]))
}

# Exemple : trouver le document le plus similaire au document 1
resultat <- obtenir_doc_simil_f(1, xyz_sim_m)
cat("Le document le plus similaire au document 1 est le document", resultat$doc_index, 
    "avec une similarité de", resultat$similarity, "\n")





#### Regroupement des documents (clustering) ----
# Le calcul de la similarité documentaire que nous avons fait précédemment ouvre la porte à l'une des tâches classiques du TAL.
# Cette tâche est la formation de groupes de documents sur la base d'un seuil de similarité.
# Dans le cadre de cette tâche, un document ne peut appartenir qu'à un seul groupe.
# Il existe deux principales méthodes de regroupement: 
# 1) la méthode agglomérative (ou ascendante), où l'on assemble en un groupe les documents les plus similaires, puis les groupes les plus similaires, et ainsi de suite.
# L'autre méthode est dite discriminante (ou descendante). Elle consiste à séparer en deux, puis en quatre, et ainsi de suite, l'ensemble des documents considérés initialement comme formant un seul groupe.
# Dans cette tâche, la mesure de similarité ou de distance (cosinus, euclidienne, Manhattan, etc.) est très importante.
# Une fois la mesure de distance obtenue pour chaque paire de documents, on appliquera un algorithme d'agglomération ou de séparation (ex.: ward.D ou ward.D2).
# Le résultat de cette opération pourra être transposé en un dendogramme, qui nous aidera à visualiser nos groupes.
# Un dendogramme classique ressemble à un arbre renversé. À la base se trouvent les documents (feuilles) et tout en haut, le tronc (agglomération totale de tous les documents).
# On pourra alors couper l'arbre à une certaine auteur pour obtenir un nombre donné de groupes.

# Le regroupement hiérarchique est une méthode de classification non supervisée qui permet d'explorer des structures cachées d'un jeu de données (textuelles).
# Il est utilisé dans toutes sortes de domaines (biologie, marketing, histoire de l'art). 
# En littérature et plus généralement dans les sciences du langage, il a fréquemment été utilisé pour explorer les thèmes d'une collection documentaire ou encore pour regrouper des auteurs selon le contenu de leurs œuvres.

# Ci-dessous, nous allons reprendre l'opération de vectorisation du corpus XYZ en appliquant des fonctions de pondération et de normalisation aux fréquences brutes.

tfidf <- text2vec::TfIdf$new(smooth_idf = TRUE, norm = 'l2', sublinear_tf = TRUE)

xyz_dtm_tfidf <- text2vec::fit_transform(xyz_dtm, tfidf)

str(xyz_dtm_tfidf)


# Maintenant, nous allons calculer la distance entre chaque document en utilisant la fonction proxyC::simil().
# Le résultat sera une matrice carrée de 564 documents X 564 documents.
# Au croisement de chaque paire de documents, dans la matrice de sortie, nous aurons une valeur qui correspond à la distance entre les deux documents.
xyz_doc_doc_dist_cosine <- 1 - proxyC::simil(x = xyz_dtm_tfidf, method = "cosine")

str(xyz_doc_doc_dist_cosine)

# Appliquée à cette matrice, la fonction agglomérative hclust() va produire un arbre complet, du tronc jusqu'aux feuilles, représentant les documents uniques.
# Une fois que nous aurons cet arbre, nous pourrons le visualiser sous la forme d'un dendogramme et déterminer le nombre de groupes que nous souhaitons obtenir (valeur `k`).
# Combien de groupes devrait-on idéalement former? Des méthodes avancées permettent de trouver le meilleur `k` pour un jeu de données,
# mais ces méthodes ne sont pas infaillibles et il faut souvent explorer plusieurs partitions pour trouver la meilleure (selon la question de recherche et l'objectif de l'exploration).

xyz_clust <- hclust(d = as.dist(xyz_doc_doc_dist_cosine), method = "ward.D")

str(xyz_clust)

# Vu le nombre tout de même conséquent de documents (564), le résultat sera de peu d'intérêt. Attendez-vous à un diagramme plutôt illisible!
plot(xyz_clust)

# On peut regarder quel serait l'effet d'une taille de l'arbre pour en tirer six groupes distincts.
rect.hclust(tree = xyz_clust, k = 6, border = "red")

# Pour commencer l'exploration, coupons l'arbre à `k`=6.
group_clust <- cutree(tree = xyz_clust, k = 6)

# Si on appelle cet objet, on verra que chaque document a été assigné à un groupe. 
# La table indiquera combien de documents sont associés à chacun des groupes.
table(group_clust)

# Chaque groupe ou "cluster" peut être exploré très finement et des sous-groupes peuvent être composés à partir d'une première partition.
# Dans l'immédiat, on explorera le vocabulaire de chaque groupe sous forme d'un nuage de mots.

# Prenons par exemple le premier groupe:
no_groupe <- 1

# On extrait les identifiants correspondant à ce groupe:
docs_groupe <- which(group_clust == no_groupe)

# On calcule la somme des lemmes correspondant aux documents du groupe:
sumMots_v <- Matrix::colSums(xyz_dtm_tfidf[docs_groupe,])

# On ordonne ce vecteur en fonction des valeurs décroissantes, de manière à retenir les mots les plus importants:
sumMots_v <- sort(sumMots_v, decreasing = TRUE)[1:50]

# On projette le résultat dans un nuages de mots:
wordcloud2(
  data.frame(
    word = names(sumMots_v),
    freq = as.vector(sumMots_v)
  ), size = .4
)

# Le résultat laisse croire que ce groupe contient des nouvelles où l'on trouve notamment des personnages de professeurs et d'étudiants en littérature!
# On pourrait retourner au texte et lire l'une des nouvelles de ce groupe:
xyz_dt[doc_id == names(docs_groupe)[2], .(texte)]




# Écrivons maintenant une fonction qui simplifiera l'exploration de nos groupes.

exploration_clus_f <- function(no_groupe, nbre_mots_nuage=50) {
  docs_groupe <- which(group_clust == no_groupe)
  sumMots_v <- Matrix::colSums(xyz_dtm_tfidf[docs_groupe,])
  sumMots_v <- sort(sumMots_v, decreasing = TRUE)[1:nbre_mots_nuage]
  return(
    wordcloud2(
      data.frame(
        word = names(sumMots_v),
        freq = as.vector(sumMots_v)
      ), size = .3
    )
  )
}
exploration_clus_f(no_groupe = 3)

# Utilisez la fonction pour explorer les groupes. Modifiez le nombre de `k`, recomposez de nouveaux groupes et explorez-les à nouveau avec la fonction de visualisation.


# À partir de cette exploration, on pourrait se demander si, par exemple, la thématique d'un numéro de la revue a un effet sur le vocabulaire des textes.
# On pourrait ajouter à l'identifiant unique de chaque document le thème du numéro, puis observer la classification hiérarchique pour voir si les textes ont tendance à être réunis au sein des mêmes clusters.


# Rappelons en guise de conclusion les avantages et désavantages du regroupement hiérarchique:
# Avantages :
# Simplicité: méthode facile à comprendre et à implémenter.
# Flexibilité: on n'a pas à préciser à l'avance un nombre défini de groupe.
# Visualisation: le dendrogramme offre une représentation intuitive de la hiérarchie des groupes et permet d'observer les effets de la coupe de l'arbre sur la partition.
# Dévoilement de structures sous-jacentes: il révèle des associations (horizontales et verticales ou ascendantes) entre documents qui ne sont pas forcément perceptibles à la lecture des textes.
# Exploration: il s'agit d'une méthode d'exploration des données non supervisée, qui ne requiert pas une connaissance préalable de la collection de documents, mais qui suscite un retour actif aux textes.

# Désavantages
# Perte d'information: parce que l'algorithme associe chaque document à un seul groupe, il y a une importante perte d'information: seuls ressortent les éléments les plus visibles ou caractéristiques de chaque groupe.
# Pas de probabilités: l'algorithme ne permet pas de savoir quelles sont les probabilités que tel document appartienne à tel groupe.
# Contrairement à l'algorithme du K-moyenne, on ne sait pas quel(s) document(s) sont plus centraux dans les groupes.
# Sensibilité à l'ordre des données: l'ordre dans lequel les données sont traitées peut influencer la structure du dendrogramme.




