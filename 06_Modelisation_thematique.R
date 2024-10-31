#####################################################
#### La modélisation thématique (topic modeling) ####
#####################################################

# Dans le dernier atelier, nous avons exploré les avantages et les limites du regroupement hiérarchique pour classer des documents. 
# Ici, nous allons aborder une méthode qui pousse l’analyse un peu plus loin : la modélisation thématique, ou topic modeling. 
# Alors que le regroupement hiérarchique associe chaque document à un seul groupe-thème, la modélisation thématique, qui est également une méthode non supervisée, considère qu'un document est traversé par plusieurs thèmes. 
# Cette méthode de fouille permet ainsi d'aborder les documents de manière plus fine et complexe qu'un regroupement hiérarchique ou à plat.

# Un thème est une structure sémantique enfouie dans les textes, une structure que l'algorithme met au jour à travers les mots partagés.
# L’algorithme le plus largement utilisé pour faire ressortir ces thèmes est la LDA (Latent Dirichlet Allocation)
# Cet algorithme repose sur deux hypothèses :
# 1) chaque document contient un mélange de plusieurs thèmes;
# 2) chaque thème est formé d'un mélange de mots.

# Partant de ces deux hypothèses, l’algorithme identifie les mots les plus représentatifs d’un thème et évalue la part de chaque thème dans chaque document. 
# Une fois l'évaluation terminée et les proportions calculées, on peut obtenir les mots les plus probables de chaque thème et les documents qui contribuent le plus à ces thèmes.

### Sac de Mots (BoW) et Embeddings
# Pour construire ce modèle, nous utilisons ci-dessous une représentation simple du texte, de type « Bag of Words » (sac de mots).


#### Chargement des fonctions personnelles ----
# On importe dans l'environnement les fonctions personnelles:
source("fonctions.R")

# On installe et active les extensions externes:
extensions <- c("stringr",
                "data.table",
                "text2vec", 
                "udpipe",
                "ggplot2",
                "wordcloud2",
                "tm",
                "topicmodels", 
                "LDAvis",
                "umap",
                "ldatuning")                             

sapply(extensions, inst_ext_f)

rm(list = ls())


#### Importation des données ----
# Le tableau de donnéees ci-dessous est celui issu de l'atelier 4. Il contient les données et métadonnées de la collection de nouvelles publiées dans la revue XYZ.
xyz_enrichie_rich_lex <- readRDS("donnees/xyz_enrichie_rich_lex.RDS")

#### Vectorisation et création d'une matrice Documents-mots ----
# Pour construire notre premier modèle thématique, nous allons utiliser les lemmes de chaque nouvelle (document).
# Les lemmes que nous retiendrons sont les noms (communs et propres), les adjectifs et les verbes.
# L'ensemble du corpus XYZ a déjà été annoté avec udpipe et le modèle de language "french-gsd". 
# Nous allons importer cette structure de données pour éviter d'avoir à refaire l'opération d'annotation, longue et coûteuse:
xyz_pos <- readRDS("donnees/xyz_pos.RDS")

# On allège la structure de données en ne retenant que trois colonnes: `doc_id`, `lemma` et `pos` (Part-of-Speech):
xyz_pos_annote <- xyz_pos[, .(doc_id, lemma, upos)]


# On peut visualiser les 10 premières lignes de cette structure:
# xyz_pos_annote[1:10]

# Après avoir sélectionné les colonnes, on filtre les lignes en ne retenant que les lemma correspondant à des verbes, noms et adjectifs:
xyz_pos_annote <- xyz_pos_annote[upos %in% c('VERB','NOUN','ADJ', 'PROPN')]

# On rassemble ensuite les lemmes sous la forme de liste (une liste de lemmes par document) :
xyz_pos_annote <- xyz_pos_annote[,
                                 .(lemma = list(lemma)
                                 ),
                                 by = .(doc_id)]

# Visualisation de la structure de données:
xyz_pos_annote

# Nous allons maintenant imposer le bas de casse à tous les mots et remplacer les traits d'union par une espace simple:
xyz_pos_annote[, lemma:=sapply(lemma, tolower)]
xyz_pos_annote[, lemma:=sapply(lemma, str_replace_all, pattern = "[-]", replacement = " ")]

#### Vectorisation ----

# Créons maintenant une matrice de type documents-mots
iter <- text2vec::itoken(xyz_pos_annote$lemma,              
                         
                         ids = xyz_pos_annote$doc_id, 
                         
                         progressbar = FALSE)

vocab <- text2vec::create_vocabulary(iter,                       # Création d'une table comprenant tous les lemmes et leurs fréquences.
                                     ngram=c(ngram_min = 1,
                                             ngram_max = 1))     # On précise qu'on ne veut que des unigrammes.

vocab <- text2vec::prune_vocabulary(vocab,                       # On allège le vocabulaire en fournissant des seuils minimums et maximums de fréquences et de proportions documentaires.
                                    term_count_min = 5,
                                    doc_proportion_max = 0.5,
                                    doc_count_min = 3)

vocab <- data.table::setDT(vocab)                                # On transforme la structure en un tableau de type data.table.
vocab

vocab <- vocab[-1]

# Avec cette structure, nous allons créer la matrice documents-mots:
vectorizer <- text2vec::vocab_vectorizer(vocab)

xyz_dtm <- text2vec::create_dtm(iter,
                                vectorizer,
                                type = "dgTMatrix")

# saveRDS(xyz_dtm, "donnees/dtm4topicModels.RDS")
# Vous pouvez importer le résultat des dernières opérations:
xyz_dtm <- readRDS("donnees/dtm4topicModels.RDS")

# On peut visualiser les mots du premier document:
xyz_dtm[1 , xyz_dtm[1 ,] > 0]

# Nous allons maintenant créer notre premier modèle thématique, composé de 12 groupes (c'est un paramètre qu'il faut fournir dès le départ):
lda_model <- topicmodels::LDA(xyz_dtm, k = 12, control = list(seed = 1234))


# Il y a plusieurs manières de visualiser le résultat. Le plus intéressant outil de visualisation est à mon avis LDAViz, créé par Sievert et Shirley (2014).
# Cet outil permet de visualiser plusieurs aspects du modèle thématique de manière parallèle:
# -- La distribution des thèmes sur un plan en deux dimensions (réduction dimensionnelle): les thèmes qui partagent un ou plusieurs termes sont voisins, tandis que les thèmes sémantiquement distants sont situés dans des quadrants opposés;
# -- L'importance relative des topiques (taille des sphères qui les représentent);
# -- Les termes associés à chaque thème: en cliquant sur l'une des sphères, on fait apparaître à la droite les termes les plus fréquents (lambda = 1) pour ce topique ou ses termes les plus exclusifs (lambda = 0);
# -- Les topiques où un terme donné est présent: en cliquant sur un mot du vocabulaire, on peut visualiser les thèmes auxquels ce mot contribue le plus.

# Dans les lignes qui suivent, nous allons extraire de l'objet lda_model les différentes données dont le système LDAVis a besoin pour être affiché.

phi <- posterior(lda_model)$terms
theta <- posterior(lda_model)$topics

vocab <- colnames(xyz_dtm)  # Liste des mots.
doc_length <- rowSums(as.matrix(xyz_dtm))  # Longueur des documents.
term_frequency <- colSums(as.matrix(xyz_dtm))  # Fréquence des termes dans la collection de documents.

# Création d'une structure JSON pour la fonction de visualisation.
json_lda_12 <- createJSON(
  phi = phi,
  theta = theta,
  doc.length = doc_length,
  vocab = vocab,
  term.frequency = term_frequency
)

# L'exécution de la ligne suivante va susciter l'affichage du module de visualisation à la droite.
serVis(json_lda_12)

# Sauvegarde du modèle.
if(!dir.exists("resultats")) {dir.create("resultats")}
saveRDS(json_lda_12, "resultats/topicModel_12k.RDS")

# Le modèle thématique proposé ci-dessus utilise tous les documents de la collection XYZ. 
# On pourrait sélectionner une partie seulement de ces documents, comme les nouvelles écrites seulement par les femmes, ou celles d'un dossier thématique en particulier, ou encore celles publiées avant ou après une certaine date.


#### Trouver le meilleur modèle ----
# L'un des principaux défis de la modélisation thématique est de devoir fournir d'entrée de jeu, dès la création du modèle, le nombre de thèmes que celui-ci doit générer (Voir Arun et al., 2010, p. 391).
# La qualité de la partition dépend en grande partie de ce paramètre, mais, le plus souvent, on connaît mal ou peu le corpus, l'objectif de la modélisation étant précisément de l'explorer et d'en révéler les structures latentes!
# Il existe plusieurs méthodes pour découvrir de bonnes valeurs pour le paramètre `k`, des valeurs susceptibles de déboucher sur des partitions pertinentes.

# Les méthodes de Cao et Juan (2009) et de Ajun et al. (2010) mesurent la capacité du modèle à créer des thèmes distincts et cohérents. Ce sont les deux mesures que nous utiliserons ci-dessous.
# Pour en savoir davantage sur ces méthodes, on consultera avec profit la documentation de l'extension ldatuning:
# https://cran.r-project.org/web/packages/ldatuning/vignettes/topics.html

# L'extension ldatuning prend en entrée quelques éléments: 
# 1. Une plage ou séquence proposée de valeurs `k` à tester;
# 2. Une matrice Documents-mots;
# 3. Le nom des mesures à utiliser;
# 4. Le nom de la méthode;
# 5. Un `seed` pour la reproductibilité du test;
# 6. Le nombre de coeurs à utiliser sur l'ordinateur pour mener les tests de manière parallèle.

# Vous pouvez faire l'expérience par vous-même, mais celle-ci requiert d'importantes ressources computationnelles et pourrait prendre plusieurs minutes.
# Vous pouvez simplement importer le résultat du test en exécutant la ligne ci-dessous:
result <- readRDS("resultats/bestK_CaoJuan_Arun.RDS")

# # On définit ici une plage de valeurs de k:
# k_values <- seq(2, 20, by = 1)
# 
# # On crée une liste pour stocker les scores de cohérence:
# coherences <- numeric(length(k_values))
# 
# # L'instruction suivante lance l'évaluation des modèles à partir de différents "k":
# result <- FindTopicsNumber(
#   xyz_dtm,
#   topics = k_values,
#   metrics = c(
#     # "Griffiths2004", 
#     "CaoJuan2009",
#     "Arun2010"
#     #,
#     # "Deveaud2014"
#     ),
#   method = "Gibbs",
#   control = list(seed = 1234),
#   mc.cores = 8L,  # Changez selon le nombre de coeurs disponibles
#   verbose = TRUE
# )

# saveRDS(result, "resultats/bestK_CaoJuan_Arun.RDS")

# Visualiser les résultats
FindTopicsNumber_plot(result)

# Le diagramme donne à penser que les valeurs 5, 7, 8, 14, 18 et 20 seraient intéressantes à explorer.
# Voyons le modèle à 18 topiques.
# À nouveau, vous pouvez passer cette étape en important simplement le résultat du processus en exécutant la ligne suivante:
json_lda <- readRDS("resultats/topicModel_14k.RDS")

lda_model <- topicmodels::LDA(xyz_dtm, k = 14, control = list(seed = 1234))


phi <- posterior(lda_model)$terms
theta <- posterior(lda_model)$topics

vocab <- colnames(xyz_dtm)  # Liste des mots
doc_length <- rowSums(as.matrix(xyz_dtm))  # Longueur des documents
term_frequency <- colSums(as.matrix(xyz_dtm))  # Fréquence des termes dans le corpus

# Préparation pour LDAvis
json_lda <- createJSON(
  phi = phi,
  theta = theta,
  doc.length = doc_length,
  vocab = vocab,
  term.frequency = term_frequency
)

#saveRDS(json_lda, "resultats/topicModel_14k.RDS")
# L'exécution de la ligne suivante va susciter l'affichage du module de visualisation à la droite.
serVis(json_lda)

# Comme on l'a fait pour le regroupement hiérarchique, évoquons en guise de conclusion quelques avantages et inconvénients ou limites de la modélisation thématique:
# 
# Avantages :
# Détection de thèmes latents : elle identifie les thèmes sous-jacents dans une collection de documents, révélant des lignes de force sémantiques qui ne seraient pas perceptibles par la simple lecture des documents.
# Thèmes et documents : contrairement au regroupement hiérarchique, elle attribue à chaque document une probabilité d’appartenance à chaque thème, offrant une vue plus complexe des documents analysés.
# Adaptation à des corpus vastes : elle est bien adaptée aux grands ensembles de données textuelles, où le traitement manuel des thèmes serait laborieux, voire impossible.
# Exploration flexible : méthode non supervisée qui ne nécessite pas de connaître les thèmes à l’avance, ce qui en fait un excellent outil d’exploration pour des collections de textes variées et/ou inconnues.
# Outils de visualisation : les outils comme LDAvis permettent une visualisation interactive des thèmes, facilitant l’exploration des similarités et spécificités thématiques entre documents.
# 
# Limites :
# Paramétrage complexe : la qualité de la modélisation thématique dépend fortement du choix des paramètres (nombre de thèmes, cohérence, etc.), ce qui nécessite souvent des essais multiples pour parvenir à des résultats pertinents.
# Perte de nuances : malgré la distribution des mots en thèmes et des thèmes au sein des documents, il y a forcément une perte d'information et de nuances par rapport aux textes originaux.
# Dépendance au prétraitement : la qualité des thèmes est sensible au prétraitement des données (lemmatisation, élimination de mots vides), rendant les résultats variables selon les étapes de nettoyage appliquées.
# Interprétabilité des thèmes : les thèmes obtenus peuvent être difficiles à interpréter sans une connaissance fine du corpus. Un aller-retour du modèle final aux textes originaux, pour validation de l'interprétation, est inévitable.


# Pour aller plus loin
# Arun, R., Suresh, V., Veni Madhavan, C.E., Narasimha Murthy, M.N. "On Finding the Natural Number of Topics with Latent Dirichlet Allocation: Some Observations", dans Zaki, M.J., Yu, J.X., Ravindran, B., Pudi, V. (dirs.), Advances in Knowledge Discovery and Data Mining. PAKDD 2010. Lecture Notes in Computer Science(), vol 6118. Springer, Berlin, Heidelberg. https://doi-org.proxy3.library.mcgill.ca/10.1007/978-3-642-13657-3_43
# Matthew L. Jockers, "Topic Modeling", dans Text Analysis with R For Students of Literature, Springer, 2020, p. 211-235. 
# Martin Ponweiser, "Latent Dirichlet Allocation in R", These, Vienna University of Economics and Business, https://research.wu.ac.at/en/publications/latent-dirichlet-allocation-in-r-3

