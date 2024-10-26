#######################################
#### Distribution et corrélation   ####
#######################################

# Après avoir observé le poids brut et pondéré des mots au sein des chapitres, on peut s'interroger sur la distribution de certains d'entre eux au fil des pages du roman.
# On peut également se demander si certains mots tendent à se retrouver fréquemment ensemble à l'intérieur d'une fenêtre de mots donnés (un chapitre entier ou un paragraphe, par exemple) ou si, au contraire, leur coprésence est rare.
# Dans l'atelier qui suit, nous allons d'abord observer la distribution, au sein de Maria Chapdelaine, de certains mots choisis du roman. 
# Ensuite, nous allons développer une méthode pour mesurer la corrélation des mots.
# Comme toujours, nous aurons besoin des expressions régulières pour saisir des motifs d'intérêt dans le texte.

# L'atelier s'inspire des chapitres 4 et 6 de Jockers (2020).

#### Chargement des modules supplémentaires ----
inst_ext_f <- function(extension) {
  if(!extension %in% rownames(installed.packages())) {
    install.packages(extension, dependencies = TRUE)
  }
  require(extension, character.only = TRUE)
}

extensions <- c("stringr",
                "data.table",
                "ggplot2",
                "tibble",
                "dplyr",
                "udpipe")                             

sapply(extensions, inst_ext_f)

### Importation de deux structures de données déjà traitées ----

# La première structure de données est le tableau dans lequel nous avons emmagasiné les mots et lemmes par chapitre
maria_df <- readRDS("donnees/maria_traite.RDS")

# Cette autre structure est plus brute: il s'agit du texte du roman contenu en un seul vecteur
maria_v <- readLines("donnees/maria_sans_epitexte.txt") |> paste(collapse = " ")


# Utilisons d'abord la structure de données simple, celle composée d'un seul long vecteur comprenant, les uns à la suite des autres, tous les mots du roman, y compris les titres de chapitres ("CHAPITRE I", etc.)
# Nous allons découper l'élément unique de ce vecteur en mots à l'aide du motif "\\W".

maria_split_v <- strsplit(maria_v, "\\W") |> unlist()

# La segmentation du texte a généré des éléments vides, que nous allons éliminer pour obtenir un vecteur propre ne contenant que les mots.
maria_split_v[1:100]

mots_pleins <- which(maria_split_v != "")

maria_split_v <- maria_split_v[mots_pleins]

maria_split_v[1:100]

# Le vecteur `maria_split_v` contient maintenant tous les mots du roman, chaque mot correspondant à un élément du vecteur.
# On peut calculer la longueur de ce vecteur, donc le nombre total de mots, simplement comme suit:

length(maria_split_v)

# On voudrait maintenant observer sous la forme d'un diagramme de dispersion la distribution de motifs (comme "François") le long du roman.
# La méthode est expliquée en détail dans Jockers (2020), p. 37-67. Je me contente ici de fabriquer une première fonction qui permette, pour un mot donné, de repérer les occurrences le long du vecteur-roman.

n_time_v <- seq(from = 1, to = length(maria_split_v))
w_count_v <- rep(NA, times = length(n_time_v))
which(maria_split_v == "François")

diagramme_dispersion_f <- function(mot) {
  n_time_v <- seq(from = 1, to = length(maria_split_v))
  w_count_v <- rep(NA, times = length(n_time_v))
  index_v <- which(maria_split_v == mot)
  w_count_v[index_v] <- 1
  diagramme_plot <- plot(w_count_v,
                         main = paste("Distribution du mot «", mot, "» dans Maria Chapdelaine"),
                         type = "h",
                         xlab = "Temps romanesque",
                         ylab = mot,
                         ylim = c(0,1),
                         yaxt = "n")
  return(diagramme_plot)
}

diagramme_dispersion_f("lorenzo")

# Il y a plusieurs manières d'améliorer ce diagramme.
# Tout d'abord, la fonction pourrait accepter une expression régulière au lieu d'un simple mot. 
# Cela permettrait par exemple d'attraper toutes les variantes d'un nom, voire tous les mots renvoyant à un concept ("froid", "neige", "hiver").
# Commençons par créer une fonction qui prenne en entrée un motif, puis voyons à quoi cela peut servir.

diagramme_dispersion_regex_f <- function(expression, mot_simplifie = "", ignore_case = FALSE) {
  if(mot_simplifie != ""){
    un_motif = mot_simplifie
  } else {
    un_motif <- expression
  }
  n_time_v <- seq(from = 1, to = length(maria_split_v))
  w_count_v <- rep(NA, times = length(n_time_v))
  index_v <- grep(expression, maria_split_v, ignore.case = ignore_case)
  w_count_v[index_v] <- 1
  diagramme_plot <- plot(w_count_v,
                         main = paste("Distribution du motif «", un_motif, "» dans Maria Chapdelaine"),
                         type = "h",
                         xlab = "Temps romanesque",
                         ylab = mot_simplifie,
                         ylim = c(0,1),
                         yaxt = "n")
  return(diagramme_plot)
}

# Essayons avec "François". À noter que la fonction prend deux arguments: 1) l'expression régulière et 2) un mot qui puisse servir de thème. 
# Si on ne fournit pas ce deuxième argument, l'expression régulière sera affichée dans le titre, ce qui pourrait n'être pas très avantageux du point de vue esthétique!
diagramme_dispersion_regex_f(expression = "hiver|froid|glace|neige", mot_simplifie = "Hiver", ignore_case = TRUE)

# Avec une telle fonction, on peut donner à voir la distribution non seulement d'un mot, mais d'un concept ou d'un thème relié à un ensemble de mots.
# Par exemple, si je suis intéressé par la représentation de la nature.

diagramme_dispersion_regex_f(expression = "forêt|arbre|\\bboisé?\\b", mot_simplifie = "Nature", ignore_case = TRUE)

#### Comparer la distribution de motifs sur l'axe du temps romanesque ----

# Nous pourrions souhaiter comparer la distribution de différents motifs au sein d'un même roman. C'est ce que fait la fonction ci-dessous.
# Cette fonction prend jusqu'à quatre expressions régulières en entrée et produit un diagramme de densité avec une couleur pour chacun des motifs.

# Création d'une structure de données composée des phrases du roman dans une seule colonne `Texte`.
maria_v <- paste(maria_df$texte, collapse = " ")
maria_phrases_v <- strsplit(maria_v, "(?<=\\.)\\s", perl = TRUE) |> unlist()
maria_phrases_df <- data.table(Id = 1:length(maria_phrases_v), Texte = maria_phrases_v)


# Création d'une fonction qui prend, en entrée, la structure de données ainsi que des motifs (jusqu'à 4 différents motifs).
plot_comparative_word_occurrences_f <-
  function(dt,
           pattern1,
           pattern2 = NULL,
           pattern3 = NULL,
           pattern4 = NULL,
           ignore_case = FALSE) {
    plot_data <-
      data.frame(
        Id = integer(),
        Pattern = character(),
        Ymin = numeric(),
        Ymax = numeric()
      )
    
    # Délimitations des segments selon le nombre de motfs
    segments <- list(c(0, 1 / 4), c(1 / 4, 1 / 2), c(1 / 2, 3 / 4), c(3 /
                                                                        4, 1))
    labels <- list(pattern1, pattern2, pattern3, pattern4)
    
    # Fonction pour ajouter les données
    add_matches_to_plot_data <-
      function(plot_data,
               pattern,
               pattern_name,
               ymin,
               ymax) {
        if (!is.null(pattern) && nzchar(pattern)) {
          matches <- dt[grepl(pattern, Texte, perl = TRUE, ignore.case = ignore_case), .(Id = Id)]
          if (nrow(matches) > 0) {
            new_rows <-
              data.frame(
                Id = matches$Id,
                Pattern = pattern_name,
                Ymin = rep(ymin, nrow(matches)),
                Ymax = rep(ymax, nrow(matches))
              )
            plot_data <- rbind(plot_data, new_rows)
          }
        }
        return(plot_data)
      }
    
    # Ajouter les données pour chaque motif
    for (i in 1:length(labels)) {
      if (!is.null(labels[[i]])) {
        plot_data <-
          add_matches_to_plot_data(plot_data,
                                   labels[[i]],
                                   paste0("Pattern", i),
                                   segments[[i]][1],
                                   segments[[i]][2])
      }
    }
    
    # Génère le diagramme de dispersion avec extension de la limite X pour placer les annotations
    p <- ggplot(plot_data) +
      geom_segment(aes(
        x = Id,
        xend = Id,
        y = Ymin,
        yend = Ymax,
        color = Pattern
      )) +
      scale_color_manual(
        values = c(
          "Pattern1" = "blue",
          "Pattern2" = "red",
          "Pattern3" = "green",
          "Pattern4" = "black"
        )
      ) +
      scale_x_continuous(limits = c(-50, max(dt$Id) * 1.05)) +
      theme_minimal() +
      labs(title = "Distribution de motifs dans le roman Maria Chapdelaine") +
      theme(
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
      )
    
    # Calculer la position des motifs
    text_positions <-
      lapply(1:length(segments), function(i)
        mean(segments[[i]]))
    
    # Ajouter les motifs à -50
    for (i in seq_along(labels)) {
      if (!is.null(labels[[i]])) {
        p <- p + annotate(
          "text",
          x = -50,
          y = text_positions[[i]],
          label = labels[[i]],
          color = "black",
          angle = 90,
          vjust = 0.5,
          size = 4,
          hjust = 0.5
        )
      }
    }
    
    print(p)
  }

# Utilisation de la fonction
plot_comparative_word_occurrences_f(dt = maria_phrases_df, pattern1 = "Maria", "François", "Eutrope", "Lorenzo")



# Dans l'exemple ci-dessus, nous comparons la distribution des noms de personnages le long de l'axe.
# Nous pourrions souhaiter comparer les densités entre un concept, l'idée de l'Amérique par exemple, et un personnage.
motif_personnage <- "lorenzo"
motif_amerique <- "\\bville|\\bcités?\\b|états|amérique"
plot_comparative_word_occurrences_f(dt = maria_phrases_df, motif_personnage, motif_amerique, ignore_case = TRUE)

#### Retour au texte: le concordancier ----
# Dans l'exemple ci-dessus, le mot "états" peut certes renvoyer aux États-Unis, mais il pourrait également renvoyer à des "états" d'un personnage, par exemple.
# Il convient donc de vérifier les éléments qui sont "attrapés" par l'expression de termes polysémiques.
# Pour faire cela, on utilisera avantageusement un concordancier qui renverra, pour un motif donné, les mots qui précèdent et ceux qui suivent.

# La fonction ci-dessous contient quatre arguments:
# 1) Un texte sous la forme d'un vecteur (tout le texte contenu dans un seul élément)
# 2) Un motif, c'est-à-dire une expression régulière
# 3) Un contexte, soit un nombre entier indiquant à la fonction combien de mots doivent être renvoyés avant et après le motif
# 4) ignore_case: si on souhaite ignorer la casse, on doit ajouter "ignore_case = TRUE")


concordancier_f <- function(texte, motif, contexte, ignore_case = FALSE) {
  # Split the text into individual words
  texte_split <- strsplit(texte, "\\s") |> unlist()
  
  # Find occurrences of the motif using grep with word boundaries to ensure exact matches
  occurrences_v <- grep(motif, texte_split, ignore.case = ignore_case, perl = TRUE)
  
  # Loop through each occurrence to extract context
  for (i in seq_along(occurrences_v)) {
    start <- occurrences_v[i] - contexte
    if (start < 1) {
      start <- 1
    }
    end <- occurrences_v[i] + contexte
    if (end >= length(texte_split)) {
      end <- length(texte_split)
    }
    
    # Extract the context around the motif
    output <- texte_split[start:end]
    
    # Find the index of the exact match within the output context
    motif_matches <- grep(motif, output, ignore.case = ignore_case, perl = TRUE)
    
    # Highlight the motif in the output by surrounding it with "[" and "]"
    output[motif_matches] <- paste0("[", output[motif_matches], "]")
    
    # Print the result
    cat("--------------------------------------------", i, "--------------------------------------------", "\n")
    cat(output, sep = " ", "\n")
  }
}

concordancier_f(texte = maria_v, motif = "états", contexte = 10, ignore_case = TRUE)

#### Corrélation ----
# Comment faire pour savoir si l'idée de l'Amérique, définie grossièrement par notre expression régulière, et le personnage de Lorenzo sont statistiquement corrélés, comme semble le montrer le diagramme?
# La corrélation est un concept utilisé pour mesurer l'association entre deux variables. 
# Si cette association est forte, la variation de la valeur de la variable A suivra la variation de valeur de la variable B.
# Lorsque les valeurs de deux variables augmentent ou diminuent ensemble, la corrélation est dite positive.
# Lorsque les valeurs des deux variables augmentent ou diminuent de manière inversée, la corrélation est dite négative.
# Pour mesurer la force de la relation ou de l'association entre les variables, on utilise fréquemment le coefficient de Pearson.
# Ce coefficient varie entre -1 et +1. Lorsque le résultat du test de Pearson révèle une corrélation de -1, les variables sont fortement corrélées de manière négative.
# À l'inverse, si le coefficient est de 1, les deux variables sont positivement et fortement corrélées.
# Plus le résultat du test se rapproche de zéro (0), plus la corrélation est faible.

# Pour mesurer la corrélation entre des termes, nous avons besoin d'un contexte à l'intérieur duquel nous allons vérifier si les termes sont ou non présents.
# On peut utiliser une unité neutre (ex.: tranches de 1000 mots) qui ne tienne pas compte de la division de l'œuvre.
# On peut également utiliser un contexte qui soit suggéré par l'œuvre, telle la division en chapitres. C'est ce que nous allons faire ici.

#### Préparation de la matrice de corrélation ----

# Nous disposons déjà d'un tableau de données (data frame) contenant une division du texte par chapitre. Nous utiliserons cette structure commode.

matrice_correlation_f <- function(dt = maria_df, motif_1, motif_2) {
  chapter_raws_l <- list()
  chapter_freqs_l <- list()
  for(i in 1:nrow(dt)) {
    chapter_title <- rownames(dt)[i]
    chapter_words_v <- dt$texte[i]
    chapter_word_v <- strsplit(chapter_words_v, "\\W") |> unlist()
    chapter_word_v <- chapter_word_v[which(chapter_word_v != "")]
    chapter_freqs_t <- table(chapter_word_v)
    chapter_raws_l[[chapter_title]] <- chapter_freqs_t
    chapter_freqs_t_rel <- 100*(chapter_freqs_t/sum(chapter_freqs_t))
    chapter_freqs_l[[chapter_title]] <- chapter_freqs_t_rel
  }
  premier_motif_l <- lapply(chapter_freqs_l, '[', motif_1)
  premier_motif_m <- do.call(rbind, premier_motif_l)
  deuxieme_motif_l <- lapply(chapter_freqs_l, '[', motif_2)
  deuxieme_motif_m <- do.call(rbind, deuxieme_motif_l)
  premier_motif_v <- as.vector(premier_motif_m[,1])
  deuxieme_motif_v <- as.vector(deuxieme_motif_m[,1])
  premier_deuxieme_motifs_m <- cbind(premier_motif_v,deuxieme_motif_v)
  colnames(premier_deuxieme_motifs_m) <- c(motif_1, motif_2)
  premier_deuxieme_motifs_m[which(is.na(premier_deuxieme_motifs_m))] <- 0
  return(premier_deuxieme_motifs_m)
}

# La ligne de code ci-dessous utilise la fonction ci-dessus à l'intérieur de la fonction `cor()`, qui calcule la corrélation.
cor(matrice_correlation_f(dt = maria_df, motif_1 = "Lorenzo", "États"))


# La fonction suivante, un peu améliorée, accepte des motifs (expressions régulières) au lieu de chaînes de caractères simples
matrice_correlation_f <- function(dt = maria_df, motif_1, motif_2, ignore_case=FALSE) {
  chapter_raws_l <- list()
  chapter_freqs_l <- list()
  for(i in 1:nrow(dt)) {
    chapter_title <- rownames(dt)[i]
    chapter_words_v <- dt$texte[i]
    chapter_word_v <- strsplit(chapter_words_v, "\\W") |> unlist()
    chapter_word_v <- chapter_word_v[which(chapter_word_v != "")]
    chapter_freqs_t <- table(chapter_word_v)
    chapter_raws_l[[chapter_title]] <- chapter_freqs_t
    chapter_freqs_t_rel <- 100*(chapter_freqs_t/sum(chapter_freqs_t))
    chapter_freqs_l[[chapter_title]] <- chapter_freqs_t_rel
  }
  
  # Utilisation d'expressions régulières avec grep
  premier_motif_l <- lapply(chapter_freqs_l, function(freq_table) {
    mots_trouves <- grep(motif_1, names(freq_table), value = TRUE, ignore.case = ignore_case)
    sum(freq_table[mots_trouves])
  })
  
  deuxieme_motif_l <- lapply(chapter_freqs_l, function(freq_table) {
    mots_trouves <- grep(motif_2, names(freq_table), value = TRUE, ignore.case = ignore_case)
    sum(freq_table[mots_trouves])
  })
  
  premier_motif_v <- unlist(premier_motif_l)
  deuxieme_motif_v <- unlist(deuxieme_motif_l)
  premier_deuxieme_motifs_m <- cbind(premier_motif_v, deuxieme_motif_v)
  colnames(premier_deuxieme_motifs_m) <- c(motif_1, motif_2)
  premier_deuxieme_motifs_m[which(is.na(premier_deuxieme_motifs_m))] <- 0
  
  return(premier_deuxieme_motifs_m)
}

# Dans la ligne ci-dessous, on passe à la fonction `cor()` de R, qui calcule la corrélation entre deux variable d'une matrice, la fonction créée ci-dessus.
cor(
  matrice_correlation_f(dt = maria_df, motif_1 = "lorenzo", "états|amérique", ignore_case = TRUE)
  )

# Le résultat est une mesure de la force de la dépendance linéaire entre les valeurs (fréquences relatives) des deux termes.
# Elle montre une très forte association positive indiquant que les probabilités qu'il soit question du personnage de Lorenzo lorsqu'il est question des États-Unis sont très élevées.


# On peut projeter les données de la matrice de corrélation dans un diagramme à barres
barplot(matrice_correlation_f(dt = maria_df, motif_1 = "états", "lorenzo", ignore_case = TRUE), beside = TRUE)


#### Le test de corrélation de Pearson ----
# L'une des questions qui se posent lorsqu'on mesure l'association entre deux variables est celle de savoir à quel point cette association est due au hasard.
# Le test de Pearson repose sur le concept d'hypothèse nulle selon laquelle deux valeurs ne sont pas corrélées.
# Il s'agit donc de mesurer, à partir du nombre d'observations (ici, 16 pour 16 chapitres) et des valeurs obtenues, avec quelle confiance on peut rejeter l'hypothèse nulle.
# L'une des mesures les plus souvent utilisées pour évaluer cette possibilité est la valeur p (probability value).
# La valeur p indique les chances que l'on obtienne par chance/hasard une valeur aussi extrême que la mesure de corrélation obtenue.
# Un "p" plus petit ou égal à 0.01: très forte présomption contre l'hypothèse nulle;
# Un "p" plus petit ou égal à 0.05: forte présomption contre l'hypothèse nulle;
# Un "p" entre 0.05 et 0.1: faible présomption contre l'hypothèse nulle: on ne peut la rejeter;


cor.test(matrice_correlation_f(dt = maria_df, motif_1 = "états|amérique|luxe", "lorenzo", ignore_case = TRUE)[,1],
         matrice_correlation_f(dt = maria_df, motif_1 = "états|amérique|luxe", "lorenzo", ignore_case = TRUE)[,2],
         method = "pearson")


# Dans le test de corrélation que nous avons fait, la valeur "p" est très nettement inférieure à 0.05 (elle est de 0.00000001281), ce qui indique une très forte présomption contre l'hypothèse nulle.
# Les chances que nous obtenions par hasard le même résultat que celui obtenu, soit une très forte corrélation entre les motifs "États" et "Lorenzo", sont extrêmement faibles.
# On peut donc rejeter l'hypothèse nulle et affirmer qu'une corrélation statistiquement significative existe entre les motifs.





#### Bibliographie ----
# Matthew L. Jockers et Rosamond Thalken, Text Analysis With R For Students in Literature, Springer, 2020.
# Wikipedia, "Valeur_p".
