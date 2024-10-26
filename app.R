
maria_df <- readRDS("donnees/maria_traite.RDS")
maria_dfm <- readRDS("donnees/maria_dfm.RDS")
maria_tfidf_dfm <- readRDS("donnees/maria_tfidf_dfm.RDS")
maria_lemma_dfm <- readRDS("donnees/maria_lemma_dfm.RDS")

inst_ext_f <- function(extension) {
  if(!extension %in% rownames(installed.packages())) {
    install.packages(extension, dependencies = TRUE)
  }
  require(extension, character.only = TRUE)
}

extensions <- c("stringr",                               # Module pour la manipulation des chaînes de caractères
                "tibble",                                # Module permettant de créer des tableaux de données avancés
                "quanteda",                              # Module spécialisé dans le traitement de textes
                "quanteda.textstats",                    # Module spécialisé dans les statistiques textuelles
                "gt",                                    # Module pour créer des tableau 
                "wordcloud2",                            # Module pour la création de nuages de mots
                "dplyr")                                 # Module pour la manipulation de structures de données

sapply(extensions, inst_ext_f)
# Assume maria_dfm and maria_tfidf_dfm are already created and available in your environment

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

# Define UI for application
ui <- fluidPage(
  titlePanel("Maria Chapdelaine: analyse de la distribution des mots par chapitre"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("chapitre", "Choisissez un chapitre :", 
                  choices = 1:16,
                  selected = 1),
      sliderInput("n", "Nombre de mots à afficher: ",
                  min = 1, max = 100, value = 10, step = 1)
    ),
    
    mainPanel(
      gt_output("tableau")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$tableau <- render_gt({
    req(input$chapitre) # Ensure the chapter is selected
    comparer_poids_f(input$chapitre, input$n)
  })

}

# Run the application 
shinyApp(ui = ui, server = server)