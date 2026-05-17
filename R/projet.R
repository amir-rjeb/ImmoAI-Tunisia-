# =============================================================================
# MINI-PROJET - Analyse Multivariée
# Dataset : Property Prices in Tunisia (Kaggle - tayara.tn)
# =============================================================================
# ÉTAPE 2 : PRÉPARATION DES DONNÉES
# =============================================================================
# Auteur  : [Ton nom]
# Module  : Méthodes statistiques et étude de données
# =============================================================================


# -----------------------------------------------------------------------------
# 0. WORKING DIRECTORY + PACKAGES
# -----------------------------------------------------------------------------

setwd("C:/Users/Amir rjeb/Desktop/R")
cat("Working directory :", getwd(), "\n")

packages_necessaires <- c("tidyverse", "naniar", "corrplot", "scales")

for (pkg in packages_necessaires) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cran.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

cat("Tous les packages sont chargés.\n")


# -----------------------------------------------------------------------------
# 1. CHARGEMENT DES DONNÉES
# -----------------------------------------------------------------------------

data_raw <- read.csv("data/Property Prices in Tunisia.csv",
                     header     = TRUE,
                     sep        = ",",
                     encoding   = "UTF-8",
                     na.strings = c("", "NA", "N/A", "null", "NULL"))

cat("=== APERÇU DU DATASET BRUT ===\n")
print(head(data_raw, 6))

cat("\nNombre de lignes   :", nrow(data_raw), "\n")
cat("Nombre de colonnes :", ncol(data_raw), "\n")
cat("Colonnes           :", paste(names(data_raw), collapse = ", "), "\n")

# Structure détaillée
cat("\nTypes des colonnes :\n")
print(str(data_raw))

cat("\n=== RÉSUMÉ STATISTIQUE INITIAL ===\n")
print(summary(data_raw))


# -----------------------------------------------------------------------------
# 2. TRAITEMENT DES VALEURS -1 (= données manquantes déguisées)
# -----------------------------------------------------------------------------

# IMPORTANT : dans ce dataset, -1 signifie "valeur non renseignée"
# (notamment pour room_count, bathroom_count, size)
# On les convertit en NA vrais avant tout traitement.

cat("\n=== REMPLACEMENT DES -1 PAR NA ===\n")

# Colonnes numériques concernées par les -1
cols_avec_moins1 <- c("room_count", "bathroom_count", "size")

data_raw_corrige <- data_raw

for (col in cols_avec_moins1) {
  if (col %in% names(data_raw_corrige)) {
    nb <- sum(data_raw_corrige[[col]] == -1, na.rm = TRUE)
    data_raw_corrige[[col]][data_raw_corrige[[col]] == -1] <- NA
    cat("Colonne [", col, "] :", nb, "valeurs -1 → remplacées par NA\n")
  }
}

cat("\n=== TAUX DE NA RÉELS (après conversion des -1) ===\n")
taux_na_reel <- data.frame(
  Variable = names(data_raw_corrige),
  Taux_NA  = round(colSums(is.na(data_raw_corrige)) / nrow(data_raw_corrige) * 100, 2)
)
taux_na_reel <- taux_na_reel[order(-taux_na_reel$Taux_NA), ]
print(taux_na_reel)

# Visualisation des valeurs manquantes réelles
vis_miss(data_raw_corrige) +
  labs(title    = "Carte des valeurs manquantes (après conversion des -1)",
       subtitle = "Les -1 représentaient des données non renseignées") +
  theme(plot.title    = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))


# -----------------------------------------------------------------------------
# 3. SUPPRESSION DES DOUBLONS
# -----------------------------------------------------------------------------

data_clean <- data_raw_corrige

nb_doublons <- sum(duplicated(data_clean))
cat("\n=== DOUBLONS ===\n")
cat("Doublons détectés  :", nb_doublons, "\n")
data_clean <- unique(data_clean)
cat("Lignes après nettoyage :", nrow(data_clean), "\n")


# -----------------------------------------------------------------------------
# 4. SUPPRESSION DE log_price (colonne redondante)
# -----------------------------------------------------------------------------

# log_price est une transformation de price → on la supprime pour éviter
# la redondance dans l'ACP. On pourra la recalculer si besoin.

if ("log_price" %in% names(data_clean)) {
  data_clean$log_price <- NULL
  cat("\nColonne [log_price] supprimée (redondante avec price).\n")
}

cat("Colonnes restantes :", paste(names(data_clean), collapse = ", "), "\n")


# -----------------------------------------------------------------------------
# 5. IMPUTATION DES VALEURS MANQUANTES
# -----------------------------------------------------------------------------

# Identifier les colonnes numériques et catégorielles
cols_num <- names(data_clean)[sapply(data_clean, is.numeric)]
cols_cat <- names(data_clean)[sapply(data_clean, is.character)]

cat("\nVariables numériques  :", paste(cols_num, collapse = ", "), "\n")
cat("Variables catégorielles:", paste(cols_cat, collapse = ", "), "\n")

# Fonction pour calculer le mode
get_mode <- function(x) {
  x    <- x[!is.na(x)]
  uniq <- unique(x)
  uniq[which.max(tabulate(match(x, uniq)))]
}

cat("\n=== IMPUTATION ===\n")

# Variables numériques → MÉDIANE
# Justification : la médiane est robuste aux valeurs extrêmes.
# Pour l'immobilier (prix très asymétriques), elle est préférable à la moyenne.
for (col in cols_num) {
  nb_na <- sum(is.na(data_clean[[col]]))
  if (nb_na > 0) {
    val <- median(data_clean[[col]], na.rm = TRUE)
    data_clean[[col]][is.na(data_clean[[col]])] <- val
    cat("Numérique  [", col, "] :", nb_na, "NA → médiane =", round(val, 2), "\n")
  }
}

# Variables catégorielles → MODE
for (col in cols_cat) {
  nb_na <- sum(is.na(data_clean[[col]]))
  if (nb_na > 0) {
    val <- get_mode(data_clean[[col]])
    data_clean[[col]][is.na(data_clean[[col]])] <- val
    cat("Catégorielle [", col, "] :", nb_na, "NA → mode =", val, "\n")
  }
}

cat("\nTotal NA restants :", sum(is.na(data_clean)), "\n")


# -----------------------------------------------------------------------------
# 6. DÉTECTION ET TRAITEMENT DES OUTLIERS (méthode IQR)
# -----------------------------------------------------------------------------

cat("\n=== DÉTECTION DES OUTLIERS — MÉTHODE IQR ===\n")
cat("Règle : outlier si x < Q1 - 1.5*IQR  ou  x > Q3 + 1.5*IQR\n\n")

# On analyse les variables numériques pertinentes (pas room_count/bathroom_count
# qui sont des entiers discrets à faible variance)
cols_outliers <- c("price", "size", "room_count", "bathroom_count")
cols_outliers <- intersect(cols_outliers, names(data_clean))

for (col in cols_outliers) {
  Q1      <- quantile(data_clean[[col]], 0.25, na.rm = TRUE)
  Q3      <- quantile(data_clean[[col]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  nb_out  <- sum(data_clean[[col]] < (Q1 - 1.5 * IQR_val) |
                   data_clean[[col]] > (Q3 + 1.5 * IQR_val), na.rm = TRUE)
  cat("Variable [", col, "] →", nb_out,
      "outliers (", round(nb_out / nrow(data_clean) * 100, 2), "%)\n")
}

# --- Boxplot AVANT winsorizing ---
p_avant <- ggplot(data_clean, aes(y = price)) +
  geom_boxplot(fill = "#E74C3C", color = "#2C3E50",
               outlier.color = "red", outlier.shape = 16, outlier.size = 1.5) +
  scale_y_continuous(labels = scales::comma) +
  labs(title    = "Boxplot — Prix AVANT traitement outliers",
       subtitle = "Points rouges = outliers détectés par méthode IQR",
       y = "Prix (TND)") +
  theme_minimal() +
  theme(plot.title    = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))
print(p_avant)

# --- Winsorizing : plafonner aux bornes IQR ---
# Justification : les prix très élevés (villas de luxe, grands terrains)
# existent réellement en Tunisie. On PLAFONNE plutôt que supprimer
# pour conserver toutes les observations et ne pas biaiser l'analyse.

cat("\n=== WINSORIZING (plafonnement aux bornes IQR) ===\n")

for (col in cols_outliers) {
  Q1        <- quantile(data_clean[[col]], 0.25, na.rm = TRUE)
  Q3        <- quantile(data_clean[[col]], 0.75, na.rm = TRUE)
  IQR_val   <- Q3 - Q1
  borne_inf <- Q1 - 1.5 * IQR_val
  borne_sup <- Q3 + 1.5 * IQR_val
  
  nb_inf <- sum(data_clean[[col]] < borne_inf, na.rm = TRUE)
  nb_sup <- sum(data_clean[[col]] > borne_sup, na.rm = TRUE)
  
  data_clean[[col]][data_clean[[col]] < borne_inf] <- borne_inf
  data_clean[[col]][data_clean[[col]] > borne_sup] <- borne_sup
  
  cat("Variable [", col, "] :",
      nb_inf, "valeurs plafonnées en bas |",
      nb_sup, "valeurs plafonnées en haut\n")
}

# --- Boxplot APRÈS winsorizing ---
p_apres <- ggplot(data_clean, aes(y = price)) +
  geom_boxplot(fill = "#2ECC71", color = "#2C3E50") +
  scale_y_continuous(labels = scales::comma) +
  labs(title    = "Boxplot — Prix APRÈS winsorizing",
       subtitle = "Plus d'outliers extrêmes",
       y = "Prix (TND)") +
  theme_minimal() +
  theme(plot.title    = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))
print(p_apres)

cat("\nLignes après traitement outliers :", nrow(data_clean), "\n")


# -----------------------------------------------------------------------------
# 7. NORMALISATION Z-SCORE
# -----------------------------------------------------------------------------

cat("\n=== NORMALISATION Z-SCORE ===\n")
cat("Formule : z = (x - moyenne) / écart-type\n")
cat("Résultat : moyenne = 0, écart-type = 1\n")
cat("Justification : l'ACP est sensible aux unités — sans normalisation,\n")
cat("la variable 'price' (milliers de TND) dominerait toutes les autres.\n\n")

# On normalise uniquement les variables numériques continues
cols_a_normaliser <- intersect(cols_num, names(data_clean))

data_normalise <- data_clean

for (col in cols_a_normaliser) {
  m <- mean(data_normalise[[col]], na.rm = TRUE)
  s <- sd(data_normalise[[col]],   na.rm = TRUE)
  if (s > 0) {
    data_normalise[[col]] <- (data_normalise[[col]] - m) / s
  }
}

# Vérification
cat("Vérification moyennes après Z-score (doivent être ≈ 0) :\n")
for (col in cols_a_normaliser) {
  cat(" ", col, "→ moyenne =", round(mean(data_normalise[[col]]), 6), "\n")
}

cat("\nVérification écarts-types après Z-score (doivent être ≈ 1) :\n")
for (col in cols_a_normaliser) {
  cat(" ", col, "→ sd =", round(sd(data_normalise[[col]]), 6), "\n")
}


# -----------------------------------------------------------------------------
# 8. ENCODAGE DES VARIABLES CATÉGORIELLES EN FACTEURS
# -----------------------------------------------------------------------------

cat("\n=== ENCODAGE EN FACTEURS ===\n")

for (col in cols_cat) {
  if (col %in% names(data_clean)) {
    data_clean[[col]]     <- as.factor(data_clean[[col]])
    data_normalise[[col]] <- as.factor(data_normalise[[col]])
    cat("Variable [", col, "] → facteur |",
        nlevels(data_clean[[col]]), "niveaux\n")
  }
}

# Aperçu des niveaux
cat("\nNiveaux de [type]     :", levels(data_clean$type), "\n")
cat("Niveaux de [category] (top 5) :", paste(head(levels(data_clean$category), 5), collapse = ", "), "...\n")


# -----------------------------------------------------------------------------
# 9. VISUALISATIONS DESCRIPTIVES FINALES
# -----------------------------------------------------------------------------

cat("\n=== VISUALISATIONS DESCRIPTIVES ===\n")

# Distribution du prix (après nettoyage)
p1 <- ggplot(data_clean, aes(x = price)) +
  geom_histogram(fill = "#3498DB", color = "white", bins = 50) +
  scale_x_continuous(labels = scales::comma) +
  labs(title    = "Distribution des prix immobiliers en Tunisie",
       subtitle = paste("n =", nrow(data_clean), "observations (après nettoyage)"),
       x = "Prix (TND)", y = "Nombre de biens") +
  theme_minimal() +
  theme(plot.title    = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))
print(p1)

# Distribution de la superficie
p2 <- ggplot(data_clean, aes(x = size)) +
  geom_histogram(fill = "#9B59B6", color = "white", bins = 50) +
  labs(title    = "Distribution des superficies (m²)",
       subtitle = "Après imputation et winsorizing",
       x = "Superficie (m²)", y = "Nombre de biens") +
  theme_minimal() +
  theme(plot.title    = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))
print(p2)

# Répartition par type (Vente vs Location)
p3 <- ggplot(data_clean, aes(x = type, fill = type)) +
  geom_bar(show.legend = FALSE) +
  labs(title = "Répartition Vente vs Location",
       x = "Type de transaction", y = "Nombre d'annonces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(p3)

# Top 10 catégories de biens
top_cat <- sort(table(data_clean$category), decreasing = TRUE)[1:10]
df_cat  <- data.frame(category = names(top_cat), n = as.numeric(top_cat))

p4 <- ggplot(df_cat, aes(x = reorder(category, n), y = n, fill = category)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Top 10 des catégories de biens",
       x = "Catégorie", y = "Nombre d'annonces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(p4)

# Top 15 villes
top_villes <- sort(table(data_clean$city), decreasing = TRUE)[1:15]
df_villes  <- data.frame(city = names(top_villes), n = as.numeric(top_villes))

p5 <- ggplot(df_villes, aes(x = reorder(city, n), y = n, fill = city)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Top 15 des villes",
       x = "Ville", y = "Nombre d'annonces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(p5)

# Prix moyen par ville (top 10)
prix_par_ville <- data_clean %>%
  group_by(city) %>%
  summarise(prix_moyen = mean(price, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(prix_moyen)) %>%
  head(10)

p6 <- ggplot(prix_par_ville, aes(x = reorder(city, prix_moyen),
                                 y = prix_moyen, fill = city)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Prix moyen par ville (Top 10)",
       x = "Ville", y = "Prix moyen (TND)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(p6)

# Matrice de corrélation des variables numériques
mat_cor <- cor(data_clean[, cols_a_normaliser], use = "complete.obs")

cat("\n=== MATRICE DE CORRÉLATION ===\n")
print(round(mat_cor, 3))

corrplot(mat_cor,
         method      = "color",
         type        = "upper",
         tl.cex      = 0.9,
         addCoef.col = "black",
         number.cex  = 0.8,
         col         = colorRampPalette(c("#E74C3C", "white", "#3498DB"))(200),
         title       = "Matrice de corrélation — Variables numériques",
         mar         = c(0, 0, 2, 0))


# -----------------------------------------------------------------------------
# 10. SAUVEGARDE DES FICHIERS
# -----------------------------------------------------------------------------

write.csv(data_clean,
          file      = "data/data_clean.csv",
          row.names = FALSE)

write.csv(data_normalise,
          file      = "data/data_normalise.csv",
          row.names = FALSE)

cat("\n=== FICHIERS SAUVEGARDÉS ===\n")
cat("data/data_clean.csv     →", nrow(data_clean),     "lignes x", ncol(data_clean),     "colonnes\n")
cat("data/data_normalise.csv →", nrow(data_normalise), "lignes x", ncol(data_normalise), "colonnes\n")


# -----------------------------------------------------------------------------
# 11. RÉCAPITULATIF FINAL
# -----------------------------------------------------------------------------

cat("\n")
cat("=================================================================\n")
cat("  RÉCAPITULATIF — ÉTAPE 2 : PRÉPARATION DES DONNÉES\n")
cat("=================================================================\n")
cat("  Dataset brut            :", nrow(data_raw),   "lignes x", ncol(data_raw),   "col.\n")
cat("  Dataset nettoyé         :", nrow(data_clean), "lignes x", ncol(data_clean), "col.\n")
cat("  Valeurs -1 converties   : NA réels (room_count, bathroom_count, size)\n")
cat("  Doublons supprimés      :", nb_doublons, "\n")
cat("  Colonne supprimée       : log_price (redondante avec price)\n")
cat("  NA imputés              : médiane (num.) / mode (cat.)\n")
cat("  Outliers traités        : winsorizing IQR\n")
cat("  Normalisation           : Z-score (variables numériques)\n")
cat("  Encodage                : facteurs (category, type, city, region)\n")
cat("  Fichiers produits       : data_clean.csv | data_normalise.csv\n")
cat("=================================================================\n")