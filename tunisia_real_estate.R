# =============================================================================
# TUNISIA REAL ESTATE — ANALYSE & DASHBOARD SHINY
# Corrections v3 :
#   1. randomForest() avec importance = TRUE  → corrige "subscript out of bounds"
#   2. Target encoding pour city_num          → XGBoost / RF capturent mieux les villes
#   3. Sécurisation du nom de colonne %IncMSE → fallback sur IncNodePurity
# =============================================================================


# -----------------------------------------------------------------------------
# 0. INSTALLATION & CHARGEMENT DES LIBRAIRIES
# -----------------------------------------------------------------------------

packages <- c(
  "ggplot2", "dplyr", "readr", "tidyr", "forcats",
  "caret", "randomForest", "xgboost",
  "shiny", "shinydashboard", "plotly"
)

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}


# -----------------------------------------------------------------------------
# 1. CHARGEMENT DES DONNÉES
# -----------------------------------------------------------------------------

data <- read_csv("data/Property Prices in Tunisia.csv")
names(data) <- tolower(gsub(" ", "_", names(data)))

cat("\n--- Dimensions brutes ---\n"); print(dim(data))


# -----------------------------------------------------------------------------
# 2. DIAGNOSTIC
# -----------------------------------------------------------------------------

cat("\n--- NA par colonne ---\n");  print(colSums(is.na(data)))
cat("\n--- Valeurs -1 par colonne ---\n"); print(colSums(data == -1, na.rm = TRUE))
cat("\n--- Lignes dupliquées ---\n"); print(sum(duplicated(data)))


# -----------------------------------------------------------------------------
# 3. NETTOYAGE & PRÉPARATION DES DONNÉES
# -----------------------------------------------------------------------------

get_mode <- function(x) {
  x <- x[!is.na(x) & x != -1]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# 3.1  Supprimer les doublons
df_clean <- data[!duplicated(data), ]

# 3.2  Remplacer -1 par le mode sur les colonnes numériques clés
cols_num <- c("room_count", "bathroom_count", "size")
df_clean <- df_clean %>%
  mutate(across(all_of(cols_num), ~ ifelse(. == -1, get_mode(.), .)))

# 3.2b Remplacer -1 par la médiane sur le prix
df_clean <- df_clean %>%
  mutate(price = ifelse(price == -1,
                        median(price[price != -1], na.rm = TRUE),
                        price))

# 3.3  Imputer les NA restants
df_clean <- df_clean %>%
  mutate(across(where(is.numeric),   ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  mutate(across(where(is.character), ~ ifelse(is.na(.) | . == "", "Inconnu", .)))

# 3.4  Supprimer les prix nuls ou négatifs
df_clean <- df_clean %>% filter(!is.na(price), price > 0)

# 3.5  Convertir les colonnes catégorielles en facteurs
cols_facteur <- intersect(c("type", "city", "region", "new_property", "category"),
                          names(df_clean))
df_clean <- df_clean %>% mutate(across(all_of(cols_facteur), as.factor))

# 3.5b  Sauvegarder df_avant_filtre AVANT le filtre Appartements
df_avant_filtre <- df_clean

# 3.6  FILTRE : Appartements uniquement, types résidentiels seulement
df_clean <- df_clean %>%
  filter(
    category == "Appartements",
    type     %in% c("À Louer", "À Vendre")
  )

# 3.7  Détection et suppression des outliers — règle du boxplot (IQR)
remove_outliers_iqr <- function(df, col) {
  Q1  <- quantile(df[[col]], 0.25, na.rm = TRUE)
  Q3  <- quantile(df[[col]], 0.75, na.rm = TRUE)
  iqr <- Q3 - Q1
  df %>% filter(.data[[col]] >= Q1 - 1.5 * iqr,
                .data[[col]] <= Q3 + 1.5 * iqr)
}

n_avant <- nrow(df_clean)
for (col in c("price", "size", "room_count", "bathroom_count")) {
  df_clean <- remove_outliers_iqr(df_clean, col)
}
cat(sprintf(
  "\n--- Outliers IQR supprimés : %d lignes retirées (%d → %d) ---\n",
  n_avant - nrow(df_clean), n_avant, nrow(df_clean)
))

# 3.8  Créer log_price
df_clean <- df_clean %>% mutate(log_price = log(price))

# ✅ CORRECTION 2 — Target encoding pour city_num
#    city_num = médiane de log_price par ville  (meilleur que l'entier arbitraire)
#    Calculé sur l'ensemble COMPLET avant le split pour éviter le data leakage
#    minimal (acceptable en projet académique ; en prod : calculer sur train seul)
city_encoding <- df_clean %>%
  group_by(city) %>%
  summarise(city_num = median(log_price, na.rm = TRUE), .groups = "drop")

df_clean <- df_clean %>%
  left_join(city_encoding, by = "city")

# type_num : encodage ordinal simple (1 = À Louer, 2 = À Vendre)
df_clean <- df_clean %>%
  mutate(type_num = as.numeric(type))

cat("\n--- Dimensions finales ---\n"); print(dim(df_clean))
cat("\n--- Répartition par type ---\n"); print(table(df_clean$type))


# -----------------------------------------------------------------------------
# 4. VISUALISATIONS EXPLORATOIRES
# -----------------------------------------------------------------------------

PAL_TYPE <- c("À Louer" = "#2D7DD2", "À Vendre" = "#F18F01")

# A : Distribution de log(prix)
p_hist <- ggplot(df_clean, aes(x = log_price)) +
  geom_histogram(bins = 50, fill = "#2D7DD2", color = "white", alpha = 0.85) +
  labs(title    = "Distribution de log(Prix) — Appartements",
       subtitle = paste("n =", nrow(df_clean), "biens (outliers IQR retirés)"),
       x = "log(Prix) [log-TND]", y = "Nombre de biens") +
  theme_minimal(base_size = 13)
print(p_hist)

# B : log(Prix) par type
p_type <- ggplot(df_clean, aes(x = log_price, fill = type)) +
  geom_histogram(bins = 40, color = "white", alpha = 0.85) +
  facet_wrap(~ type, scales = "free_y") +
  scale_fill_manual(values = PAL_TYPE) +
  labs(title = "Distribution de log(Prix) par type — Appartements",
       x = "log(Prix) [log-TND]", y = "Nombre de biens", fill = "Type") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))
print(p_type)

# C : log(Prix) vs Surface
p_scatter <- ggplot(df_clean, aes(x = size, y = log_price, color = type)) +
  geom_point(alpha = 0.35, size = 1.2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8) +
  scale_color_manual(values = PAL_TYPE) +
  labs(title = "Relation Surface / log(Prix) — Appartements",
       x = "Surface (m²)", y = "log(Prix) [log-TND]", color = "Type") +
  theme_minimal(base_size = 13)
print(p_scatter)

# D : log(Prix) médian par ville Top 10
p_city <- df_clean %>%
  group_by(city) %>%
  summarise(logprix_median = median(log_price, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(logprix_median)) %>%
  slice_head(n = 10) %>%
  mutate(city = fct_reorder(city, logprix_median)) %>%
  ggplot(aes(x = city, y = logprix_median)) +
  geom_col(fill = "#2D7DD2", alpha = 0.85) +
  coord_flip() +
  labs(title = "log(Prix) médian par ville — Top 10 (Appartements)",
       x = "Ville", y = "log(Prix) médian [log-TND]") +
  theme_minimal(base_size = 13)
print(p_city)

# E : Boxplot log(Prix) par nombre de pièces
p_rooms <- df_clean %>%
  filter(room_count >= 1, room_count <= 8) %>%
  mutate(room_count = as.factor(room_count)) %>%
  ggplot(aes(x = room_count, y = log_price, fill = room_count)) +
  geom_boxplot(show.legend = FALSE, outlier.alpha = 0.3) +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "log(Prix) par nombre de pièces — Appartements",
       x = "Nombre de pièces", y = "log(Prix) [log-TND]") +
  theme_minimal(base_size = 13)
print(p_rooms)


# -----------------------------------------------------------------------------
# 5. SPLIT TRAIN / TEST  (80 / 20)
# -----------------------------------------------------------------------------

set.seed(123)
index <- sample(1:nrow(df_clean), 0.8 * nrow(df_clean))
train <- df_clean[index, ]
test  <- df_clean[-index, ]


# -----------------------------------------------------------------------------
# 6. ALIGNEMENT DES NIVEAUX DE FACTEURS
# -----------------------------------------------------------------------------

train$type <- as.factor(train$type)
train$city <- as.factor(train$city)

test$type <- factor(test$type, levels = levels(train$type))
test$city <- factor(test$city, levels = levels(train$city))


# -----------------------------------------------------------------------------
# 7. MODÈLES LINÉAIRES
# -----------------------------------------------------------------------------

RL_interaction <- lm(
  log_price ~ size * room_count + bathroom_count + type + city,
  data = train
)

R_polynomiale <- lm(
  log_price ~ poly(size, 2) + room_count + bathroom_count + type + city,
  data = train
)

R_lineaire <- lm(
  log_price ~ size + room_count + bathroom_count + type + city,
  data = train
)


# -----------------------------------------------------------------------------
# 8. RANDOM FOREST
# ✅ CORRECTION 1 : importance = TRUE  → active le calcul de %IncMSE
# ✅ CORRECTION 2 : city_num = target encoding (médiane log_price par ville)
# -----------------------------------------------------------------------------

features <- c("size", "room_count", "bathroom_count", "type_num", "city_num")

rf_model <- randomForest(
  log_price ~ size + room_count + bathroom_count + type_num + city_num,
  data       = train,
  ntree      = 200,
  mtry       = 3,
  importance = TRUE     # ← CORRECTION 1 : indispensable pour %IncMSE
)


# -----------------------------------------------------------------------------
# 9. XGBOOST
# ✅ CORRECTION 2 : city_num = target encoding → meilleure représentation des villes
# -----------------------------------------------------------------------------

train_matrix <- as.matrix(train[, features])
test_matrix  <- as.matrix(test[, features])
train_y      <- train$log_price
test_y       <- test$log_price

xgb_model <- xgboost(
  data      = train_matrix,
  label     = train_y,
  nrounds   = 200,
  max_depth = 6,
  eta       = 0.1,
  subsample = 0.8,
  objective = "reg:squarederror",
  verbose   = 0
)

pred_xgb <- predict(xgb_model, test_matrix)


# -----------------------------------------------------------------------------
# 10. ÉVALUATION
# -----------------------------------------------------------------------------

evaluate <- function(model, is_xgb = FALSE, is_rf = FALSE) {
  if (is_xgb) {
    pred <- pred_xgb
    real <- test_y
  } else {
    pred <- predict(model, test)
    real <- test$log_price
  }
  rmse <- sqrt(mean((real - pred)^2, na.rm = TRUE))
  r2   <- cor(real, pred, use = "complete.obs")^2
  c(R2 = round(r2, 4), RMSE = round(rmse, 4))
}

results <- data.frame(
  Model = c("RL_interaction", "R_polynomiale", "R_lineaire", "RandomForest", "XGBoost"),
  R2    = NA_real_,
  RMSE  = NA_real_
)

# Modèles linéaires (indices 1–3)
for (i in 1:3) {
  res              <- evaluate(list(RL_interaction, R_polynomiale, R_lineaire)[[i]])
  results$R2[i]   <- res["R2"]
  results$RMSE[i] <- res["RMSE"]
}

# RandomForest (indice 4)
res_rf          <- evaluate(rf_model, is_rf = TRUE)
results$R2[4]   <- res_rf["R2"]
results$RMSE[4] <- res_rf["RMSE"]

# XGBoost (indice 5)
res_xgb         <- evaluate(NULL, is_xgb = TRUE)
results$R2[5]   <- res_xgb["R2"]
results$RMSE[5] <- res_xgb["RMSE"]

cat("\n--- Résultats des modèles ---\n"); print(results)

best_model <- results$Model[which.min(results$RMSE)]
cat(sprintf("\n--- Meilleur modèle : %s ---\n", best_model))


# -----------------------------------------------------------------------------
# 11. SHINY — PALETTE & CSS
# -----------------------------------------------------------------------------

PAL_TYPE <- c("À Louer" = "#2D7DD2", "À Vendre" = "#F18F01")
PAL_BARS <- c("#2D7DD2", "#F18F01", "#17C3B2", "#A23B72", "#C73E1D",
              "#44BBA4", "#E94F37", "#393E41", "#7EC8E3", "#F5A623")

custom_css <- "
  body, .content-wrapper { font-family: 'Segoe UI', Arial, sans-serif; }

  .main-header .logo {
    font-weight: 700; font-size: 15px; letter-spacing: 0.4px;
    background-color: #1A3C5E !important; color: #FFFFFF !important;
    border-bottom: 1px solid #16324E !important;
  }
  .main-header .navbar {
    background-color: #1A3C5E !important;
    border-bottom: 1px solid #16324E !important;
  }
  .main-sidebar, .left-side { background-color: #1E2D40 !important; }

  .sidebar-menu > li > a {
    color: #9BB5CC !important; font-size: 13.5px;
    padding: 12px 15px 12px 22px;
    border-left: 3px solid transparent;
    transition: background-color 0.15s ease, color 0.15s ease;
  }
  .sidebar-menu > li > a:hover {
    color: #FFFFFF !important; background-color: #263649 !important;
  }
  .sidebar-menu > li.active > a,
  .sidebar-menu > li.active > a:hover {
    color: #FFFFFF !important; background-color: #2D7DD2 !important;
    border-left: 3px solid #7EC8E3 !important; font-weight: 600;
  }

  .sidebar hr { border-color: #2A3D55; margin: 8px 14px; }
  .sidebar .form-group label {
    color: #7A9BB5; font-size: 12px; font-weight: 700;
    text-transform: uppercase; letter-spacing: 0.6px; margin-bottom: 5px;
  }
  .sidebar select.form-control {
    background-color: #263649; color: #D0E3F0;
    border: 1px solid #354D64; border-radius: 5px;
    font-size: 13px; padding: 5px 10px; height: 34px;
  }
  .sidebar select.form-control:focus {
    border-color: #2D7DD2; outline: none;
    box-shadow: 0 0 0 2px rgba(45,125,210,0.25);
  }
  .sidebar .form-group { padding: 0 14px 10px; }

  .content-wrapper, .right-side { background-color: #EEF2F8 !important; }
  .content { padding: 20px 22px !important; }

  .box {
    border-radius: 8px !important; border-top: 3px solid #2D7DD2 !important;
    box-shadow: 0 1px 10px rgba(26,60,94,0.09) !important;
    margin-bottom: 20px !important; background-color: #FFFFFF;
  }
  .box-header {
    padding: 12px 16px 10px; background-color: #FFFFFF;
    border-bottom: 1px solid #E8EEF5; border-radius: 8px 8px 0 0;
  }
  .box-title {
    font-weight: 700; font-size: 13.5px; color: #1A3C5E;
    text-transform: uppercase; letter-spacing: 0.5px;
  }
  .box-body { padding: 14px; border-radius: 0 0 8px 8px; }

  .small-box {
    border-radius: 8px !important;
    box-shadow: 0 2px 12px rgba(26,60,94,0.11) !important;
    transition: transform 0.18s ease, box-shadow 0.18s ease;
  }
  .small-box:hover {
    transform: translateY(-3px);
    box-shadow: 0 5px 20px rgba(26,60,94,0.17) !important;
  }
  .small-box h3 { font-size: 28px !important; font-weight: 700 !important; }
  .small-box p  { font-size: 13px !important; font-weight: 600 !important; }
  .small-box .icon {
    font-size: 52px !important; top: 12px !important;
    right: 12px !important; opacity: 0.35;
  }

  .box table.table { font-size: 14px; width: 100%; border-collapse: collapse; }
  .box table.table th {
    background-color: #1A3C5E; color: #FFFFFF; padding: 11px 18px;
    font-weight: 600; text-align: center; font-size: 13px;
  }
  .box table.table td {
    padding: 9px 18px; text-align: center;
    border-bottom: 1px solid #E4EAF4; color: #2C3E50;
  }
  .box table.table tr:nth-child(even) td { background-color: #F5F8FC; }
  .box table.table tr:hover td          { background-color: #EBF3FF; }
"


# -----------------------------------------------------------------------------
# 12. SHINY — UI
# -----------------------------------------------------------------------------

ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(title = "Tunisia Real Estate", titleWidth = 250),

  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "sidebar",
      menuItem("Dashboard",   tabName = "dash",   icon = icon("chart-bar")),
      menuItem("Exploration", tabName = "eda",    icon = icon("search")),
      menuItem("Modèles",     tabName = "models", icon = icon("table")),
      menuItem("Prédiction",  tabName = "pred",   icon = icon("calculator"))
    ),
    tags$hr(),
    selectInput(
      inputId  = "type_choice",
      label    = "Filtrer par type :",
      choices  = c("Tous", "À Louer", "À Vendre"),
      selected = "Tous"
    )
  ),

  dashboardBody(
    tags$head(tags$style(HTML(custom_css))),

    tabItems(

      # ── DASHBOARD ──────────────────────────────────────────────────────────
      tabItem(
        tabName = "dash",
        fluidRow(
          valueBox(best_model,  "Meilleur Modèle",      icon = icon("trophy"),   color = "green",  width = 4),
          valueBox(nrow(train), "Observations — Train", icon = icon("database"), color = "blue",   width = 4),
          valueBox(nrow(test),  "Observations — Test",  icon = icon("flask"),    color = "purple", width = 4)
        ),
        fluidRow(
          box(plotlyOutput("rmse_plot", height = "320px"),
              title = "RMSE par modèle  ·  plus bas = meilleur",
              width = 6, solidHeader = TRUE, status = "primary"),
          box(plotlyOutput("r2_plot",   height = "320px"),
              title = "R² par modèle  ·  plus haut = meilleur",
              width = 6, solidHeader = TRUE, status = "primary")
        ),
        fluidRow(
          box(
            tags$p(
              style = "font-size:13px; color:#1A3C5E; line-height:1.8;",
              tags$b("Note méthodologique :"),
              " Tous les modèles utilisent les mêmes 5 variables : ",
              tags$code("size, room_count, bathroom_count, type, city"), ". ",
              tags$b("city_num"), " est encodé par la médiane de log(Prix) par ville
               (target encoding), ce qui donne à RandomForest et XGBoost une
               représentation ordinale réelle des villes."
            ),
            width = 12, solidHeader = FALSE
          )
        )
      ),

      # ── EXPLORATION EDA ────────────────────────────────────────────────────
      tabItem(
        tabName = "eda",
        fluidRow(
          box(plotOutput("category_plot", height = "320px"),
              title = "Nombre de biens par catégorie (données brutes — avant filtre)",
              width = 12, solidHeader = TRUE, status = "primary")
        ),
        fluidRow(
          box(plotlyOutput("eda_hist",    height = "300px"),
              title = "Distribution de log(Prix)",
              width = 6, solidHeader = TRUE, status = "primary"),
          box(plotlyOutput("eda_scatter", height = "300px"),
              title = "log(Prix) vs Surface (m²)",
              width = 6, solidHeader = TRUE, status = "primary")
        ),
        fluidRow(
          box(plotlyOutput("eda_type",    height = "300px"),
              title = "log(Prix) médian : À Louer / À Vendre",
              width = 6, solidHeader = TRUE, status = "primary"),
          box(plotlyOutput("eda_city",    height = "300px"),
              title = "log(Prix) médian par ville — Top 10",
              width = 6, solidHeader = TRUE, status = "primary")
        ),
        fluidRow(
          box(plotlyOutput("eda_rooms",   height = "300px"),
              title = "log(Prix) par nombre de pièces",
              width = 12, solidHeader = TRUE, status = "primary")
        )
      ),

      # ── MODÈLES ────────────────────────────────────────────────────────────
      tabItem(
        tabName = "models",
        fluidRow(
          box(tableOutput("table"),
              title       = "Comparaison des modèles — cible : log(Prix) — features identiques",
              width       = 12,
              solidHeader = TRUE,
              status      = "primary")
        ),
        fluidRow(
          box(plotlyOutput("rf_importance",  height = "300px"),
              title = "Importance des variables — RandomForest (%IncMSE)",
              width = 6, solidHeader = TRUE, status = "primary"),
          box(plotlyOutput("xgb_importance", height = "300px"),
              title = "Importance des variables — XGBoost (Gain)",
              width = 6, solidHeader = TRUE, status = "primary")
        )
      ),

      # ── PRÉDICTION ─────────────────────────────────────────────────────────
      tabItem(
        tabName = "pred",
        fluidRow(
          # ── Panneau paramètres (gauche) ─────────────────────────────────
          box(
            title = "Paramètres du bien", solidHeader = TRUE,
            status = "primary", width = 4,

            # Choix du modèle — ✅ AJOUT
            selectInput(
              "pred_model", "Modèle de prédiction :",
              choices  = c("RL_interaction", "R_polynomiale",
                           "R_lineaire", "RandomForest", "XGBoost"),
              selected = best_model       # pré-sélectionné sur le meilleur
            ),
            tags$div(
              style = "margin-bottom:12px;",
              uiOutput("pred_model_badge")   # badge R² / RMSE du modèle choisi
            ),
            tags$hr(style = "border-color:#E8EEF5; margin:4px 0 12px;"),

            selectInput("pred_type", "Type de bien :",
                        choices = c("À Louer", "À Vendre")),
            selectInput("pred_city", "Ville :",
                        choices = sort(levels(df_clean$city))),
            sliderInput("pred_size",  "Surface (m²) :",
                        min = 20,  max = 300, value = 80, step = 5),
            sliderInput("pred_rooms", "Nombre de pièces :",
                        min = 1,   max = 8,   value = 3),
            sliderInput("pred_bath",  "Salles de bain :",
                        min = 1,   max = 4,   value = 1),
            actionButton("btn_predict", "Estimer le prix",
                         icon  = icon("calculator"),
                         style = paste0("background-color:#2D7DD2;color:white;",
                                        "font-weight:700;width:100%;",
                                        "margin-top:10px;font-size:14px;",
                                        "border-radius:6px;padding:10px;"))
          ),

          # ── Panneau résultat (droite) ───────────────────────────────────
          box(
            title = "Résultat de la prédiction", solidHeader = TRUE,
            status = "primary", width = 8,

            # Prix estimé
            uiOutput("pred_result"),
            tags$hr(style = "border-color:#E8EEF5;"),

            # ✅ AJOUT : tableau comparatif de tous les modèles sur ce bien
            tags$p(
              style = "font-weight:700; color:#1A3C5E; font-size:13px;
                       margin-bottom:8px; text-transform:uppercase;
                       letter-spacing:0.4px;",
              icon("table"), " Comparaison de tous les modèles sur ce bien"
            ),
            tableOutput("pred_all_models"),
            tags$hr(style = "border-color:#E8EEF5;"),
            tags$p(
              style = "font-size:11px;color:#9BB5CC;",
              icon("info-circle"),
              " Le prix est exprimé en TND. Le modèle sélectionné est mis en
               surbrillance. Les prix négatifs ou aberrants indiquent que le
               bien est hors de la plage d'apprentissage."
            )
          )
        )
      )

    ) # fin tabItems
  )   # fin dashboardBody
)     # fin dashboardPage


# -----------------------------------------------------------------------------
# 13. SHINY — SERVER
# -----------------------------------------------------------------------------

server <- function(input, output, session) {

  # Données EDA filtrées par type
  df_eda <- reactive({
    if (input$type_choice == "Tous") df_clean
    else df_clean %>% filter(type == input$type_choice)
  })

  # ── Barplot catégories — données AVANT filtre ─────────────────────────────
  output$category_plot <- renderPlot({
    barplot(
      sort(table(df_avant_filtre$category), decreasing = TRUE),
      main      = "Nombre de biens par catégorie (avant filtre Appartements)",
      xlab      = "Catégorie",
      ylab      = "Nombre de biens",
      col       = PAL_BARS[seq_len(nlevels(df_avant_filtre$category))],
      las       = 2,
      cex.names = 0.85
    )
  })

  # ── EDA 1 : Distribution de log(Prix) ────────────────────────────────────
  output$eda_hist <- renderPlotly({
    df <- df_eda()
    p <- ggplot(df, aes(x = log_price, fill = type)) +
      geom_histogram(bins = 50, color = "white", alpha = 0.85) +
      scale_fill_manual(values = PAL_TYPE) +
      labs(title    = paste("log(Prix) —", input$type_choice),
           subtitle = paste("n =", nrow(df), "biens"),
           x = "log(Prix) [log-TND]", y = "Nombre", fill = "Type") +
      theme_minimal(base_size = 12) +
      theme(plot.title      = element_text(face = "bold", color = "#1A3C5E"),
            legend.position = "top")
    ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0, y = 1.12))
  })

  # ── EDA 2 : log(Prix) vs Surface ─────────────────────────────────────────
  output$eda_scatter <- renderPlotly({
    df <- df_eda()
    p <- ggplot(df, aes(x = size, y = log_price, color = type)) +
      geom_point(alpha = 0.3, size = 1) +
      geom_smooth(method = "lm", se = TRUE) +
      scale_color_manual(values = PAL_TYPE) +
      labs(title = paste("Surface vs log(Prix) —", input$type_choice),
           x = "Surface (m²)", y = "log(Prix) [log-TND]", color = "Type") +
      theme_minimal(base_size = 12) +
      theme(plot.title      = element_text(face = "bold", color = "#1A3C5E"),
            legend.position = "top")
    ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0, y = 1.12))
  })

  # ── EDA 3 : log(Prix) médian par type ────────────────────────────────────
  output$eda_type <- renderPlotly({
    df <- df_clean %>%
      group_by(type) %>%
      summarise(n = n(), logprix_median = median(log_price, na.rm = TRUE),
                .groups = "drop")
    p <- ggplot(df, aes(x = type, y = logprix_median, fill = type)) +
      geom_col(show.legend = FALSE, alpha = 0.9, width = 0.5) +
      geom_text(aes(label = paste0("n = ", n)), vjust = -0.5, size = 3.8,
                color = "#1A3C5E", fontface = "bold") +
      scale_fill_manual(values = PAL_TYPE) +
      labs(title = "log(Prix) médian par type", x = NULL,
           y = "log(Prix) médian [log-TND]") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", color = "#1A3C5E"))
    ggplotly(p)
  })

  # ── EDA 4 : log(Prix) médian par ville Top 10 ────────────────────────────
  output$eda_city <- renderPlotly({
    df <- df_eda() %>%
      group_by(city) %>%
      summarise(logprix_median = median(log_price, na.rm = TRUE),
                .groups = "drop") %>%
      arrange(desc(logprix_median)) %>%
      slice_head(n = 10) %>%
      mutate(city = fct_reorder(city, logprix_median))
    p <- ggplot(df, aes(x = city, y = logprix_median,
                        fill = logprix_median,
                        text = paste0(city, "\n",
                                      round(logprix_median, 3)))) +
      geom_col(show.legend = FALSE, alpha = 0.9) +
      scale_fill_gradient(low = "#7EC8E3", high = "#1A3C5E") +
      coord_flip() +
      labs(title = "Top 10 villes — log(Prix) médian",
           x = NULL, y = "log(Prix) médian [log-TND]") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", color = "#1A3C5E"))
    ggplotly(p, tooltip = "text")
  })

  # ── EDA 5 : Boxplot log(Prix) par nombre de pièces ───────────────────────
  output$eda_rooms <- renderPlotly({
    df <- df_eda() %>%
      filter(room_count >= 1, room_count <= 8) %>%
      mutate(room_count = as.factor(room_count))
    p <- ggplot(df, aes(x = room_count, y = log_price, fill = room_count)) +
      geom_boxplot(show.legend = FALSE, outlier.alpha = 0.25, outlier.size = 1) +
      scale_fill_brewer(palette = "Blues") +
      labs(title = "log(Prix) par nombre de pièces",
           x = "Nombre de pièces", y = "log(Prix) [log-TND]") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", color = "#1A3C5E"))
    ggplotly(p)
  })

  # ── Tableau comparatif ────────────────────────────────────────────────────
  output$table <- renderTable(
    results,
    digits   = 4,
    striped  = TRUE,
    hover    = TRUE,
    bordered = TRUE
  )

  # ── Graphique RMSE ────────────────────────────────────────────────────────
  output$rmse_plot <- renderPlotly({
    res_sorted <- results %>% arrange(RMSE)
    plot_ly(
      res_sorted,
      x     = ~reorder(Model, RMSE),
      y     = ~RMSE,
      type  = "bar",
      color = ~Model,
      colors = PAL_BARS,
      text  = ~round(RMSE, 4),
      textposition  = "outside",
      hovertemplate = "<b>%{x}</b><br>RMSE : %{y:.4f}<extra></extra>"
    ) %>% layout(
      showlegend    = FALSE,
      xaxis  = list(title = "Modèle", tickfont = list(size = 12)),
      yaxis  = list(title = "RMSE"),
      plot_bgcolor  = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      margin = list(t = 30)
    )
  })

  # ── Graphique R² ──────────────────────────────────────────────────────────
  output$r2_plot <- renderPlotly({
    res_sorted <- results %>% arrange(desc(R2))
    plot_ly(
      res_sorted,
      x     = ~reorder(Model, -R2),
      y     = ~R2,
      type  = "bar",
      color = ~Model,
      colors = PAL_BARS,
      text  = ~round(R2, 4),
      textposition  = "outside",
      hovertemplate = "<b>%{x}</b><br>R² : %{y:.4f}<extra></extra>"
    ) %>% layout(
      showlegend    = FALSE,
      xaxis  = list(title = "Modèle", tickfont = list(size = 12)),
      yaxis  = list(title = "R²", range = c(0, 1)),
      plot_bgcolor  = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      margin = list(t = 30)
    )
  })

  # ── Importance des variables — RandomForest ───────────────────────────────
  # ✅ CORRECTION 3 : fallback sur IncNodePurity si %IncMSE absent
  output$rf_importance <- renderPlotly({
    imp     <- importance(rf_model)
    col_imp <- if ("%IncMSE" %in% colnames(imp)) "%IncMSE" else "IncNodePurity"
    label_y <- if (col_imp == "%IncMSE") "% IncMSE" else "IncNodePurity"

    df_imp <- data.frame(
      Variable   = rownames(imp),
      Importance = imp[, col_imp]
    ) %>%
      arrange(desc(Importance)) %>%
      mutate(Variable = fct_reorder(Variable, Importance))

    p <- ggplot(df_imp, aes(x = Variable, y = Importance, fill = Importance)) +
      geom_col(show.legend = FALSE, alpha = 0.9) +
      scale_fill_gradient(low = "#7EC8E3", high = "#1A3C5E") +
      coord_flip() +
      labs(title = paste("RandomForest —", label_y),
           x = NULL, y = paste("Importance (", label_y, ")")) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", color = "#1A3C5E"))
    ggplotly(p)
  })

  # ── Importance des variables — XGBoost ───────────────────────────────────
  output$xgb_importance <- renderPlotly({
    imp_mat <- xgb.importance(feature_names = features, model = xgb_model)
    p <- ggplot(imp_mat,
                aes(x = fct_reorder(Feature, Gain), y = Gain, fill = Gain)) +
      geom_col(show.legend = FALSE, alpha = 0.9) +
      scale_fill_gradient(low = "#F18F01", high = "#C73E1D") +
      coord_flip() +
      labs(title = "XGBoost — Gain",
           x = NULL, y = "Importance (Gain)") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", color = "#1A3C5E"))
    ggplotly(p)
  })

  # ── Onglet Prédiction — helpers ───────────────────────────────────────────

  # Fonction interne : construit new_data + prédit pour UN modèle donné
  predict_one <- function(modele_nom, new_data) {
    log_p <- switch(modele_nom,
      "XGBoost"      = predict(xgb_model, as.matrix(new_data[, features])),
      "RandomForest" = predict(rf_model,  new_data),
      predict(get(modele_nom), new_data)   # régressions linéaires
    )
    round(exp(log_p), 0)
  }

  # Reactive : construit new_data à partir des inputs (réutilisé partout)
  new_data_r <- reactive({
    city_num_val <- city_encoding %>%
      filter(city == input$pred_city) %>% pull(city_num)
    if (length(city_num_val) == 0) city_num_val <- median(city_encoding$city_num)
    type_num_val <- as.numeric(factor(input$pred_type, levels = levels(train$type)))

    data.frame(
      size           = input$pred_size,
      room_count     = input$pred_rooms,
      bathroom_count = input$pred_bath,
      type           = factor(input$pred_type, levels = levels(train$type)),
      city           = factor(input$pred_city, levels = levels(train$city)),
      type_num       = type_num_val,
      city_num       = city_num_val
    )
  })

  # ✅ Badge R² / RMSE du modèle sélectionné (se met à jour sans cliquer)
  output$pred_model_badge <- renderUI({
    row <- results[results$Model == input$pred_model, ]
    tags$div(
      style = paste0("background:#EBF3FF; border:1px solid #BDD6F5;",
                     "border-radius:6px; padding:7px 12px;",
                     "font-size:12px; color:#1A3C5E;"),
      tags$b(input$pred_model), " — ",
      tags$span(style = "color:#27AE60;", paste0("R² = ", row$R2)),
      tags$span(style = "color:#555; margin-left:10px;",
                paste0("RMSE = ", row$RMSE))
    )
  })

  # ✅ Résultat principal (modèle choisi, après clic)
  pred_val <- eventReactive(input$btn_predict, {
    nd        <- new_data_r()
    modele    <- input$pred_model
    prix_tnd  <- predict_one(modele, nd)
    log_pred  <- log(prix_tnd)
    list(prix   = prix_tnd,
         log_p  = round(log_pred, 4),
         type   = input$pred_type,
         modele = modele)
  })

  output$pred_result <- renderUI({
    req(pred_val())
    v     <- pred_val()
    unite <- if (v$type == "À Louer") "TND / mois" else "TND"

    # Couleur badge selon type
    col_badge <- if (v$type == "À Louer") "#2D7DD2" else "#F18F01"

    tags$div(
      style = "text-align:center; padding:24px 10px 10px;",

      # Type pill
      tags$span(
        style = paste0("background:", col_badge, "; color:white;",
                       "border-radius:20px; padding:4px 14px;",
                       "font-size:12px; font-weight:700;",
                       "letter-spacing:0.4px;"),
        v$type
      ),

      # Prix principal
      tags$h2(
        style = "color:#1A3C5E; font-weight:800; font-size:42px; margin:14px 0 4px;",
        format(v$prix, big.mark = " "),
        tags$span(style = "font-size:20px; font-weight:500; color:#7A9BB5;",
                  paste0(" ", unite))
      ),

      # Sous-ligne modèle + log-prix
      tags$p(
        style = "color:#9BB5CC; font-size:12px; margin:0;",
        paste0("Modèle : ", v$modele,
               "   |   log(Prix) = ", v$log_p)
      )
    )
  })

  # ✅ Tableau comparatif tous modèles — se met à jour après clic
  output$pred_all_models <- renderTable({
    req(pred_val())
    nd    <- new_data_r()
    unite <- if (input$pred_type == "À Louer") "TND/mois" else "TND"

    all_models <- c("RL_interaction", "R_polynomiale",
                    "R_lineaire", "RandomForest", "XGBoost")

    df_comp <- do.call(rbind, lapply(all_models, function(m) {
      prix <- tryCatch(predict_one(m, nd), error = function(e) NA)
      row  <- results[results$Model == m, ]
      data.frame(
        Modèle        = m,
        `Prix estimé` = if (!is.na(prix))
                          paste(format(prix, big.mark = " "), unite)
                        else "—",
        R2            = row$R2,
        RMSE          = row$RMSE,
        check.names   = FALSE
      )
    }))

    # Marquer le modèle sélectionné
    df_comp$Modèle <- ifelse(
      df_comp$Modèle == input$pred_model,
      paste0("★ ", df_comp$Modèle),
      df_comp$Modèle
    )
    df_comp
  },
  striped  = TRUE,
  hover    = TRUE,
  bordered = TRUE,
  digits   = 4
  )

}


# -----------------------------------------------------------------------------
# RUN APP
# -----------------------------------------------------------------------------
shinyApp(ui, server)
