# 🏘️ ImmoAI Tunisia   — Analyse & Prédiction des Prix Immobiliers

> **PFA 2025–2026 · TEK-UP University**  
> Méthodes Statistiques  
> Encadrant : Pr. Ahmed DHOUIBI

---

## 📌 Description

Projet de data science complet appliqué au marché immobilier tunisien. À partir de **12 748 annonces brutes** scrappées sur [tayara.tn](https://tayara.tn) via Kaggle, ce projet couvre l'intégralité du pipeline :

1. **Nettoyage et préparation** des données (pipeline 8 étapes)
2. **Analyse Exploratoire (EDA)** avec visualisations interactives
3. **Modélisation ML supervisée** — 5 modèles comparés
4. **Dashboard Shiny interactif** — Tunisia Real Estate

> 🎯 **Focus : Appartements uniquement** — filtre stratégique qui fait passer le R² de ~0.20 à **0.9484**

---

## 📊 Résultats des modèles

| Modèle | R² | RMSE |
|---|---|---|
| **R_polynomiale** ⭐ | **0.9484** | **0.6590** |
| RandomForest | 0.9482 | 0.6598 |
| R_lineaire | 0.9478 | 0.6628 |
| RL_interaction | 0.9477 | 0.6633 |
| XGBoost | 0.9443 | 0.6852 |

> Variable cible : `log(price)` · Jeu de test : 649 appartements

---

## 🗂️ Structure du projet

```
tunisia-real-estate/
│
├── data/
│   └── Property Prices in Tunisia.csv   # Dataset brut Kaggle
│
├── img_v2/                               # Captures dashboard Shiny
│   ├── dashboard_categories.png
│   ├── dashboard_modeles_table.png
│   ├── dist_logprix.png
│   ├── dist_alouer.png
│   ├── dist_avendre.png
│   ├── scatter_logprix.png
│   ├── scatter_alouer.png
│   ├── scatter_avendre.png
│   ├── logprix_median_type.png
│   ├── top10_villes_tous.png
│   ├── top10_villes_alouer.png
│   ├── top10_villes_avendre.png
│   ├── boxplot_pieces_tous.png
│   ├── boxplot_pieces_alouer.png
│   ├── boxplot_pieces_avendre.png
│   ├── r2_modeles.png
│   └── rmse_modeles.png
│
├── tunisia_real_estate.R                                 # Code R complet (EDA + ML + Shiny)
├── rapport_immobilier_tunisie.Rmd        # Rapport R Markdown
└── README.md
```

---

## 🔧 Pipeline de nettoyage

```
Dataset brut        →  12 748 lignes
Suppression doublons →  11 135 lignes  (−1 613)
Filtre Appartements →   4 240 lignes  (focus catégorie)
Suppression outliers →   3 243 lignes  (−997, règle IQR)
Split train / test  →   2 594 / 649
```

**Traitements appliqués :**
- Remplacement des valeurs `-1` par le **mode** (`room_count`, `bathroom_count`, `size`)
- Imputation des NA par la médiane (numériques) / `"Inconnu"` (caractères)
- Suppression des prix nuls ou négatifs
- Encodage : `type_num` (ordinal) · `city_num` (target encoding — médiane log_price par ville)
- Création de `log_price = log(price)` comme variable cible

---

## 🤖 Modèles implémentés

| Modèle | Formule | Variables |
|---|---|---|
| RL_interaction | `log_price ~ size × room_count + bathroom_count + type + city` | Toutes + interaction |
| R_polynomiale | `log_price ~ poly(size, 2) + room_count + bathroom_count + type + city` | Toutes |
| R_lineaire | `log_price ~ size + room_count + bathroom_count + type + city` | Toutes |
| RandomForest | `log_price ~ size + room_count + bathroom_count + type_num + city_num` | 5 variables encodées |
| XGBoost | `log_price ~ size + room_count + bathroom_count + type_num + city_num` | 5 variables encodées |

---

## 📦 Packages R requis

```r
install.packages(c(
  "ggplot2", "dplyr", "readr", "tidyr", "forcats",
  "caret", "randomForest", "xgboost",
  "shiny", "shinydashboard", "plotly"
))
```

---

## 🚀 Lancer le dashboard Shiny

```r
# 1. Cloner le repo
# 2. Placer le dataset dans data/
# 3. Lancer depuis RStudio ou la console R :

shiny::runApp("app.R")
```

Le dashboard s'ouvre automatiquement dans le navigateur avec 4 onglets :

| Onglet | Contenu |
|---|---|
| 🏠 Dashboard | ValueBoxes + graphiques RMSE & R² interactifs |
| 🔍 Exploration | EDA complète avec filtre dynamique (Tous / À Louer / À Vendre) |
| 📋 Modèles | Tableau comparatif + importance des variables RF & XGBoost |
| 🧮 Prédiction | Estimation du prix en temps réel selon les caractéristiques |

---

## 📁 Dataset

- **Source :** [Property Prices in Tunisia — Kaggle](https://www.kaggle.com/datasets/ghassen1302/property-prices-in-tunisia)
- **Auteur :** ghassen1302 · scraping tayara.tn
- **Licence :** CC BY 4.0
- **Dimensions brutes :** 12 748 observations · 9 variables

---

## 👥 Auteurs

| Nom | Filière |
|---|---|
| **Amir Rjeb** | Data Engineering & AI — TEK-UP University |
| **Abed Rahim Kaouech** | Data Engineering & AI — TEK-UP University |

---

## 📄 Licence

Ce projet est réalisé dans un cadre académique (PFA 2025–2026).  
