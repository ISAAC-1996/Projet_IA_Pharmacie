# Projet IA Pharmacie — Analyse & Prédiction du CA des Officines en France

Ce projet vise à construire une IA intelligente, autonome et évolutive pour analyser, surveiller et prédire le chiffre d'affaires des pharmacies françaises, tout en détectant les tendances du marché santé.

---

## Objectif principal

> Créer une plateforme IA capable de :

- **Collecter automatiquement des données publiques** (INSEE, ANSM, Google Trends, Data.gouv.fr…)
- **Prévoir le chiffre d'affaires mensuel ou saisonnier** (Prophet, XGBoost…)
- **Surveiller les grandes tendances santé et marché** (web scraping, forums, Google Trends)
- **Détecter les nouveautés produits, ruptures, ou mouvements du marché** (scraping sites spécialisés, APIs)
- **Générer des alertes sectorielles** (nouvelles lois, appels à projets, subventions, changement de réglementation)
- **Fournir des recommandations personnalisées** (ex. stock à renforcer, pathologie en hausse…)
- **Afficher les données dans un dashboard interactif** (Shiny, flexdashboard)
- **S’auto-mettre à jour régulièrement** (via cron ou planificateur, sans intervention manuelle)


---

## Structure du projet
Projet_IA_Pharmacie/
├── Creation_Datalake/        # Scripts de collecte & web scraping
├── Data_Nettoyage/           # Nettoyage, formatage, validation
├── Analyse/                  # Modèles statistiques et IA
├── Visualisation/            # Graphiques, dashboards Shiny
├── data_raw/                 # Données brutes collectées
├── data_clean/               # Données prêtes à modéliser
├── results/                  # Résultats des modèles, exports
├── README.md                 # Documentation du projet
└── .gitignore                # Fichiers exclus de Git


---

##Technologies & Packages R

| Besoin | Package |
|--------|---------|
| Web Scraping | `rvest`, `httr`, `xml2` |
| API | `httr`, `jsonlite`, `gtrendsR` |
| Traitement | `dplyr`, `data.table`, `tidyr` |
| Modélisation | `prophet`, `xgboost`, `caret`, `forecast` |
| Visualisation | `ggplot2`, `plotly`, `shiny`, `flexdashboard` |
| Automatisation | `cronR`, `taskscheduleR` |
| Stockage | `RSQLite`, `DBI`, `PostgreSQL` |

---

##Données utilisées

- **INSEE** : données démographiques, économiques
- **Data.gouv.fr** : annuaire des pharmacies
- **ANSM / Médicaments** : bases publiques
- **Google Trends** : suivi de recherches santé
- **Scraping** : forums santé, sites e-commerce pharma

---

##Modules IA

- **Prévision de CA** (Prophet, séries temporelles)
- **Détection de tendances santé** (Google Trends + NLP)
- **Segmentation des pharmacies** (KMeans, DBSCAN)
- **Scoring de performance produit / zone**

---

#Déploiement prévu

- Dashboard Shiny (local → cloud)
- Rapport automatisé PDF/Excel
- Notifications par mail (via `blastula`)

---

##Prochaine étape

- Scraper la base des pharmacies sur Data.gouv.fr
- Construire le Data Lake local
- Intégrer les données INSEE pour enrichir le modèle

---

## 👤 Auteur

Projet initié par **ISAAC-1996**  
amabileisaac@gmail.com

---

## Licence

Projet open-source — libre d’utilisation avec attribution. Respect du RGPD requis pour toute donnée sensible.
