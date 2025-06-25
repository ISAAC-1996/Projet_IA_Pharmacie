# OSPHARM Application R Shiny - Version Complète avec notifications, calendrier et filtre syndicat
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ospharm2)
library(tidyverse)
library(plotly)
library(dygraphs)
library(DT)
library(xts)
library(shinyjs)

table_actifs <- actives_datastat()
view(table_actifs)

table_inactifs <- inactifs_datastat()
view(table_inactifs)

# Interface utilisateur
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center;",
      tags$img(
        src = "https://isaac-1996.github.io/Localisation/logo.png",
        height = "35px",
        style = "margin-right: 10px;"
      ),
      tags$span("OSPHARM", class = "logo-text", style = "font-size: 20px; font-weight: bold; color: #22c55e;")
    ),
    titleWidth = 250,
    
    # Zone A: Notifications (cloche)
    dropdownMenu(type = "notifications", icon = icon("bell"),
                 badgeStatus = "danger", headerText = "Vous avez 5 notifications",
                 notificationItem(
                   text = "Erreur critique système",
                   icon("exclamation-triangle"),
                   status = "danger"
                 ),
                 notificationItem(
                   text = "Dysfonctionnement base de données",
                   icon("database"),
                   status = "warning"
                 ),
                 notificationItem(
                   text = "Rapport mensuel généré avec succès",
                   icon("check-circle"),
                   status = "success"
                 ),
                 notificationItem(
                   text = "Synchronisation échouée",
                   icon("sync-alt"),
                   status = "danger"
                 ),
                 notificationItem(
                   text = "Validation des données terminées",
                   icon("clipboard-check"),
                   status = "success"
                 )
    ),
    
    # Zone B: Calendrier (Sélection mois/année)
    tags$li(class = "dropdown",
            tags$a(href = "#", class = "dropdown-toggle", `data-toggle` = "dropdown",
                   icon("calendar-alt"),
                   tags$span("Période")
            ),
            tags$ul(class = "dropdown-menu", style = "min-width: 300px;",
                    tags$li(
                      tags$div(style = "padding: 15px;",
                               tags$h5("Sélection de période", style = "margin-top: 0; color: #22c55e; font-weight: bold;"),
                               fluidRow(
                                 column(6,
                                        selectInput("mois_debut",
                                                    label = "Mois de début:",
                                                    choices = list(
                                                      "Janvier" = 1, "Février" = 2, "Mars" = 3, "Avril" = 4,
                                                      "Mai" = 5, "Juin" = 6, "Juillet" = 7, "Août" = 8,
                                                      "Septembre" = 9, "Octobre" = 10, "Novembre" = 11, "Décembre" = 12
                                                    ),
                                                    selected = format(Sys.Date(), "%m")
                                        )
                                 ),
                                 column(6,
                                        selectInput("annee_debut",
                                                    label = "Année:",
                                                    choices = 2020:2030,
                                                    selected = format(Sys.Date(), "%Y")
                                        )
                                 )
                               ),
                               fluidRow(
                                 column(6,
                                        selectInput("mois_fin",
                                                    label = "Mois de fin (optionnel):",
                                                    choices = c("Aucun" = "", list(
                                                      "Janvier" = 1, "Février" = 2, "Mars" = 3, "Avril" = 4,
                                                      "Mai" = 5, "Juin" = 6, "Juillet" = 7, "Août" = 8,
                                                      "Septembre" = 9, "Octobre" = 10, "Novembre" = 11, "Décembre" = 12
                                                    )),
                                                    selected = ""
                                        )
                                 ),
                                 column(6,
                                        selectInput("annee_fin",
                                                    label = "Année fin:",
                                                    choices = 2020:2030,
                                                    selected = format(Sys.Date(), "%Y")
                                        )
                                 )
                               ),
                               tags$div(style = "margin-top: 10px; padding: 8px; background-color: rgba(34, 197, 94, 0.1); border-radius: 4px;",
                                        tags$small(
                                          tags$strong("Période sélectionnée: "),
                                          textOutput("periode_selectionnee", inline = TRUE)
                                        )
                               )
                      )
                    )
            )
    ),
    
    # Zone C: Filtre Syndicat
    tags$li(class = "dropdown",
            tags$a(href = "#", class = "dropdown-toggle", `data-toggle` = "dropdown",
                   icon("filter"),
                   "Syndicat"
            ),
            tags$ul(class = "dropdown-menu",
                    tags$li(
                      tags$div(style = "padding: 10px; min-width: 200px;",
                               selectInput("syndicat_filter",
                                           label = "Choisir le syndicat:",
                                           choices = list(
                                             "Tous les syndicats" = "all",
                                             "Syndicat A" = "syndicat_a",
                                             "Syndicat B" = "syndicat_b",
                                             "Syndicat C" = "syndicat_c",
                                             "Syndicat D" = "syndicat_d"
                                           ),
                                           selected = "all"
                               )
                      )
                    )
            )
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Infos Générales", tabName = "infos_generales", icon = icon("info-circle"),
               menuSubItem("Vue Carte", tabName = "infos_gen_carte", icon = icon("map")),
               menuSubItem("Statistiques", tabName = "infos_gen_stats", icon = icon("chart-bar"))),
      menuItem("Infos Produits", tabName = "infos_produits", icon = icon("pills"),
               menuSubItem("Vue Carte", tabName = "produits_carte", icon = icon("map")),
               menuSubItem("Statistiques", tabName = "produits_stats", icon = icon("chart-bar"))),
      menuItem("Infos Famille", tabName = "infos_famille", icon = icon("sitemap"),
               menuSubItem("Vue Carte", tabName = "famille_carte", icon = icon("map")),
               menuSubItem("Statistiques", tabName = "famille_stats", icon = icon("chart-bar"))),
      menuItem("Infos TVA", tabName = "infos_tva", icon = icon("percent"),
               menuSubItem("Vue Carte", tabName = "tva_carte", icon = icon("map")),
               menuSubItem("Statistiques", tabName = "tva_stats", icon = icon("chart-bar"))),
      menuItem("Infos Caractère", tabName = "infos_caractere", icon = icon("map-marker"),
               menuSubItem("Vue Carte", tabName = "caractere_carte", icon = icon("map")),
               menuSubItem("Statistiques", tabName = "caractere_stats", icon = icon("chart-bar"))),
      menuItem("Segmentation", tabName = "segmentation", icon = icon("chart-pie"),
               menuSubItem("Vue Carte", tabName = "segment_carte", icon = icon("map")),
               menuSubItem("Statistiques", tabName = "segment_stats", icon = icon("chart-bar"))),
      div(
        style = "position: absolute; bottom: 10px; left: 10px; right: 10px; padding: 10px; text-align: center;",
        uiOutput("theme_switch")
      )
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        /* DARK MODE DEFAULT */
        .content-wrapper, .right-side, .content {
          background-color: #1a1a1a !important;
          color: white !important;
        }

        .main-header, .main-header .navbar, .main-header .logo {
          background-color: #1a1a1a !important;
          color: #22c55e !important;
          border-bottom: 1px solid #333 !important;
        }

        .skin-blue .main-sidebar {
          background-color: #000000 !important;
        }
        
        .form-group label {
          font-size: 14px !important;
          margin-bottom: 0px !important;
        }

        .skin-blue .sidebar-menu > li > a,
        .skin-blue .sidebar-menu > li > a > i {
          color: white !important;
          font-weight: bold !important;
        }

        .skin-blue .sidebar-menu > li.active > a,
        .skin-blue .sidebar-menu > li.active > a > i {
          background-color: #22c55e !important;
          color: black !important;
          font-weight: bold !important;
        }

        .logo-text {
          color: #22c55e !important;
          font-size: 24px !important;
          font-weight: bold !important;
        }

        .box {
          background-color: #2d2d2d !important;
          border: 1px solid #22c55e !important;
          color: white !important;
          border-radius: 8px;
        }

        .box-header {
          background-color: #2d2d2d !important;
          color: #22c55e !important;
        }

        .box-title {
          color: #22c55e !important;
          font-weight: bold !important;
        }

        /* Styles pour les nouveaux éléments du header */
        .main-header .navbar .nav > li > a {
          color: #22c55e !important;
        }

        .main-header .navbar .nav > li > a:hover {
          background-color: rgba(34, 197, 94, 0.1) !important;
        }

        .dropdown-menu {
          background-color: #2d2d2d !important;
          border: 1px solid #22c55e !important;
        }

        .dropdown-menu > li > a {
          color: white !important;
        }

        .dropdown-menu > li > a:hover {
          background-color: #22c55e !important;
          color: black !important;
        }

        /* Badge notifications avec couleurs */
        .label-danger {
          background-color: #dc3545 !important;
        }
        
        .label-warning {
          background-color: #f39c12 !important;
        }
        
        .label-success {
          background-color: #28a745 !important;
        }

        /* Notifications items colors */
        .dropdown-menu .notification-item.danger {
          border-left: 4px solid #dc3545;
        }
        
        .dropdown-menu .notification-item.warning {
          border-left: 4px solid #f39c12;
        }
        
        .dropdown-menu .notification-item.success {
          border-left: 4px solid #28a745;
        }

        /* Inputs dans les dropdowns */
        .dropdown-menu .form-control {
          background-color: #1a1a1a !important;
          border: 1px solid #22c55e !important;
          color: white !important;
        }

        .dropdown-menu .form-control:focus {
          border-color: #22c55e !important;
          box-shadow: 0 0 0 0.2rem rgba(34, 197, 94, 0.25) !important;
        }

        /* Select inputs */
        .dropdown-menu select.form-control {
          background-color: #1a1a1a !important;
          color: white !important;
        }

        .dropdown-menu select.form-control option {
          background-color: #1a1a1a !important;
          color: white !important;
        }

        /* Boutons de période */
        .btn-period {
          margin: 3px;
          font-size: 12px;
          padding: 6px 12px;
          background-color: #2d2d2d !important;
          border: 1px solid #22c55e !important;
          color: white !important;
          border-radius: 4px;
        }
        
        .btn-period:hover {
          background-color: #22c55e !important;
          color: black !important;
        }
        
        .btn-period.active {
          background-color: #22c55e !important;
          color: black !important;
          border-color: #22c55e !important;
          font-weight: bold !important;
        }

        /* Styles pour les DataTables */
        .dataTables_wrapper {
          color: white !important;
        }

        .dataTables_wrapper .dataTables_filter input {
          background-color: #1a1a1a !important;
          border: 1px solid #22c55e !important;
          color: white !important;
        }

        .dataTables_wrapper .dataTables_length select {
          background-color: #1a1a1a !important;
          border: 1px solid #22c55e !important;
          color: white !important;
        }

        .dataTables_wrapper .dataTables_info {
          color: white !important;
        }

        .dataTables_wrapper .dataTables_paginate .paginate_button {
          background-color: #2d2d2d !important;
          border: 1px solid #22c55e !important;
          color: white !important;
        }

        .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
          background-color: #22c55e !important;
          color: black !important;
        }

        .dataTables_wrapper .dataTables_paginate .paginate_button.current {
          background-color: #22c55e !important;
          color: black !important;
        }

        table.dataTable thead th {
          background-color: #2d2d2d !important;
          color: #22c55e !important;
          border-bottom: 1px solid #22c55e !important;
        }

        table.dataTable tbody td {
          background-color: #1a1a1a !important;
          color: white !important;
          border-bottom: 1px solid #333 !important;
        }

        table.dataTable tbody tr:hover td {
          background-color: rgba(34, 197, 94, 0.1) !important;
        }
        /* MODE CLAIR */
        .light-mode .content-wrapper, 
        .light-mode .right-side, 
        .light-mode .content {
          background-color: #f4f4f4 !important;
          color: black !important;
        }
        
        .light-mode .main-header, 
        .light-mode .main-header .navbar, 
        .light-mode .main-header .logo {
          background-color: white !important;
          color: #22c55e !important;
          border-bottom: 1px solid #ddd !important;
        }
        
        .light-mode .skin-blue .main-sidebar {
          background-color: #f8f9fa !important;
        }
        
        .light-mode .box {
          background-color: white !important;
          border: 1px solid #22c55e !important;
          color: black !important;
        }
      ")), 
      tags$script(HTML("
        $(document).on('click', '.dropdown-menu', function (e) {
          e.stopPropagation();
        });
      "))
    ),
    
    # Affichage des informations sélectionnées (exclu pour produits_stats)
    conditionalPanel(
      condition = "input.sidebar != 'produits_stats'",
      fluidRow(
        box(
          title = "Informations de Session", 
          status = "info", 
          solidHeader = TRUE, 
          width = 12,
          collapsible = TRUE,
          collapsed = TRUE,
          fluidRow(
            column(4, 
                   h4("Période sélectionnée:"),
                   verbatimTextOutput("periode_display")
            ),
            column(4,
                   h4("Syndicat sélectionné:"),
                   verbatimTextOutput("selected_syndicat_display")
            ),
            column(4,
                   h4("Date actuelle:"),
                   verbatimTextOutput("current_date_display")
            )
          )
        )
      )
    ),
    
    tabItems(
      tabItem(tabName = "infos_gen_carte",
              fluidRow(
                box(title = "Cartographie - Infos Générales", status = "primary", solidHeader = TRUE, width = 12,
                    div(style = "text-align:center;padding-top:200px;color:#888;",
                        h3("Zone carte infos générales")))
              )),
      tabItem(tabName = "infos_gen_stats",
              fluidRow(
                box(title = "Statistiques - Infos Générales", status = "primary", solidHeader = TRUE, width = 12,
                    div(style = "text-align:center;padding-top:100px;color:#888;",
                        p("Statistiques générales ici")))
              )),
      
      tabItem(tabName = "produits_carte",
              fluidRow(
                box(title = "Cartographie - Produits", status = "success", solidHeader = TRUE, width = 12,
                    div(style = "text-align:center;padding-top:200px;color:#888;",
                        h3("Zone carte produits")))
              )),
      
      # Section Statistiques Produits mise à jour
      tabItem(tabName = "produits_stats",
              # Boutons de sélection de période
              fluidRow(
                box(title = "Sélection de période", status = "success", solidHeader = TRUE, width = 12,
                    div(style = "text-align: center; padding: 10px;",
                        actionButton("btn_aujourd_hui", "Aujourd'hui", class = "btn btn-default btn-period"),
                        actionButton("btn_hier", "Hier", class = "btn btn-default btn-period"),
                        actionButton("btn_semaine", "Semaine", class = "btn btn-default btn-period"),
                        actionButton("btn_semaine_n1", "Semaine N-1", class = "btn btn-default btn-period"),
                        actionButton("btn_mois", "Mois (Calendrier)", class = "btn btn-default btn-period active")
                    ),
                    div(style = "text-align: center; margin-top: 10px; padding: 8px; background-color: rgba(34, 197, 94, 0.1); border-radius: 4px;",
                        tags$small(
                          tags$strong("Période active: "),
                          tags$span(style = "color: #22c55e; font-weight: bold;", textOutput("periode_active", inline = TRUE))
                        )
                    )
                )
              ),
              
              # Première ligne: Camembert
              fluidRow(
                box(title = "Répartition Actifs/Inactifs", status = "success", solidHeader = TRUE, width = 12,
                    plotlyOutput("pie_chart_actifs_inactifs", height = "400px")
                )
              ),
              
              # Deuxième ligne: Graphique temporel
              fluidRow(
                box(title = "Évolution Actifs/Inactifs sur 12 mois", status = "success", solidHeader = TRUE, width = 12,
                    div(style = "margin-bottom: 10px;",
                        tags$small("Ce graphique montre l'évolution des pharmacies actives et inactives depuis les premiers adhérents OSPHARM datastat", 
                                   style = "color: #888; font-style: italic;")
                    ),
                    dygraphOutput("time_series_12_months", height = "400px")
                )
              ),
              
              # Troisième ligne: Tableau récapitulatif
              fluidRow(
                box(title = "Tableau récapitulatif", status = "success", solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(4,
                             selectInput("filter_status", "Filtrer par statut:",
                                         choices = list("Tous" = "all", "Actifs" = "actifs", "Inactifs" = "inactifs"),
                                         selected = "all")
                      ),
                      column(8,
                             div(style = "padding-top: 25px;",
                                 tags$small("Tableau des pharmacies selon la période et le statut sélectionnés", 
                                            style = "color: #888; font-style: italic;")
                             )
                      )
                    ),
                    DT::dataTableOutput("table_recap")
                )
              )
      ),
      
      tabItem(tabName = "famille_carte",
              fluidRow(
                box(title = "Cartographie - Familles", status = "warning", solidHeader = TRUE, width = 12,
                    div(style = "text-align:center;padding-top:200px;color:#888;",
                        h3("Zone carte familles")))
              )),
      tabItem(tabName = "famille_stats",
              fluidRow(
                box(title = "Statistiques - Familles", status = "warning", solidHeader = TRUE, width = 12,
                    div(style = "text-align:center;padding-top:100px;color:#888;",
                        p("Statistiques familles ici")))
              )),
      
      tabItem(tabName = "tva_carte",
              fluidRow(
                box(title = "Cartographie - TVA", status = "info", solidHeader = TRUE, width = 12,
                    div(style = "text-align:center;padding-top:200px;color:#888;",
                        h3("Zone carte TVA")))
              )),
      tabItem(tabName = "tva_stats",
              fluidRow(
                box(title = "Statistiques - TVA", status = "info", solidHeader = TRUE, width = 12,
                    div(style = "text-align:center;padding-top:100px;color:#888;",
                        p("Statistiques TVA ici")))
              )),
      
      tabItem(tabName = "caractere_carte",
              fluidRow(
                box(title = "Cartographie - Caractère", status = "danger", solidHeader = TRUE, width = 12,
                    div(style = "text-align:center;padding-top:200px;color:#888;",
                        h3("Zone carte caractère")))
              )),
      tabItem(tabName = "caractere_stats",
              fluidRow(
                box(title = "Statistiques - Caractère", status = "danger", solidHeader = TRUE, width = 12,
                    div(style = "text-align:center;padding-top:100px;color:#888;",
                        p("Statistiques caractère ici")))
              )),
      
      tabItem(tabName = "segment_carte",
              fluidRow(
                box(title = "Cartographie - Segmentation", status = "primary", solidHeader = TRUE, width = 12,
                    div(style = "text-align:center;padding-top:200px;color:#888;",
                        h3("Zone carte segmentation")))
              )),
      tabItem(tabName = "segment_stats",
              fluidRow(
                box(title = "Statistiques - Segmentation", status = "primary", solidHeader = TRUE, width = 12,
                    div(style = "text-align:center;padding-top:100px;color:#888;",
                        p("Statistiques segmentation ici")))
              ))
    )
  )
)

# Opérateur d'alternative nul (pour switch)
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Serveur
dashboard_server <- function(input, output, session) {
  
  # Variable réactive pour stocker la période active
  periode_active <- reactiveVal("mois")
  
  # Gestion des boutons de période
  observeEvent(input$btn_aujourd_hui, {
    periode_active("aujourd_hui")
    runjs("$('.btn-period').removeClass('active'); $('#btn_aujourd_hui').addClass('active');")
  })
  
  observeEvent(input$btn_hier, {
    periode_active("hier")
    runjs("$('.btn-period').removeClass('active'); $('#btn_hier').addClass('active');")
  })
  
  observeEvent(input$btn_semaine, {
    periode_active("semaine")
    runjs("$('.btn-period').removeClass('active'); $('#btn_semaine').addClass('active');")
  })
  
  observeEvent(input$btn_semaine_n1, {
    periode_active("semaine_n1")
    runjs("$('.btn-period').removeClass('active'); $('#btn_semaine_n1').addClass('active');")
  })
  
  observeEvent(input$btn_mois, {
    periode_active("mois")
    runjs("$('.btn-period').removeClass('active'); $('#btn_mois').addClass('active');")
  })
  
  # Fonction pour calculer les dates selon la période
  get_period_dates <- reactive({
    periode <- periode_active()
    today <- Sys.Date()
    
    switch(periode,
           "aujourd_hui" = {
             list(debut = today, fin = today)
           },
           "hier" = {
             hier <- today - 1
             list(debut = hier, fin = hier)
           },
           "semaine" = {
             debut_semaine <- today - as.numeric(format(today, "%u")) + 1
             fin_semaine <- debut_semaine + 6
             list(debut = debut_semaine, fin = fin_semaine)
           },
           "semaine_n1" = {
             debut_semaine_n1 <- today - as.numeric(format(today, "%u")) + 1 - 7
             fin_semaine_n1 <- debut_semaine_n1 + 6
             list(debut = debut_semaine_n1, fin = fin_semaine_n1)
           },
           "mois" = {
             if (!is.null(input$mois_debut) && !is.null(input$annee_debut)) {
               debut_periode <- as.Date(paste(input$annee_debut, input$mois_debut, "01", sep = "-"))
               
               if (!is.null(input$mois_fin) && input$mois_fin != "") {
                 fin_periode <- as.Date(paste(input$annee_fin, input$mois_fin, 
                                              format(as.Date(paste(input$annee_fin, input$mois_fin, "01", sep = "-")) + months(1) - 1, "%d"), 
                                              sep = "-"))
               } else {
                 fin_periode <- as.Date(paste(input$annee_debut, input$mois_debut, 
                                              format(debut_periode + months(1) - 1, "%d"), 
                                              sep = "-"))
               }
               list(debut = debut_periode, fin = fin_periode)
             } else {
               list(debut = as.Date(paste(format(today, "%Y"), format(today, "%m"), "01", sep = "-")),
                    fin = today)
             }
           }
    )
  })
  
  # Fonction pour filtrer les données selon la période
  # Fonction pour filtrer les données selon la période - VERSION CORRIGÉE
  get_filtered_data <- reactive({
    dates <- get_period_dates()
    
    # DEBUG: Vérifier les données
    cat("DEBUG - Période:", periode_active(), "\n")
    cat("DEBUG - Date début:", as.character(dates$debut), "\n")
    cat("DEBUG - Date fin:", as.character(dates$fin), "\n")
    
    # Filtrer les données selon la période sélectionnée
    if ("date_complete" %in% names(table_actifs)) {
      # Convertir les dates si nécessaire
      table_actifs$date_complete <- as.Date(table_actifs$date_complete)
      actifs_filtered <- table_actifs[
        table_actifs$date_complete >= dates$debut & 
          table_actifs$date_complete <= dates$fin, 
      ]
    } else {
      # Si pas de colonne date, utiliser toutes les données (cas de démonstration)
      actifs_filtered <- table_actifs
    }
    
    if ("date_complete" %in% names(table_inactifs)) {
      # Convertir les dates si nécessaire
      table_inactifs$date_complete <- as.Date(table_inactifs$date_complete)
      inactifs_filtered <- table_inactifs[
        table_inactifs$date_complete >= dates$debut & 
          table_inactifs$date_complete <= dates$fin, 
      ]
    } else {
      # Si pas de colonne date, utiliser toutes les données (cas de démonstration)
      inactifs_filtered <- table_inactifs
    }
    
    cat("DEBUG - Actifs filtrés:", nrow(actifs_filtered), "\n")
    cat("DEBUG - Inactifs filtrés:", nrow(inactifs_filtered), "\n")
    
    list(actifs = actifs_filtered, inactifs = inactifs_filtered)
  })
  
  # Remplacez tout le bloc output$table_recap par ce code corrigé :
  
  output$table_recap <- DT::renderDataTable({
    data_filtered <- get_filtered_data()
    
    # Créer les données selon le filtre sélectionné
    if (input$filter_status == "actifs") {
      if (nrow(data_filtered$actifs) > 0) {
        table_data <- data_filtered$actifs
        table_data$Status <- "Actif"
      } else {
        table_data <- data.frame(
          n_auto_adhpa = character(0),
          Status = character(0),
          stringsAsFactors = FALSE
        )
      }
    } else if (input$filter_status == "inactifs") {
      if (nrow(data_filtered$inactifs) > 0) {
        table_data <- data_filtered$inactifs
        table_data$Status <- "Inactif"
      } else {
        table_data <- data.frame(
          n_auto_adhpa = character(0),
          Status = character(0),
          stringsAsFactors = FALSE
        )
      }
    } else {
      # "all" - combiner les deux tables
      actifs_data <- data_filtered$actifs
      inactifs_data <- data_filtered$inactifs
      
      if (nrow(actifs_data) > 0) {
        actifs_data$Status <- "Actif"
      }
      if (nrow(inactifs_data) > 0) {
        inactifs_data$Status <- "Inactif"
      }
      
      # Combiner les données
      if (nrow(actifs_data) > 0 && nrow(inactifs_data) > 0) {
        # Trouver les colonnes communes
        common_cols <- intersect(names(actifs_data), names(inactifs_data))
        table_data <- rbind(
          actifs_data[, common_cols, drop = FALSE],
          inactifs_data[, common_cols, drop = FALSE]
        )
      } else if (nrow(actifs_data) > 0) {
        table_data <- actifs_data
      } else if (nrow(inactifs_data) > 0) {
        table_data <- inactifs_data
      } else {
        table_data <- data.frame(
          n_auto_adhpa = character(0),
          Status = character(0),
          stringsAsFactors = FALSE
        )
      }
    }
    
    # S'assurer que n_auto_adhpa existe
    if (nrow(table_data) > 0 && !"n_auto_adhpa" %in% names(table_data)) {
      # Créer des IDs automatiques si la colonne n'existe pas
      table_data$n_auto_adhpa <- paste0("AUTO_", sprintf("%04d", 1:nrow(table_data)))
    }
    
    # Sélectionner seulement les colonnes d'intérêt pour l'affichage
    if (nrow(table_data) > 0) {
      # Garder seulement n_auto_adhpa et Status, plus toutes les autres colonnes importantes
      essential_cols <- c("n_auto_adhpa", "Status")
      other_cols <- setdiff(names(table_data), essential_cols)
      
      # Limiter le nombre de colonnes pour la lisibilité (garder les 5 premières autres colonnes)
      if (length(other_cols) > 5) {
        other_cols <- other_cols[1:5]
      }
      
      cols_to_show <- c(essential_cols, other_cols)
      cols_to_show <- cols_to_show[cols_to_show %in% names(table_data)]
      table_data <- table_data[, cols_to_show, drop = FALSE]
    }
    
    # Créer le DataTable
    DT::datatable(table_data,
                  options = list(
                    pageLength = 10,
                    searching = TRUE,
                    ordering = TRUE,
                    info = TRUE,
                    lengthChange = TRUE,
                    scrollX = TRUE,
                    language = list(
                      search = "Rechercher:",
                      lengthMenu = "Afficher _MENU_ entrées",
                      info = "Affichage de _START_ à _END_ sur _TOTAL_ entrées",
                      infoEmpty = "Aucune entrée disponible",
                      infoFiltered = "(filtré à partir de _MAX_ entrées au total)",
                      paginate = list(
                        first = "Premier",
                        last = "Dernier",
                        "next" = "Suivant",
                        "previous" = "Précédent"
                      )
                    )
                  ),
                  class = "display nowrap compact",
                  rownames = FALSE) %>%
      # Appliquer le style seulement si la colonne Status existe
      {
        if ("Status" %in% names(table_data)) {
          DT::formatStyle(., 
                          columns = "Status",
                          backgroundColor = DT::styleEqual(c("Actif", "Inactif"), 
                                                           c("#22c55e", "#dc3545")),
                          color = "white",
                          fontWeight = "bold")
        } else {
          .
        }
      }
  })
  # Affichage de la période active
  output$periode_active <- renderText({
    dates <- get_period_dates()
    periode <- periode_active()
    
    switch(periode,
           "aujourd_hui" = format(dates$debut, "%d/%m/%Y"),
           "hier" = format(dates$debut, "%d/%m/%Y"),
           "semaine" = paste0("Du ", format(dates$debut, "%d/%m/%Y"), " au ", format(dates$fin, "%d/%m/%Y")),
           "semaine_n1" = paste0("Du ", format(dates$debut, "%d/%m/%Y"), " au ", format(dates$fin, "%d/%m/%Y")),
           "mois" = {
             if (!is.null(input$mois_debut) && !is.null(input$annee_debut)) {
               mois_noms <- c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin",
                              "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre")
               mois_debut_nom <- mois_noms[as.numeric(input$mois_debut)]
               
               if (!is.null(input$mois_fin) && input$mois_fin != "") {
                 mois_fin_nom <- mois_noms[as.numeric(input$mois_fin)]
                 paste0(mois_debut_nom, " ", input$annee_debut, " - ", mois_fin_nom, " ", input$annee_fin)
               } else {
                 paste0(mois_debut_nom, " ", input$annee_debut)
               }
             } else {
               format(dates$debut, "%B %Y")
             }
           }
    )
  })
  
  # Camembert Actifs/Inactifs
  # Camembert Actifs/Inactifs - VERSION CORRIGÉE
  output$pie_chart_actifs_inactifs <- renderPlotly({
    data_filtered <- get_filtered_data()
    
    # Appliquer le filtre de statut
    if (input$filter_status == "actifs") {
      nb_actifs <- nrow(data_filtered$actifs)
      nb_inactifs <- 0
    } else if (input$filter_status == "inactifs") {
      nb_actifs <- 0
      nb_inactifs <- nrow(data_filtered$inactifs)
    } else {
      # "all" - montrer les deux
      nb_actifs <- nrow(data_filtered$actifs)
      nb_inactifs <- nrow(data_filtered$inactifs)
    }
    
    total <- nb_actifs + nb_inactifs
    
    if (total == 0) {
      # Données par défaut si aucune donnée
      pie_data <- data.frame(
        Status = c("Actifs", "Inactifs"),
        Count = c(0, 0),
        Percentage = c(0, 0)
      )
    } else {
      # Ne montrer que les catégories avec des données > 0
      if (input$filter_status == "actifs") {
        pie_data <- data.frame(
          Status = "Actifs",
          Count = nb_actifs,
          Percentage = 100
        )
        colors <- c("#22c55e")
      } else if (input$filter_status == "inactifs") {
        pie_data <- data.frame(
          Status = "Inactifs",
          Count = nb_inactifs,
          Percentage = 100
        )
        colors <- c("#dc3545")
      } else {
        pie_data <- data.frame(
          Status = c("Actifs", "Inactifs"),
          Count = c(nb_actifs, nb_inactifs),
          Percentage = round(c(nb_actifs/total*100, nb_inactifs/total*100), 1)
        )
        colors <- c("#22c55e", "#dc3545")
      }
    }
    
    p <- plot_ly(pie_data, 
                 labels = ~Status, 
                 values = ~Count,
                 type = 'pie',
                 textposition = 'inside',
                 textinfo = 'label+percent+value',
                 hovertemplate = paste('<b>%{label}</b><br>',
                                       'Nombre: %{value}<br>',
                                       'Pourcentage: %{percent}<br>',
                                       '<extra></extra>'),
                 marker = list(colors = colors,
                               line = list(color = '#FFFFFF', width = 2))) %>%
      layout(
        title = list(
          text = paste0("Total: ", total),
          font = list(color = "white")
        ),
        font = list(color = "white"),
        paper_bgcolor = '#1a1a1a',
        plot_bgcolor = '#1a1a1a',
        hoverlabel = list(
          bgcolor = "white",
          font = list(color = "black")
        ),
        legend = list(
          font = list(color = "white"),
          bgcolor = '#1a1a1a',
          bordercolor = "#22c55e",
          borderwidth = 1
        )
      )
    return(p)
  })
  # Graphique temporel sur 12 mois (dygraphs)
  output$time_series_12_months <- renderDygraph({
    # Crée une séquence des 12 derniers mois
    end_date <- as.Date(paste(format(Sys.Date(), "%Y-%m"), "01", sep = "-"))
    start_date <- end_date %m-% months(11)
    months_seq <- seq(start_date, end_date, by = "month")
    
    # Ajoute une colonne mois-année dans les tables si elle n'existe pas
    table_actifs$month <- format(as.Date(table_actifs$date_complete), "%Y-%m")
    table_inactifs$month <- format(as.Date(table_inactifs$date_complete), "%Y-%m")
    
    # Compter les entrées par mois
    monthly_actifs <- table_actifs %>%
      group_by(month) %>%
      summarise(nb = n()) %>%
      complete(month = format(months_seq, "%Y-%m"), fill = list(nb = 0)) %>%
      arrange(month)
    
    monthly_inactifs <- table_inactifs %>%
      group_by(month) %>%
      summarise(nb = n()) %>%
      complete(month = format(months_seq, "%Y-%m"), fill = list(nb = 0)) %>%
      arrange(month)
    
    # Fusion des deux tables
    df <- full_join(monthly_actifs, monthly_inactifs, by = "month", suffix = c("_actifs", "_inactifs")) %>%
      replace_na(list(nb_actifs = 0, nb_inactifs = 0))
    
    df_xts <- xts(df[, c("nb_actifs", "nb_inactifs")], order.by = as.Date(paste0(df$month, "-01")))
    dygraph(df_xts, main = "") %>%
      dySeries("nb_actifs", label = "Actifs", color = "#22c55e", strokeWidth = 2) %>%
      dySeries("nb_inactifs", label = "Inactifs", color = "#dc3545", strokeWidth = 2) %>%
      dyAxis("x", axisLabelColor = "white", axisLineColor = "#666666") %>%
      dyAxis("y", label = "Nombre", axisLabelColor = "white", axisLineColor = "#666666") %>%
      dyLegend(show = "always", labelsSeparateLines = TRUE, labelsDiv = NULL) %>%
      dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.3, hideOnMouseOut = FALSE) %>%
      dyRangeSelector(height = 40, strokeColor = "#22c55e", fillColor = "#22c55e") %>%
      dyOptions(colors = c("#22c55e", "#dc3545"), 
                axisLabelFontSize = 12,
                axisLineColor = "#666666",
                gridLineColor = "#333333") %>%
      dyCSS(textConnection("
    .dygraph-legend {
      background: white !important;
      color: black !important;
      border: 1px solid #22c55e !important;
      border-radius: 4px !important;
      padding: 4px !important;
    }
    .dygraph-legend > span {
      color: black !important;
    }
  "))
  })
  
  # Tableau récapitulatif
  output$table_recap <- DT::renderDataTable({
    data_filtered <- get_filtered_data()
    
    # Combiner les données selon le filtre sélectionné
    if (input$filter_status == "actifs") {
      table_data <- data_filtered$actifs
      if (nrow(table_data) > 0) {
        table_data$Status <- "Actif"
      }
    } else if (input$filter_status == "inactifs") {
      table_data <- data_filtered$inactifs
      if (nrow(table_data) > 0) {
        table_data$Status <- "Inactif"
      }
    } else {
      # Tous - combiner les deux tables
      actifs_with_status <- data_filtered$actifs
      inactifs_with_status <- data_filtered$inactifs
      
      if (nrow(actifs_with_status) > 0) {
        actifs_with_status$Status <- "Actif"
      }
      if (nrow(inactifs_with_status) > 0) {
        inactifs_with_status$Status <- "Inactif"
      }
      
      # Identifier les colonnes communes
      if (nrow(actifs_with_status) > 0 && nrow(inactifs_with_status) > 0) {
        common_cols <- intersect(names(actifs_with_status), names(inactifs_with_status))
        table_data <- rbind(
          actifs_with_status[, common_cols, drop = FALSE],
          inactifs_with_status[, common_cols, drop = FALSE]
        )
      } else if (nrow(actifs_with_status) > 0) {
        table_data <- actifs_with_status
      } else if (nrow(inactifs_with_status) > 0) {
        table_data <- inactifs_with_status
      } else {
        table_data <- data.frame()
      }
    }
    
    if (nrow(table_data) == 0) {
      # Table vide avec colonnes par défaut
      table_data <- data.frame(
        n_auto_adhpa = character(0),
        Status = character(0),
        stringsAsFactors = FALSE
      )
    } else {
      # S'assurer que n_auto_adhpa est présent
      if (!"n_auto_adhpa" %in% names(table_data)) {
        if ("n_auto_adhpa" %in% names(table_actifs)) {
          # Utiliser les n_auto_adhpa des tables originales
          table_data$n_auto_adhpa <- paste0("AUTO_", sample(1000:9999, nrow(table_data)))
        } else {
          table_data$n_auto_adhpa <- paste0("AUTO_", sample(1000:9999, nrow(table_data)))
        }
      }
      
      # Sélectionner seulement les colonnes nécessaires
      cols_to_show <- c("n_auto_adhpa", "Status")
      if (all(cols_to_show %in% names(table_data))) {
        table_data <- table_data[, cols_to_show, drop = FALSE]
      }
    }
    
    DT::datatable(table_data,
                  options = list(
                    pageLength = 10,
                    searching = TRUE,
                    ordering = TRUE,
                    info = TRUE,
                    lengthChange = TRUE,
                    dom = 'Bfrtip',
                    scrollX = TRUE,
                    language = list(
                      search = "Rechercher:",
                      lengthMenu = "Afficher _MENU_ entrées",
                      info = "Affichage de _START_ à _END_ sur _TOTAL_ entrées",
                      infoEmpty = "Aucune entrée disponible",
                      infoFiltered = "(filtré à partir de _MAX_ entrées au total)",
                      paginate = list(
                        first = "Premier",
                        last = "Dernier",
                        "next" = "Suivant",
                        "previous" = "Précédent"
                      )
                    )
                  ),
                  class = "display nowrap compact",
                  rownames = FALSE,
                  colnames = c("N° Auto ADHPA" = "n_auto_adhpa", "Statut" = "Status")) %>%
      DT::formatStyle(columns = "Status",
                      backgroundColor = DT::styleEqual(c("Actif", "Inactif"), 
                                                       c("#22c55e", "#dc3545")),
                      color = "white",
                      fontWeight = "bold")
  })
  
  # Affichages pour les informations de session (hors produits_stats)
  output$periode_display <- renderText({
    dates <- get_period_dates()
    paste0("Du ", format(dates$debut, "%d/%m/%Y"), " au ", format(dates$fin, "%d/%m/%Y"))
  })
  
  output$selected_syndicat_display <- renderText({
    syndicat_names <- list(
      "all" = "Tous les syndicats",
      "syndicat_a" = "Syndicat A",
      "syndicat_b" = "Syndicat B", 
      "syndicat_c" = "Syndicat C",
      "syndicat_d" = "Syndicat D"
    )
    
    syndicat_names[[input$syndicat_filter %||% "all"]]
  })
  
  output$current_date_display <- renderText({
    format(Sys.Date(), "%d/%m/%Y")
  })
  
  # Affichage de la période sélectionnée dans le calendrier
  output$periode_selectionnee <- renderText({
    if (!is.null(input$mois_debut) && !is.null(input$annee_debut)) {
      mois_noms <- c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin",
                     "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre")
      
      mois_debut_nom <- mois_noms[as.numeric(input$mois_debut)]
      
      if (!is.null(input$mois_fin) && input$mois_fin != "") {
        mois_fin_nom <- mois_noms[as.numeric(input$mois_fin)]
        paste0(mois_debut_nom, " ", input$annee_debut, " - ", mois_fin_nom, " ", input$annee_fin)
      } else {
        paste0(mois_debut_nom, " ", input$annee_debut)
      }
    } else {
      "Aucune période sélectionnée"
    }
  })
  
  # Gestion du thème (pour plus tard si nécessaire)
  # Variable réactive pour le thème
  theme_mode <- reactiveVal("dark")
  
  # Gestion du changement de thème
  observeEvent(input$toggle_theme, {
    current_theme <- theme_mode()
    new_theme <- if(current_theme == "dark") "light" else "dark"
    theme_mode(new_theme)
    
    if(new_theme == "light") {
      runjs("$('body').removeClass('dark-mode').addClass('light-mode');")
    } else {
      runjs("$('body').removeClass('light-mode').addClass('dark-mode');")
    }
  })
  
  output$theme_switch <- renderUI({
    current_theme <- theme_mode()
    button_text <- if(current_theme == "dark") "Mode Clair" else "Mode Sombre"
    button_class <- if(current_theme == "dark") "btn-warning" else "btn-secondary"
    
    actionButton("toggle_theme", 
                 button_text,
                 class = paste("btn btn-sm", button_class),
                 style = "width: 100%; margin-top: 10px;")
  })
}

# Lancer l'application
shinyApp(ui = ui, server = dashboard_server)