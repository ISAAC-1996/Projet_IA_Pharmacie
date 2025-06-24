# OSPHARM Application R Shiny - Version Complète avec notifications, calendrier et filtre syndicat
library(shiny)
library(shinydashboard)
library(shinyWidgets)

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
                   text = "Validation des données terminée",
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
      ")), 
      tags$script(HTML("
        $(document).on('click', '.dropdown-menu', function (e) {
          e.stopPropagation();
        });
      "))
    ),
    
    # Affichage des informations sélectionnées
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
      tabItem(tabName = "produits_stats",
              fluidRow(
                box(title = "Statistiques - Produits", status = "success", solidHeader = TRUE, width = 12,
                    div(style = "text-align:center;padding-top:100px;color:#888;",
                        p("Statistiques produits ici")))
              )),
      
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
  
  # Gestion du thème
  observeEvent(input$theme_toggle, {
    if (input$theme_toggle) {
      insertUI(
        selector = "head",
        where = "beforeEnd",
        ui = tags$style(id = "light-theme", HTML("
    /* HEADER CLAIR */
    .main-header, .main-header .navbar, .main-header .logo,
    .skin-blue .main-header .navbar,
    .skin-blue .main-header .logo {
      background-color: #f4f4f4 !important;
      color: #22c55e !important;
    }

    /* SIDEBAR CLAIR */
    .skin-blue .main-sidebar {
      background-color: #ffffff !important;
    }

    .skin-blue .sidebar-menu > li > a,
    .skin-blue .sidebar-menu > li > a > i {
      color: black !important;
      font-weight: bold !important;
    }

    .skin-blue .sidebar-menu > li.active > a,
    .skin-blue .sidebar-menu > li.active > a > i {
      background-color: #22c55e !important;
      color: black !important;
      font-weight: bold !important;
    }

    .skin-blue .sidebar-menu > li.menu-open > a,
    .skin-blue .sidebar-menu > li.menu-open > a > i {
      background-color: #e6ffe6 !important;
      color: #22c55e !important;
      font-weight: bold !important;
    }

    /* SOUS-MENUS OUVERTS */
    .skin-blue .sidebar-menu .treeview-menu {
      background-color: #ffffff !important;
    }

    .skin-blue .sidebar-menu .treeview-menu > li > a,
    .skin-blue .sidebar-menu .treeview-menu > li > a > i {
      color: black !important;
      font-weight: bold !important;
    }

    .skin-blue .sidebar-menu .treeview-menu > li.active > a,
    .skin-blue .sidebar-menu .treeview-menu > li.active > a > i {
      background-color: #22c55e !important;
      color: black !important;
      font-weight: bold !important;
    }

    /* ZONE DE CONTENU CLAIR */
    .content-wrapper, .right-side, .content {
      background-color: #ffffff !important;
      color: black !important;
    }

    .box {
      background-color: #ffffff !important;
      color: black !important;
      border: 1px solid #22c55e !important;
      border-radius: 8px;
    }

    .box-header {
      background-color: #ffffff !important;
      color: #22c55e !important;
      border-bottom: 1px solid #ddd !important;
    }

    .box-title {
      color: #22c55e !important;
      font-weight: bold !important;
    }

    /* LOGO & TITRES */
    .logo-text {
      color: #22c55e !important;
    }

    h1, h2, h3, h4, h5, h6, p {
      color: black !important;
    }

    /* Dropdown menus en mode clair */
    .dropdown-menu {
      background-color: #ffffff !important;
      border: 1px solid #ddd !important;
    }

    .dropdown-menu > li > a {
      color: black !important;
    }

    .dropdown-menu .form-control {
      background-color: #ffffff !important;
      border: 1px solid #ddd !important;
      color: black !important;
    }

    .dropdown-menu select.form-control {
      background-color: #ffffff !important;
      color: black !important;
    }

    .dropdown-menu select.form-control option {
      background-color: #ffffff !important;
      color: black !important;
    }
  "))
      )
      
    } else {
      removeUI(selector = "#light-theme")
    }
  })
  
  # Switch de thème
  output$theme_switch <- renderUI({
    shinyWidgets::materialSwitch(
      inputId = "theme_toggle",
      label = if (is.null(input$theme_toggle) || !input$theme_toggle) "Mode Clair" else "Mode Sombre",
      status = "success",
      value = input$theme_toggle %||% FALSE,
      right = TRUE,
      inline = TRUE
    )
  })
  
  # Affichage de la période sélectionnée dans le header
  output$periode_selectionnee <- renderText({
    req(input$mois_debut, input$annee_debut)
    
    mois_noms <- c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin",
                   "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre")
    
    mois_debut_nom <- mois_noms[as.numeric(input$mois_debut)]
    
    # Si fin non renseignée, afficher uniquement début
    if (is.null(input$mois_fin) || input$mois_fin == "") {
      return(paste0(mois_debut_nom, " ", input$annee_debut))
    }
    
    mois_fin_nom <- mois_noms[as.numeric(input$mois_fin)]
    
    # Convertir en "valeurs comparables"
    debut_val <- as.numeric(input$annee_debut) * 12 + as.numeric(input$mois_debut)
    fin_val   <- as.numeric(input$annee_fin) * 12 + as.numeric(input$mois_fin)
    
    # Vérification si date de fin est avant date de début
    if (fin_val < debut_val) {
      return("Erreur : La date de fin est antérieure à la date de début.")
    }
    
    paste0(mois_debut_nom, " ", input$annee_debut, " à ", mois_fin_nom, " ", input$annee_fin)
  })
  
  
  # Affichage de la période dans le contenu principal
  output$periode_display <- renderText({
    req(input$mois_debut, input$annee_debut)
    
    mois_noms <- c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin",
                   "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre")
    
    mois_debut_nom <- mois_noms[as.numeric(input$mois_debut)]
    
    if (!is.null(input$mois_fin) && input$mois_fin != "") {
      mois_fin_nom <- mois_noms[as.numeric(input$mois_fin)]
      paste0("Du ", mois_debut_nom, " ", input$annee_debut, " au ", mois_fin_nom, " ", input$annee_fin)
    } else {
      paste0(mois_debut_nom, " ", input$annee_debut, " (mois unique)")
    }
  })
  
  # Affichage du syndicat sélectionné
  output$selected_syndicat_display <- renderText({
    if (!is.null(input$syndicat_filter)) {
      switch(input$syndicat_filter,
             "all" = "Tous les syndicats",
             "syndicat_a" = "Syndicat A",
             "syndicat_b" = "Syndicat B", 
             "syndicat_c" = "Syndicat C",
             "syndicat_d" = "Syndicat D",
             "Non sélectionné"
      )
    } else {
      "Tous les syndicats"
    }
  })
  
  # Affichage de la date actuelle (dans le contenu)
  output$current_date_display <- renderText({
    format(Sys.time(), "%d/%m/%Y %H:%M:%S")
  })
}

# Lancement
shinyApp(ui = ui, server = dashboard_server)