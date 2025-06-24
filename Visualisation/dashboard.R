# OSPHARM Application R Shiny v2
# Interface avec animation de pharmacies et structure d'onglets avancée

library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(plotly)
library(dplyr)
library(shinycssloaders)
library(shinyWidgets)
library(htmltools)

# Données simulées de pharmacies
set.seed(123)
pharmacies_data <- data.frame(
  id = 1:500,
  nom = paste("Pharmacie", 1:500),
  lat = runif(500, 43.0, 51.0),
  lng = runif(500, -5.0, 8.0),
  chiffre_affaires = round(runif(500, 50000, 500000)),
  nb_produits = sample(100:2000, 500, replace = TRUE),
  famille_principale = sample(c("Médicaments", "Parapharmacie", "Homéopathie", "Cosmétiques"), 500, replace = TRUE),
  tva = sample(c("5.5%", "10%", "20%"), 500, replace = TRUE),
  caractere = sample(c("Urbaine", "Rurale", "Semi-urbaine"), 500, replace = TRUE),
  segment = sample(c("Premium", "Standard", "Économique"), 500, replace = TRUE),
  syndicat = sample(c("FSPF", "USPO", "Indépendant"), 500, replace = TRUE)
)

# Interface utilisateur
ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      tags$img(src = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg==", 
               height = "30px", style = "background-color: #00A86B; border-radius: 5px; padding: 2px;"),
      "OSPHARM", 
      style = "color: #00A86B; font-weight: bold; font-size: 18px;"
    ),
    tags$li(class = "dropdown",
            tags$a(href = "#", class = "dropdown-toggle", "data-toggle" = "dropdown",
                   tags$i(class = "fa fa-calendar"), "A")),
    tags$li(class = "dropdown",
            tags$a(href = "#", class = "dropdown-toggle", "data-toggle" = "dropdown",
                   tags$i(class = "fa fa-users"), "B")),
    tags$li(class = "dropdown",
            tags$a(href = "#", class = "dropdown-toggle", "data-toggle" = "dropdown",
                   "C")),
    tags$li(class = "dropdown",
            tags$a(href = "#", class = "dropdown-toggle", "data-toggle" = "dropdown",
                   tags$i(class = "fa fa-bell"), "Z"))
  ),
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("1 - Infos Générales", tabName = "infos_generales", icon = icon("info-circle"),
               menuSubItem("Vue Carte", tabName = "infos_gen_carte"),
               menuSubItem("Statistiques", tabName = "infos_gen_stats")),
      
      menuItem("2 - Infos Produits", tabName = "infos_produits", icon = icon("pills"),
               menuSubItem("Vue Carte", tabName = "produits_carte"),
               menuSubItem("Statistiques", tabName = "produits_stats")),
      
      menuItem("3 - Infos Famille", tabName = "infos_famille", icon = icon("sitemap"),
               menuSubItem("Vue Carte", tabName = "famille_carte"),
               menuSubItem("Statistiques", tabName = "famille_stats")),
      
      menuItem("4 - Infos TVA", tabName = "infos_tva", icon = icon("percent"),
               menuSubItem("Vue Carte", tabName = "tva_carte"),
               menuSubItem("Statistiques", tabName = "tva_stats")),
      
      menuItem("5 - Infos Caractère", tabName = "infos_caractere", icon = icon("map-marker"),
               menuSubItem("Vue Carte", tabName = "caractere_carte"),
               menuSubItem("Statistiques", tabName = "caractere_stats")),
      
      menuItem("6 - Segmentation", tabName = "segmentation", icon = icon("chart-pie"),
               menuSubItem("Vue Carte", tabName = "segment_carte"),
               menuSubItem("Statistiques", tabName = "segment_stats"))
    ),
    
    # Filtres
    div(style = "padding: 15px;",
        h4("Filtres Multi-Strates", style = "color: #00A86B;"),
        selectInput("filtre_region", "Région:", 
                    choices = c("Toutes" = "all", "Nord" = "nord", "Sud" = "sud", "Est" = "est", "Ouest" = "ouest"),
                    selected = "all"),
        selectInput("filtre_syndicat", "Syndicat:", 
                    choices = c("Tous" = "all", unique(pharmacies_data$syndicat)),
                    selected = "all"),
        sliderInput("filtre_ca", "Chiffre d'affaires (???):",
                    min = 0, max = 500000, value = c(0, 500000), step = 10000)
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #2c3e50;
        }
        .box {
          background-color: #34495e;
          border: 1px solid #00A86B;
        }
        .box-header {
          color: #00A86B;
        }
        .sidebar-menu > li.active > a {
          background-color: #00A86B !important;
        }
        .pharmacy-popup {
          animation: bounce 2s infinite;
        }
        @keyframes bounce {
          0%, 20%, 50%, 80%, 100% {
            transform: translateY(0);
          }
          40% {
            transform: translateY(-10px);
          }
          60% {
            transform: translateY(-5px);
          }
        }
        .main-header .navbar {
          background-color: #2c3e50 !important;
        }
        .main-header .logo {
          background-color: #2c3e50 !important;
        }
        .notification-icon {
          color: #e74c3c;
          animation: pulse 2s infinite;
        }
        @keyframes pulse {
          0% { opacity: 1; }
          50% { opacity: 0.5; }
          100% { opacity: 1; }
        }
      "))
    ),
    
    # Écran de chargement avec animation
    conditionalPanel(
      condition = "output.loading",
      div(id = "loading-screen",
          style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; 
                   background-color: rgba(44, 62, 80, 0.9); z-index: 9999; 
                   display: flex; justify-content: center; align-items: center; flex-direction: column;",
          div(style = "text-align: center; color: #00A86B;",
              h1("OSPHARM", style = "font-size: 48px; margin-bottom: 20px;"),
              div(class = "spinner-border", role = "status", style = "width: 3rem; height: 3rem; color: #00A86B;"),
              h3("Chargement des pharmacies...", style = "margin-top: 20px;"),
              div(id = "loading-counter", "0/500 pharmacies chargées", style = "margin-top: 10px; font-size: 18px;")
          )
      )
    ),
    
    tabItems(
      # Infos Générales - Vue Carte
      tabItem(tabName = "infos_gen_carte",
              fluidRow(
                box(title = "Cartographie des Pharmacies - Vue Générale", status = "primary", solidHeader = TRUE, width = 12,
                    withSpinner(leafletOutput("map_general", height = "600px"), color = "#00A86B")
                )
              ),
              fluidRow(
                valueBoxOutput("total_pharmacies"),
                valueBoxOutput("ca_total"),
                valueBoxOutput("ca_moyen")
              )
      ),
      
      # Infos Générales - Statistiques
      tabItem(tabName = "infos_gen_stats",
              fluidRow(
                box(title = "Répartition Géographique", status = "primary", solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("plot_repartition_geo"), color = "#00A86B")
                ),
                box(title = "Évolution du Chiffre d'Affaires", status = "primary", solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("plot_evolution_ca"), color = "#00A86B")
                )
              ),
              fluidRow(
                box(title = "Tableau Détaillé", status = "primary", solidHeader = TRUE, width = 12,
                    withSpinner(DT::dataTableOutput("table_general"), color = "#00A86B")
                )
              )
      ),
      
      # Infos Produits - Vue Carte
      tabItem(tabName = "produits_carte",
              fluidRow(
                box(title = "Cartographie - Analyse Produits", status = "success", solidHeader = TRUE, width = 12,
                    withSpinner(leafletOutput("map_produits", height = "600px"), color = "#00A86B")
                )
              )
      ),
      
      # Infos Produits - Statistiques
      tabItem(tabName = "produits_stats",
              fluidRow(
                box(title = "Distribution Nombre de Produits", status = "success", solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("plot_nb_produits"), color = "#00A86B")
                ),
                box(title = "Corrélation CA/Produits", status = "success", solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("plot_correlation"), color = "#00A86B")
                )
              )
      ),
      
      # Infos Famille - Vue Carte
      tabItem(tabName = "famille_carte",
              fluidRow(
                box(title = "Cartographie - Familles de Produits", status = "warning", solidHeader = TRUE, width = 12,
                    withSpinner(leafletOutput("map_famille", height = "600px"), color = "#00A86B")
                )
              )
      ),
      
      # Infos Famille - Statistiques
      tabItem(tabName = "famille_stats",
              fluidRow(
                box(title = "Répartition par Famille", status = "warning", solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("plot_famille"), color = "#00A86B")
                ),
                box(title = "CA par Famille", status = "warning", solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("plot_ca_famille"), color = "#00A86B")
                )
              )
      ),
      
      # Infos TVA - Vue Carte
      tabItem(tabName = "tva_carte",
              fluidRow(
                box(title = "Cartographie - Taux de TVA", status = "info", solidHeader = TRUE, width = 12,
                    withSpinner(leafletOutput("map_tva", height = "600px"), color = "#00A86B")
                )
              )
      ),
      
      # Infos TVA - Statistiques
      tabItem(tabName = "tva_stats",
              fluidRow(
                box(title = "Répartition TVA", status = "info", solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("plot_tva"), color = "#00A86B")
                ),
                box(title = "Impact TVA sur CA", status = "info", solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("plot_tva_ca"), color = "#00A86B")
                )
              )
      ),
      
      # Infos Caractère - Vue Carte
      tabItem(tabName = "caractere_carte",
              fluidRow(
                box(title = "Cartographie - Caractère des Pharmacies", status = "danger", solidHeader = TRUE, width = 12,
                    withSpinner(leafletOutput("map_caractere", height = "600px"), color = "#00A86B")
                )
              )
      ),
      
      # Infos Caractère - Statistiques
      tabItem(tabName = "caractere_stats",
              fluidRow(
                box(title = "Répartition Urbain/Rural", status = "danger", solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("plot_caractere"), color = "#00A86B")
                ),
                box(title = "Performance par Caractère", status = "danger", solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("plot_perf_caractere"), color = "#00A86B")
                )
              )
      ),
      
      # Segmentation - Vue Carte
      tabItem(tabName = "segment_carte",
              fluidRow(
                box(title = "Cartographie - Segmentation", status = "primary", solidHeader = TRUE, width = 12,
                    withSpinner(leafletOutput("map_segment", height = "600px"), color = "#00A86B")
                )
              )
      ),
      
      # Segmentation - Statistiques
      tabItem(tabName = "segment_stats",
              fluidRow(
                box(title = "Analyse Segmentation", status = "primary", solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("plot_segment"), color = "#00A86B")
                ),
                box(title = "Matrice Segment/Performance", status = "primary", solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("plot_matrice_segment"), color = "#00A86B")
                )
              )
      )
    )
  )
)

# Serveur
server <- function(input, output, session) {
  
  # Données réactives filtrées
  filtered_data <- reactive({
    data <- pharmacies_data
    
    if(input$filtre_syndicat != "all") {
      data <- data[data$syndicat == input$filtre_syndicat, ]
    }
    
    data <- data[data$chiffre_affaires >= input$filtre_ca[1] & 
                   data$chiffre_affaires <= input$filtre_ca[2], ]
    
    return(data)
  })
  
  # Animation de chargement
  output$loading <- reactive({
    return(TRUE)
  })
  outputOptions(output, "loading", suspendWhenHidden = FALSE)
  
  # Simulation du chargement progressif
  observe({
    for(i in 1:500) {
      Sys.sleep(0.01)  # Simulation du temps de chargement
      session$sendCustomMessage("updateCounter", i)
    }
    Sys.sleep(1)
    session$sendCustomMessage("hideLoading", "")
  })
  
  # JavaScript pour l'animation
  session$sendCustomMessage("addJS", "
    Shiny.addCustomMessageHandler('updateCounter', function(count) {
      document.getElementById('loading-counter').innerText = count + '/500 pharmacies chargées';
    });
    
    Shiny.addCustomMessageHandler('hideLoading', function(message) {
      document.getElementById('loading-screen').style.display = 'none';
    });
  ")
  
  # Cartes leaflet
  output$map_general <- renderLeaflet({
    leaflet(filtered_data()) %>%
      addTiles() %>%
      setView(lng = 2.3, lat = 46.2, zoom = 6) %>%
      addCircleMarkers(
        ~lng, ~lat,
        popup = ~paste("<b>", nom, "</b><br>",
                       "CA: ", format(chiffre_affaires, big.mark = " "), "???<br>",
                       "Produits: ", nb_produits),
        radius = ~sqrt(chiffre_affaires)/100,
        color = "#00A86B",
        fillOpacity = 0.7,
        clusterOptions = markerClusterOptions()
      )
  })
  
  output$map_produits <- renderLeaflet({
    leaflet(filtered_data()) %>%
      addTiles() %>%
      setView(lng = 2.3, lat = 46.2, zoom = 6) %>%
      addCircleMarkers(
        ~lng, ~lat,
        popup = ~paste("<b>", nom, "</b><br>",
                       "Nombre de produits: ", nb_produits),
        radius = ~sqrt(nb_produits)/10,
        color = "#28a745",
        fillOpacity = 0.7
      )
  })
  
  output$map_famille <- renderLeaflet({
    colors <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4")
    pal <- colorFactor(colors, domain = filtered_data()$famille_principale)
    
    leaflet(filtered_data()) %>%
      addTiles() %>%
      setView(lng = 2.3, lat = 46.2, zoom = 6) %>%
      addCircleMarkers(
        ~lng, ~lat,
        popup = ~paste("<b>", nom, "</b><br>",
                       "Famille: ", famille_principale),
        color = ~pal(famille_principale),
        fillOpacity = 0.7
      ) %>%
      addLegend("bottomright", pal = pal, values = ~famille_principale,
                title = "Famille Principale")
  })
  
  output$map_tva <- renderLeaflet({
    colors <- c("#e74c3c", "#f39c12", "#2ecc71")
    pal <- colorFactor(colors, domain = filtered_data()$tva)
    
    leaflet(filtered_data()) %>%
      addTiles() %>%
      setView(lng = 2.3, lat = 46.2, zoom = 6) %>%
      addCircleMarkers(
        ~lng, ~lat,
        popup = ~paste("<b>", nom, "</b><br>",
                       "Taux TVA: ", tva),
        color = ~pal(tva),
        fillOpacity = 0.7
      ) %>%
      addLegend("bottomright", pal = pal, values = ~tva,
                title = "Taux de TVA")
  })
  
  output$map_caractere <- renderLeaflet({
    colors <- c("#3498db", "#e67e22", "#9b59b6")
    pal <- colorFactor(colors, domain = filtered_data()$caractere)
    
    leaflet(filtered_data()) %>%
      addTiles() %>%
      setView(lng = 2.3, lat = 46.2, zoom = 6) %>%
      addCircleMarkers(
        ~lng, ~lat,
        popup = ~paste("<b>", nom, "</b><br>",
                       "Caractère: ", caractere),
        color = ~pal(caractere),
        fillOpacity = 0.7
      ) %>%
      addLegend("bottomright", pal = pal, values = ~caractere,
                title = "Caractère")
  })
  
  output$map_segment <- renderLeaflet({
    colors <- c("#gold", "#silver", "#bronze")
    pal <- colorFactor(colors, domain = filtered_data()$segment)
    
    leaflet(filtered_data()) %>%
      addTiles() %>%
      setView(lng = 2.3, lat = 46.2, zoom = 6) %>%
      addCircleMarkers(
        ~lng, ~lat,
        popup = ~paste("<b>", nom, "</b><br>",
                       "Segment: ", segment),
        color = ~pal(segment),
        fillOpacity = 0.7
      ) %>%
      addLegend("bottomright", pal = pal, values = ~segment,
                title = "Segment")
  })
  
  # ValueBoxes
  output$total_pharmacies <- renderValueBox({
    valueBox(
      value = nrow(filtered_data()),
      subtitle = "Pharmacies totales",
      icon = icon("hospital"),
      color = "green"
    )
  })
  
  output$ca_total <- renderValueBox({
    valueBox(
      value = paste(format(sum(filtered_data()$chiffre_affaires)/1000000, digits = 1), "M???"),
      subtitle = "CA Total",
      icon = icon("euro-sign"),
      color = "blue"
    )
  })
  
  output$ca_moyen <- renderValueBox({
    valueBox(
      value = paste(format(mean(filtered_data()$chiffre_affaires)/1000, digits = 1), "k???"),
      subtitle = "CA Moyen",
      icon = icon("chart-line"),
      color = "yellow"
    )
  })
  
  # Graphiques
  output$plot_famille <- renderPlotly({
    data <- filtered_data() %>%
      group_by(famille_principale) %>%
      summarise(count = n())
    
    p <- ggplot(data, aes(x = famille_principale, y = count, fill = famille_principale)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Répartition par Famille de Produits",
           x = "Famille", y = "Nombre de pharmacies") +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  output$plot_nb_produits <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = nb_produits)) +
      geom_histogram(bins = 30, fill = "#00A86B", alpha = 0.7) +
      theme_minimal() +
      labs(title = "Distribution du Nombre de Produits",
           x = "Nombre de produits", y = "Fréquence")
    
    ggplotly(p)
  })
  
  output$plot_correlation <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = nb_produits, y = chiffre_affaires)) +
      geom_point(alpha = 0.6, color = "#00A86B") +
      geom_smooth(method = "lm", color = "#e74c3c") +
      theme_minimal() +
      labs(title = "Corrélation CA / Nombre de Produits",
           x = "Nombre de produits", y = "Chiffre d'affaires (???)")
    
    ggplotly(p)
  })
  
  output$plot_tva <- renderPlotly({
    data <- filtered_data() %>%
      group_by(tva) %>%
      summarise(count = n())
    
    p <- plot_ly(data, labels = ~tva, values = ~count, type = 'pie',
                 textposition = 'inside', textinfo = 'label+percent') %>%
      layout(title = "Répartition des Taux de TVA")
    
    p
  })
  
  output$plot_caractere <- renderPlotly({
    data <- filtered_data() %>%
      group_by(caractere) %>%
      summarise(count = n())
    
    p <- plot_ly(data, x = ~caractere, y = ~count, type = 'bar',
                 marker = list(color = c('#3498db', '#e67e22', '#9b59b6'))) %>%
      layout(title = "Répartition Urbain/Rural",
             xaxis = list(title = "Caractère"),
             yaxis = list(title = "Nombre de pharmacies"))
    
    p
  })
  
  output$plot_segment <- renderPlotly({
    data <- filtered_data() %>%
      group_by(segment) %>%
      summarise(
        count = n(),
        ca_moyen = mean(chiffre_affaires)
      )
    
    p <- plot_ly(data, x = ~segment, y = ~count, type = 'bar', name = 'Nombre',
                 marker = list(color = '#00A86B')) %>%
      add_trace(y = ~ca_moyen/1000, name = 'CA Moyen (k???)', yaxis = 'y2',
                marker = list(color = '#e74c3c')) %>%
      layout(title = "Analyse par Segment",
             xaxis = list(title = "Segment"),
             yaxis = list(title = "Nombre de pharmacies"),
             yaxis2 = list(overlaying = 'y', side = 'right', title = 'CA Moyen (k???)'))
    
    p
  })
  
  # Tableaux
  output$table_general <- DT::renderDataTable({
    DT::datatable(filtered_data(), 
                  options = list(pageLength = 10, scrollX = TRUE),
                  filter = 'top')
  })
  
  # Autres graphiques (à compléter selon les besoins)
  output$plot_repartition_geo <- renderPlotly({
    # Graphique de répartition géographique simulé
    regions <- c("Nord", "Sud", "Est", "Ouest", "Centre")
    counts <- sample(50:150, 5)
    
    p <- plot_ly(x = regions, y = counts, type = 'bar',
                 marker = list(color = '#00A86B')) %>%
      layout(title = "Répartition Géographique",
             xaxis = list(title = "Région"),
             yaxis = list(title = "Nombre de pharmacies"))
    p
  })
  
  output$plot_evolution_ca <- renderPlotly({
    # Simulation d'une évolution temporelle
    mois <- month.name[1:12]
    ca_evolution <- cumsum(sample(1000:5000, 12))
    
    p <- plot_ly(x = mois, y = ca_evolution, type = 'scatter', mode = 'lines+markers',
                 line = list(color = '#00A86B', width = 3),
                 marker = list(color = '#00A86B', size = 8)) %>%
      layout(title = "Évolution du Chiffre d'Affaires",
             xaxis = list(title = "Mois"),
             yaxis = list(title = "CA Cumulé (k???)"))
    p
  })
  
  output$plot_ca_famille <- renderPlotly({
    data <- filtered_data() %>%
      group_by(famille_principale) %>%
      summarise(ca_total = sum(chiffre_affaires)/1000)
    
    p <- plot_ly(data, x = ~famille_principale, y = ~ca_total, type = 'bar',
                 marker = list(color = '#f39c12')) %>%
      layout(title = "Chiffre d'Affaires par Famille",
             xaxis = list(title = "Famille de produits"),
             yaxis = list(title = "CA Total (k???)"))
    p
  })
  
  output$plot_tva_ca <- renderPlotly({
    data <- filtered_data() %>%
      group_by(tva) %>%
      summarise(ca_moyen = mean(chiffre_affaires)/1000)
    
    p <- plot_ly(data, x = ~tva, y = ~ca_moyen, type = 'bar',
                 marker = list(color = '#17a2b8')) %>%
      layout(title = "Impact TVA sur le CA Moyen",
             xaxis = list(title = "Taux de TVA"),
             yaxis = list(title = "CA Moyen (k???)"))
    p
  })
  
  output$plot_perf_caractere <- renderPlotly({
    data <- filtered_data() %>%
      group_by(caractere) %>%
      summarise(
        ca_moyen = mean(chiffre_affaires)/1000,
        nb_produits_moyen = mean(nb_produits)
      )
    
    p <- plot_ly(data, x = ~caractere, y = ~ca_moyen, type = 'bar', name = 'CA Moyen (k???)',
                 marker = list(color = '#dc3545')) %>%
      add_trace(y = ~nb_produits_moyen/10, name = 'Nb Produits Moyen (/10)', yaxis = 'y2',
                marker = list(color = '#28a745')) %>%
      layout(title = "Performance par Caractère",
             xaxis = list(title = "Caractère"),
             yaxis = list(title = "CA Moyen (k???)"),
             yaxis2 = list(overlaying = 'y', side = 'right', title = 'Nb Produits Moyen'))
    
    p
  })
  
  output$plot_matrice_segment <- renderPlotly({
    data <- filtered_data() %>%
      group_by(segment) %>%
      summarise(
        ca_moyen = mean(chiffre_affaires),
        nb_produits_moyen = mean(nb_produits)
      )
    
    p <- plot_ly(data, x = ~nb_produits_moyen, y = ~ca_moyen, 
                 text = ~segment, type = 'scatter', mode = 'markers+text',
                 marker = list(size = 20, color = c('#FFD700', '#C0C0C0', '#CD7F32'))) %>%
      layout(title = "Matrice Segment/Performance",
             xaxis = list(title = "Nombre de produits moyen"),
             yaxis = list(title = "CA Moyen (???)"))
    
    p
  })
}

# Lancement de l'application
shinyApp(ui = ui, server = server)