scServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Cargar datos específicos de Pérez Zeledón
    UGM_escala_sc <- readRDS("datos/SanCarlos/datos/UGM_escala_sc.rds")
    pal_sc <- colorFactor(palette = c("#f39325", "#1270b8", "#63957b"), domain = UGM_escala_sc$cluster)
    
    Agua_sc <- Agua_sc %>%
      filter(!is.na(HIDROMETRO))
    
    Agua_sc <- st_make_valid(Agua_sc)
    Agua_sc <- st_centroid(Agua_sc)
    
    novalorizables_sc <- novalorizables_sc %>%
      select(RECOLECCIO, ID_MGN) %>%
      filter(!is.na(RECOLECCIO))
    
    # Mapa principal
    output$sc_mapDisplay <- renderLeaflet({
      generate_map(UGM_sc, pal_sc)
    })
    
    # Observador principal para actualizar el mapa según la variable seleccionada
    observe({
      req(input$sc_variable)
      proxy <- leafletProxy("sc_mapDisplay", session)
      proxy %>% clearGroup("dynamicLayer") %>% clearMarkers()
      
      # Condiciones según la variable seleccionada en sc_variable
      if (input$sc_variable == "sc_infraestructura") {
        
        req(input$sc_infraestructura_variable)
        
        # Diccionario de categorías de infraestructura
        categoria_id_sc <- c(
          "sc_Comercio" = 1, "sc_Industria" = 2, "sc_Servicios" = 3, "sc_Gobierno" = 4,
          "sc_Puertos_aeropuertos" = 5, "sc_Educacion" = 6, "sc_Universidades" = 7,
          "sc_Turisticas" = 8, "sc_Recreativos" = 9, "sc_Ciclovia" = 10, "sc_Cultura" = 11,
          "sc_Religioso" = 12, "sc_Centros_de_cuido" = 13, "sc_Salud" = 14
        )
        
        # Filtrar infraestructura
        categoria_seleccionada_sc <- categoria_id_sc[input$sc_infraestructura_variable]
        infra_filtrada_sc <- infraestructura_sc %>% filter(id == categoria_seleccionada_sc)
        
        # Añadir la capa de infraestructura urbana al mapa
        if (nrow(infra_filtrada_sc) > 0) {
          if (all(st_geometry_type(infra_filtrada_sc) == "POINT")) {
            proxy %>% addMarkers(data = infra_filtrada_sc, label = ~id, group = "dynamicLayer")
          } else {
            proxy %>% addPolygons(data = infra_filtrada_sc, fillColor = "#FF6600", fillOpacity = 0.5,
                                  weight = 1, color = "black", group = "dynamicLayer",
                                  highlightOptions = highlightOptions(color = "#FF6600", weight = 2, bringToFront = TRUE, opacity = 1))
          }
        }
        
      } else if (input$sc_variable == "sc_edificaciones") {
        proxy %>% addPolygons(data = edificaciones_sc, fillColor = "#666666", fillOpacity = 0.5,
                              weight = 1, color = "black", group = "dynamicLayer")
        
      } else if (input$sc_variable == "sc_postes") {
        proxy %>% addCircleMarkers(data = postes_sc, radius = 2, color = "green", opacity = 0.8)
        
      } else if (input$sc_variable == "sc_transformadores") {
        proxy %>% addCircleMarkers(data = transformadores_sc, radius = 2, color = "red", opacity = 0.8)
        
      } else if (input$sc_variable == "sc_Potable") {
        proxy %>% addCircleMarkers(data = Agua_sc, radius = 2, color = "blue", opacity = 0.8)
        
      } else if (input$sc_variable == "sc_valorizables") {
        proxy %>% addPolygons(data = novalorizables_sc, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, color = "green", group = "dynamicLayer")
        
      } else if (input$sc_variable == "sc_novalorizables") {
        proxy %>% addPolygons(data = novalorizables_sc, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, color = "blue", group = "dynamicLayer")
        
      } else if (input$sc_variable == "sc_congestionamiento") {
        
        req(input$sc_congestionamiento_variable)
        
        # Selección del nivel de congestionamiento
        congestion_data <- switch(input$sc_congestionamiento_variable,
                                  "sc_1" = congestionamiento1_sc,
                                  "sc_2" = congestionamiento2_sc,
                                  "sc_3" = congestionamiento3_sc,
                                  "sc_4" = congestionamiento4_sc,
                                  "sc_5" = congestionamiento5_sc)
        color <- switch(input$sc_congestionamiento_variable,
                        "sc_1" = "blue", "sc_2" = "green", "sc_3" = "yellow",
                        "sc_4" = "#c45906", "sc_5" = "red")
        
        proxy %>% addPolygons(data = congestion_data, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, color = color, group = "dynamicLayer")
        
      } else if (input$sc_variable == "sc_accidentes") {
        proxy %>% addCircleMarkers(data = accidentes_sc, radius = 0.5, color = "blue",
                                   opacity = 0.1, group = "dynamicLayer")
        
      } else if (input$sc_variable == "sc_puentes") {
        proxy %>% addPolygons(data = puentes_sc, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, group = "dynamicLayer")
        
      } else if (input$sc_variable == "sc_RVN") {
        proxy %>% addPolygons(data = RVN_sc, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, group = "dynamicLayer")
      }
    })
  })
}

