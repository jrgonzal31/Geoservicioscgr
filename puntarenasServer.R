# puntarenas_module.R
puntarenasServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Cargar datos específicos de Pérez Zeledón
    UGM_escala_punt <- readRDS("datos/Puntarenas/datos/UGM_escala_punt.rds")
    pal_punt <- colorFactor(palette = c("#f39325", "#1270b8", "#63957b"), domain = UGM_punt$cluster)
    
    
    Agua_punt <- Agua_punt %>%
      filter(Potable == Potable)
    
    saneamiento_punt <- Agua_punt %>%
      filter(DSC_SIST_S != "SIN SANEAMIENTO")
    
    # Mapa principal
    output$punt_mapDisplay <- renderLeaflet({
      generate_map(UGM_punt, pal_punt)
    })
    
    # Observador principal para actualizar el mapa según la variable seleccionada
    observe({
      req(input$punt_variable)
      proxy <- leafletProxy("punt_mapDisplay", session)
      proxy %>% clearGroup("dynamicLayer") %>% clearMarkers()
      
      # Condiciones según la variable seleccionada en punt_variable
      if (input$punt_variable == "punt_infraestructura") {
        
        req(input$punt_infraestructura_variable)
        
        # Diccionario de categorías de infraestructura
        categoria_id_punt <- c(
          "punt_Comercio" = 1, "punt_Industria" = 2, "punt_Servicios" = 3, "punt_Gobierno" = 4,
          "punt_Puertos_aeropuertos" = 5, "punt_Educacion" = 6, "punt_Universidades" = 7,
          "punt_Turisticas" = 8, "punt_Recreativos" = 9, "punt_Ciclovia" = 10, "punt_Cultura" = 11,
          "punt_Religioso" = 12, "punt_Centros_de_cuido" = 13, "punt_Salud" = 14
        )
        
        # Filtrar infraestructura
        categoria_seleccionada_punt <- categoria_id_punt[input$punt_infraestructura_variable]
        infra_filtrada_punt <- infraestructura_punt %>% filter(id == categoria_seleccionada_punt)
        
        # Añadir la capa de infraestructura urbana al mapa
        if (nrow(infra_filtrada_punt) > 0) {
          if (all(st_geometry_type(infra_filtrada_punt) == "POINT")) {
            proxy %>% addMarkers(data = infra_filtrada_punt, label = ~id, group = "dynamicLayer")
          } else {
            proxy %>% addPolygons(data = infra_filtrada_punt, fillColor = "#FF6600", fillOpacity = 0.5,
                                  weight = 1, color = "black", group = "dynamicLayer",
                                  highlightOptions = highlightOptions(color = "#FF6600", weight = 2, bringToFront = TRUE, opacity = 1))
          }
        }
        
      } else if (input$punt_variable == "punt_edificaciones") {
        proxy %>% addPolygons(data = edificaciones_punt, fillColor = "#666666", fillOpacity = 0.5,
                              weight = 1, color = "black", group = "dynamicLayer")
        
      } else if (input$punt_variable == "punt_inundacion") {
        proxy %>% addPolygons(data = inundaciones_punt, stroke = FALSE, fillColor = "#3e499a",
                              fillOpacity = 0.5, group = "dynamicLayer")
        
      } else if (input$punt_variable == "punt_medidores") {
        proxy %>% addCircleMarkers(data = medidores_punt, radius = 2, color = "blue", opacity = 0.8)
        
      } else if (input$punt_variable == "punt_postes") {
        proxy %>% addCircleMarkers(data = postes_punt, radius = 2, color = "green", opacity = 0.8)
        
      } else if (input$punt_variable == "punt_transformadores") {
        proxy %>% addCircleMarkers(data = transformadores_punt, radius = 2, color = "red", opacity = 0.8)
        
      } else if (input$punt_variable == "punt_Potable") {
        proxy %>% addCircleMarkers(data = Agua_punt, radius = 2, color = "blue", opacity = 0.8)
        
      } else if (input$punt_variable == "punt_saneamiento") {
        proxy %>% addCircleMarkers(data = saneamiento_punt, radius = 2, color = "green", opacity = 0.8)
        
      }  else if (input$punt_variable == "punt_congestionamiento") {
        
        req(input$punt_congestionamiento_variable)
        
        # Selección del nivel de congestionamiento
        congestion_data <- switch(input$punt_congestionamiento_variable,
                                  "punt_1" = congestionamiento1_punt,
                                  "punt_2" = congestionamiento2_punt,
                                  "punt_3" = congestionamiento3_punt,
                                  "punt_4" = congestionamiento4_punt,
                                  "punt_5" = congestionamiento5_punt)
        color <- switch(input$punt_congestionamiento_variable,
                        "punt_1" = "blue", "punt_2" = "green", "punt_3" = "yellow",
                        "punt_4" = "#c45906", "punt_5" = "red")
        
        proxy %>% addPolygons(data = congestion_data, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, color = color, group = "dynamicLayer")
        
      } else if (input$punt_variable == "punt_accidentes") {
        proxy %>% addCircleMarkers(data = accidentes_punt, radius = 0.5, color = "blue",
                                   opacity = 0.1, group = "dynamicLayer")
        
      } else if (input$punt_variable == "punt_puentes") {
        proxy %>% addPolygons(data = puentes_punt, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, group = "dynamicLayer")
        
      } else if (input$punt_variable == "punt_RVN") {
        proxy %>% addPolygons(data = RVN_punt, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, group = "dynamicLayer")
      }
    })
  })
}
