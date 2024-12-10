turrialbaServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Cargar datos específicos de Pérez Zeledón
    UGM_escala_turri <- readRDS("datos/Turrialba/datos/UGM_escala_turri.rds")
    pal_turri <- colorFactor(palette = c("#f39325", "#1270b8"), domain = UGM_turri$cluster)
    
    
    # Mapa principal
    output$turri_mapDisplay <- renderLeaflet({
      generate_map(UGM_turri, pal_turri)
    })
    
    # Observador principal para actualizar el mapa según la variable seleccionada
    observe({
      req(input$turri_variable)
      proxy <- leafletProxy("turri_mapDisplay", session)
      proxy %>% clearGroup("dynamicLayer") %>% clearMarkers()
      
      # Condiciones según la variable seleccionada en turri_variable
      if (input$turri_variable == "turri_infraestructura") {
        
        req(input$turri_infraestructura_variable)
        
        # Diccionario de categorías de infraestructura
        categoria_id_turri <- c(
          "turri_Comercio" = 1, "turri_Industria" = 2, "turri_Servicios" = 3, "turri_Gobierno" = 4,
          "turri_Puertos_aeropuertos" = 5, "turri_Educacion" = 6, "turri_Universidades" = 7,
          "turri_Turisticas" = 8, "turri_Recreativos" = 9, "turri_Ciclovia" = 10, "turri_Cultura" = 11,
          "turri_Religioso" = 12, "turri_Centros_de_cuido" = 13, "turri_Salud" = 14
        )
        
        # Filtrar infraestructura
        categoria_seleccionada_turri <- categoria_id_turri[input$turri_infraestructura_variable]
        infra_filtrada_turri <- infraestructura_turri %>% filter(id == categoria_seleccionada_turri)
        
        # Añadir la capa de infraestructura urbana al mapa
        if (nrow(infra_filtrada_turri) > 0) {
          if (all(st_geometry_type(infra_filtrada_turri) == "POINT")) {
            proxy %>% addMarkers(data = infra_filtrada_turri, label = ~id, group = "dynamicLayer")
          } else {
            proxy %>% addPolygons(data = infra_filtrada_turri, fillColor = "#FF6600", fillOpacity = 0.5,
                                  weight = 1, color = "black", group = "dynamicLayer",
                                  highlightOptions = highlightOptions(color = "#FF6600", weight = 2, bringToFront = TRUE, opacity = 1))
          }
        }
        
      } else if (input$turri_variable == "turri_edificaciones") {
        proxy %>% addPolygons(data = edificaciones_turri, fillColor = "#666666", fillOpacity = 0.5,
                              weight = 1, color = "black", group = "dynamicLayer")
        
      } else if (input$turri_variable == "turri_inundacion") {
        proxy %>% addPolygons(data = inundaciones_turri, stroke = FALSE, fillColor = "#3e499a",
                              fillOpacity = 0.5, group = "dynamicLayer")
        
      } else if (input$turri_variable == "turri_medidores") {
        proxy %>% addCircleMarkers(data = medidores_turri, radius = 2, color = "blue", opacity = 0.8)
        
      } else if (input$turri_variable == "turri_postes") {
        proxy %>% addCircleMarkers(data = postes_turri, radius = 2, color = "green", opacity = 0.8)
        
      } else if (input$turri_variable == "turri_transformadores") {
        proxy %>% addCircleMarkers(data = transformadores_turri, radius = 2, color = "red", opacity = 0.8)
        
      } else if (input$turri_variable == "turri_valorizables") {
        proxy %>% addPolygons(data = valorizables_turri, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, color = "green", group = "dynamicLayer")
        
      } else if (input$turri_variable == "turri_novalorizables") {
        proxy %>% addPolygons(data = valorizables_turri, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, color = "blue", group = "dynamicLayer")
        
      } else if (input$turri_variable == "turri_congestionamiento") {
        
        req(input$turri_congestionamiento_variable)
        
        # Selección del nivel de congestionamiento
        congestion_data <- switch(input$turri_congestionamiento_variable,
                                  "turri_1" = congestionamiento1_turri,
                                  "turri_2" = congestionamiento2_turri,
                                  "turri_3" = congestionamiento3_turri,
                                  "turri_4" = congestionamiento4_turri,
                                  "turri_5" = congestionamiento5_turri)
        color <- switch(input$turri_congestionamiento_variable,
                        "turri_1" = "blue", "turri_2" = "green", "turri_3" = "yellow",
                        "turri_4" = "#c45906", "turri_5" = "red")
        
        proxy %>% addPolygons(data = congestion_data, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, color = color, group = "dynamicLayer")
        
      } else if (input$turri_variable == "turri_accidentes") {
        proxy %>% addPolygons(data = accidentes_turri, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, group = "dynamicLayer")
        
      } else if (input$turri_variable == "turri_puentes") {
        proxy %>% addPolygons(data = puentes_turri, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, group = "dynamicLayer")
        
      } else if (input$turri_variable == "turri_RVN") {
        proxy %>% addPolygons(data = RVN_turri, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, group = "dynamicLayer")
      }
    })
  })
}
