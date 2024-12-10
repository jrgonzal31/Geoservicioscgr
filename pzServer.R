pzServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Cargar datos específicos de Pérez Zeledón
    UGM_escala_pz <- readRDS("datos/PZ/datos/UGM_escala_pz.rds")
    pal_pz <- colorFactor(palette = c("#f39325", "#1270b8", "#63957b"), domain = UGM_escala_pz$cluster)
    
    
    Agua_pz <- Agua_pz %>%
      filter(POTABLE == POTABLE)
    
    saneamiento_pz <- Agua_pz %>%
      filter(DSC_SIST_S != "SIN SANEAMIENTO")
    
    # Mapa principal
    output$pz_mapDisplay <- renderLeaflet({
      generate_map(UGM_pz, pal_pz)
    })
    
    # Observador principal para actualizar el mapa según la variable seleccionada
    observe({
      req(input$pz_variable)
      proxy <- leafletProxy("pz_mapDisplay", session)
      proxy %>% clearGroup("dynamicLayer") %>% clearMarkers()
      
      # Condiciones según la variable seleccionada en pz_variable
      if (input$pz_variable == "pz_infraestructura") {
        
        req(input$pz_infraestructura_variable)
        
        # Diccionario de categorías de infraestructura
        categoria_id_pz <- c(
          "pz_Comercio" = 1, "pz_Industria" = 2, "pz_Servicios" = 3, "pz_Gobierno" = 4,
          "pz_Puertos_aeropuertos" = 5, "pz_Educacion" = 6, "pz_Universidades" = 7,
          "pz_Turisticas" = 8, "pz_Recreativos" = 9, "pz_Ciclovia" = 10, "pz_Cultura" = 11,
          "pz_Religioso" = 12, "pz_Centros_de_cuido" = 13, "pz_Salud" = 14
        )
        
        # Filtrar infraestructura
        categoria_seleccionada_pz <- categoria_id_pz[input$pz_infraestructura_variable]
        infra_filtrada_pz <- infraestructura_pz %>% filter(id == categoria_seleccionada_pz)
        
        # Añadir la capa de infraestructura urbana al mapa
        if (nrow(infra_filtrada_pz) > 0) {
          if (all(st_geometry_type(infra_filtrada_pz) == "POINT")) {
            proxy %>% addMarkers(data = infra_filtrada_pz, label = ~id, group = "dynamicLayer")
          } else {
            proxy %>% addPolygons(data = infra_filtrada_pz, fillColor = "#FF6600", fillOpacity = 0.5,
                                  weight = 1, color = "black", group = "dynamicLayer",
                                  highlightOptions = highlightOptions(color = "#FF6600", weight = 2, bringToFront = TRUE, opacity = 1))
          }
        }
        
      } else if (input$pz_variable == "pz_edificaciones") {
        proxy %>% addPolygons(data = edificaciones_pz, fillColor = "#666666", fillOpacity = 0.5,
                              weight = 1, color = "black", group = "dynamicLayer")
        
      } else if (input$pz_variable == "pz_inundacion") {
        proxy %>% addPolygons(data = inundaciones_pz, stroke = FALSE, fillColor = "#3e499a",
                              fillOpacity = 0.5, group = "dynamicLayer")
        
      } else if (input$pz_variable == "pz_medidores") {
        proxy %>% addCircleMarkers(data = medidores_pz, radius = 2, color = "blue", opacity = 0.8)
        
      } else if (input$pz_variable == "pz_postes") {
        proxy %>% addCircleMarkers(data = postes_pz, radius = 2, color = "green", opacity = 0.8)
        
      } else if (input$pz_variable == "pz_transformadores") {
        proxy %>% addCircleMarkers(data = transformadores_pz, radius = 2, color = "red", opacity = 0.8)
        
      } else if (input$pz_variable == "pz_Potable") {
        proxy %>% addCircleMarkers(data = Agua_pz, radius = 2, color = "blue", opacity = 0.8)
        
      } else if (input$pz_variable == "pz_saneamiento") {
        proxy %>% addCircleMarkers(data = saneamiento_pz, radius = 2, color = "green", opacity = 0.8)
        
      } else if (input$pz_variable == "pz_valorizables") {
        proxy %>% addPolygons(data = no_valorizables_pz, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, color = "green", group = "dynamicLayer")
        
      } else if (input$pz_variable == "pz_novalorizables") {
        proxy %>% addPolygons(data = no_valorizables_pz, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, color = "blue", group = "dynamicLayer")
        
      } else if (input$pz_variable == "pz_congestionamiento") {
        
        req(input$pz_congestionamiento_variable)
        
        # Selección del nivel de congestionamiento
        congestion_data <- switch(input$pz_congestionamiento_variable,
                                  "pz_1" = congestionamiento1_pz,
                                  "pz_2" = congestionamiento2_pz,
                                  "pz_3" = congestionamiento3_pz,
                                  "pz_4" = congestionamiento4_pz,
                                  "pz_5" = congestionamiento5_pz)
        color <- switch(input$pz_congestionamiento_variable,
                        "pz_1" = "blue", "pz_2" = "green", "pz_3" = "yellow",
                        "pz_4" = "#c45906", "pz_5" = "red")
        
        proxy %>% addPolygons(data = congestion_data, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, color = color, group = "dynamicLayer")
        
      } else if (input$pz_variable == "pz_accidentes") {
        proxy %>% addCircleMarkers(data = accidentes_pz, radius = 0.5, color = "blue",
                                   opacity = 0.1, group = "dynamicLayer")
        
      } else if (input$pz_variable == "pz_puentes") {
        proxy %>% addPolygons(data = puentes_pz, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, group = "dynamicLayer")
        
      } else if (input$pz_variable == "pz_RVN") {
        proxy %>% addPolygons(data = RVN_pz, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, group = "dynamicLayer")
      }
    })
  })
}




