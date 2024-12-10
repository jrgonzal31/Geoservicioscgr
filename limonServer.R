# limon_module_server.R


# liberia_module.R
limonServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Cargar datos específicos de Pérez Zeledón
    UGM_lim_escala <- readRDS("datos/Limon/datos/UGM_escala_lim.rds")
    pal_lim <- colorFactor(palette = c("#f39325", "#1270b8"), domain = UGM_lim$cluster)
    
    Agua_lim <- Agua_lim %>% filter(Potable == Potable)
    saneamiento_lim <- Agua_lim %>% filter(DSC_SIST_S != "SIN SANEAMIENTO")
    
    # Mapa principal
    output$lim_mapDisplay <- renderLeaflet({
      generate_map(UGM_lim, pal_lim)
    })
    
    # Observador principal para actualizar el mapa según la variable seleccionada
    observe({
      req(input$lim_variable)
      proxy <- leafletProxy("lim_mapDisplay", session)
      proxy %>% clearGroup("dynamicLayer") %>% clearMarkers()
      
      # Condiciones según la variable seleccionada en lim_variable
      if (input$lim_variable == "lim_infraestructura") {
        
        req(input$lim_infraestructura_variable)
        
        # Diccionario de categorías de infraestructura
        categoria_id_lim <- c(
          "lim_Comercio" = 1, "lim_Industria" = 2, "lim_Servicios" = 3, "lim_Gobierno" = 4,
          "lim_Puertos_aeropuertos" = 5, "lim_Educacion" = 6, "lim_Universidades" = 7,
          "lim_Turisticas" = 8, "lim_Recreativos" = 9, "lim_Ciclovia" = 10, "lim_Cultura" = 11,
          "lim_Religioso" = 12, "lim_Centros_de_cuido" = 13, "lim_Salud" = 14
        )
        
        # Filtrar infraestructura
        categoria_seleccionada_lim <- categoria_id_lim[input$lim_infraestructura_variable]
        infra_filtrada_lim <- infraestructura_lim %>% filter(id == categoria_seleccionada_lim)
        
        # Añadir la capa de infraestructura urbana al mapa
        if (nrow(infra_filtrada_lim) > 0) {
          if (all(st_geometry_type(infra_filtrada_lim) == "POINT")) {
            proxy %>% addMarkers(data = infra_filtrada_lim, label = ~id, group = "dynamicLayer")
          } else {
            proxy %>% addPolygons(data = infra_filtrada_lim, fillColor = "#FF6600", fillOpacity = 0.5,
                                  weight = 1, color = "black", group = "dynamicLayer",
                                  highlightOptions = highlightOptions(color = "#FF6600", weight = 2, bringToFront = TRUE, opacity = 1))
          }
        }
        
      } else if (input$lim_variable == "lim_edificaciones") {
        proxy %>% addPolygons(data = edificaciones_lim, fillColor = "#666666", fillOpacity = 0.5,
                              weight = 1, color = "black", group = "dynamicLayer")
        
      } else if (input$lim_variable == "lim_inundacion") {
        proxy %>% addPolygons(data = inundaciones_lim, stroke = FALSE, fillColor = "#3e499a",
                              fillOpacity = 0.5, group = "dynamicLayer")
        
      } else if (input$lim_variable == "lim_medidores") {
        proxy %>% addCircleMarkers(data = medidores_lim, radius = 2, color = "blue", opacity = 0.8)
        
      } else if (input$lim_variable == "lim_postes") {
        proxy %>% addCircleMarkers(data = postes_lim, radius = 2, color = "green", opacity = 0.8)
        
      } else if (input$lim_variable == "lim_transformadores") {
        proxy %>% addCircleMarkers(data = transformadores_lim, radius = 2, color = "red", opacity = 0.8)
        
      } else if (input$lim_variable == "lim_Potable") {
        proxy %>% addCircleMarkers(data = Agua_lim, radius = 2, color = "blue", opacity = 0.8)
        
      } else if (input$lim_variable == "lim_saneamiento") {
        proxy %>% addCircleMarkers(data = saneamiento_lim, radius = 2, color = "green", opacity = 0.8)
        
      } else if (input$lim_variable == "lim_valorizables") {
        proxy %>% addPolygons(data = no_valorizables_lim, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, color = "green", group = "dynamicLayer")
        
      } else if (input$lim_variable == "lim_congestionamiento") {
        
        req(input$lim_congestionamiento_variable)
        
        # Selección del nivel de congestionamiento
        congestion_data <- switch(input$lim_congestionamiento_variable,
                                  "lim_1" = congestionamiento1_lim,
                                  "lim_2" = congestionamiento2_lim,
                                  "lim_3" = congestionamiento3_lim,
                                  "lim_4" = congestionamiento4_lim,
                                  "lim_5" = congestionamiento5_lim)
        color <- switch(input$lim_congestionamiento_variable,
                        "lim_1" = "blue", "lim_2" = "green", "lim_3" = "yellow",
                        "lim_4" = "#c45906", "lim_5" = "red")
        
        proxy %>% addPolygons(data = congestion_data, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, color = color, group = "dynamicLayer")
        
      } else if (input$lim_variable == "lim_accidentes") {
        proxy %>% addCircleMarkers(data = accidentes_lim, radius = 0.5, color = "blue",
                                   opacity = 0.1, group = "dynamicLayer")
        
      } else if (input$lim_variable == "lim_puentes") {
        proxy %>% addPolygons(data = puentes_lim, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, group = "dynamicLayer")
        
      } else if (input$lim_variable == "lim_RVN") {
        proxy %>% addPolygons(data = RVN_lim, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, group = "dynamicLayer")
      }
    })
  })
}

