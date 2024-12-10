# liberia_module.R
liberiaServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Cargar datos específicos de Pérez Zeledón
    UGM_escala_lib <- readRDS("datos/Liberia/datos/UGM_escala_lib.rds")
    pal_lib <- colorFactor(palette = c("#f39325", "#1270b8", "#63957b"), domain = UGM_escala_lib$cluster)
    
    
    Agua_lib <- Agua_lib %>%
      filter(Potable == Potable)
    
    saneamiento_lib <- Agua_lib %>%
      filter(DSC_SIST_S != "SIN SANEAMIENTO")
    
    # Mapa principal
    output$lib_mapDisplay <- renderLeaflet({
      generate_map(UGM_lib, pal_lib)
    })
    
    # Observador principal para actualizar el mapa según la variable seleccionada
    observe({
      req(input$lib_variable)
      proxy <- leafletProxy("lib_mapDisplay", session)
      proxy %>% clearGroup("dynamicLayer") %>% clearMarkers()
      
      # Condiciones según la variable seleccionada en lib_variable
      if (input$lib_variable == "lib_infraestructura") {
        
        req(input$lib_infraestructura_variable)
        
        # Diccionario de categorías de infraestructura
        categoria_id_lib <- c(
          "lib_Comercio" = 1, "lib_Industria" = 2, "lib_Servicios" = 3, "lib_Gobierno" = 4,
          "lib_Puertos_aeropuertos" = 5, "lib_Educacion" = 6, "lib_Universidades" = 7,
          "lib_Turisticas" = 8, "lib_Recreativos" = 9, "lib_Ciclovia" = 10, "lib_Cultura" = 11,
          "lib_Religioso" = 12, "lib_Centros_de_cuido" = 13, "lib_Salud" = 14
        )
        
        # Filtrar infraestructura
        categoria_seleccionada_lib <- categoria_id_lib[input$lib_infraestructura_variable]
        infra_filtrada_lib <- infraestructura_lib %>% filter(id == categoria_seleccionada_lib)
        
        # Añadir la capa de infraestructura urbana al mapa
        if (nrow(infra_filtrada_lib) > 0) {
          if (all(st_geometry_type(infra_filtrada_lib) == "POINT")) {
            proxy %>% addMarkers(data = infra_filtrada_lib, label = ~id, group = "dynamicLayer")
          } else {
            proxy %>% addPolygons(data = infra_filtrada_lib, fillColor = "#FF6600", fillOpacity = 0.5,
                                  weight = 1, color = "black", group = "dynamicLayer",
                                  highlightOptions = highlightOptions(color = "#FF6600", weight = 2, bringToFront = TRUE, opacity = 1))
          }
        }
        
      } else if (input$lib_variable == "lib_edificaciones") {
        proxy %>% addPolygons(data = edificaciones_lib, fillColor = "#666666", fillOpacity = 0.5,
                              weight = 1, color = "black", group = "dynamicLayer")
        
      } else if (input$lib_variable == "lib_inundacion") {
        proxy %>% addPolygons(data = inundaciones_lib, stroke = FALSE, fillColor = "#3e499a",
                              fillOpacity = 0.5, group = "dynamicLayer")
        
      } else if (input$lib_variable == "lib_medidores") {
        proxy %>% addCircleMarkers(data = medidores_lib, radius = 2, color = "blue", opacity = 0.8)
        
      } else if (input$lib_variable == "lib_postes") {
        proxy %>% addCircleMarkers(data = postes_lib, radius = 2, color = "green", opacity = 0.8)
        
      } else if (input$lib_variable == "lib_transformadores") {
        proxy %>% addCircleMarkers(data = transformadores_lib, radius = 2, color = "red", opacity = 0.8)
        
      } else if (input$lib_variable == "lib_Potable") {
        proxy %>% addCircleMarkers(data = Agua_lib, radius = 2, color = "blue", opacity = 0.8)
        
      } else if (input$lib_variable == "lib_saneamiento") {
        proxy %>% addCircleMarkers(data = saneamiento_lib, radius = 2, color = "green", opacity = 0.8)
        
      } else if (input$lib_variable == "lib_asentamientos") {
        proxy %>% addPolygons(data = asentamientos_lib, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, color = "darkblue", group = "dynamicLayer")
        
      } else if (input$lib_variable == "lib_valorizables") {
        proxy %>% addPolygons(data = valorizables_lib, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, color = "green", group = "dynamicLayer")
      } else if (input$lib_variable == "lib_novalorizables") {
        proxy %>% addPolygons(data = no_valorizables_lib, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, color = "blue", group = "dynamicLayer")
        
      } else if (input$lib_variable == "lib_congestionamiento") {
        
        req(input$lib_congestionamiento_variable)
        
        # Selección del nivel de congestionamiento
        congestion_data <- switch(input$lib_congestionamiento_variable,
                                  "lib_1" = congestionamiento1_lib,
                                  "lib_2" = congestionamiento2_lib,
                                  "lib_3" = congestionamiento3_lib,
                                  "lib_4" = congestionamiento4_lib,
                                  "lib_5" = congestionamiento5_lib)
        color <- switch(input$lib_congestionamiento_variable,
                        "lib_1" = "blue", "lib_2" = "green", "lib_3" = "yellow",
                        "lib_4" = "#c45906", "lib_5" = "red")
        
        proxy %>% addPolygons(data = congestion_data, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, color = color, group = "dynamicLayer")
        
      } else if (input$lib_variable == "lib_accidentes") {
        proxy %>% addPolygons(data = accidentes_lib, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, group = "dynamicLayer")
        
      } else if (input$lib_variable == "lib_puentes") {
        proxy %>% addPolygons(data = puentes_lib, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, group = "dynamicLayer")
        
      } else if (input$lib_variable == "lib_RVN") {
        proxy %>% addPolygons(data = RVN_lib, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, group = "dynamicLayer")
      }
    })
  })
}


