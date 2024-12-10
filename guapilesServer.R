# guapiles_module.R

# Server del módulo para Guápiles
guapilesServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Carga de datos específicos de Guápiles
    UGM_escala_guap <- readRDS("datos/Pococi/datos/UGM_escala_guap.rds")
    pal_guap <- colorFactor(palette = c("#f39325" ,"#1270b8", "#63957b"), domain = UGM_escala_guap$cluster)
    
    Agua_guap <- Agua_guap %>%
      filter(POTABLE == POTABLE)
    
    output$guap_mapDisplay <- renderLeaflet({
      generate_map(UGM_guap, pal_guap)
    })
    
    observe({
      req(input$guap_variable)
      proxy <- leafletProxy("guap_mapDisplay", session)
      proxy %>% clearGroup("dynamicLayer") %>% clearMarkers()
      
      # Condiciones según la variable seleccionada en guap_variable
      if (input$guap_variable == "guap_infraestructura") {
        
        req(input$guap_infraestructura_variable)
        
        # Diccionario de categorías de infraestructura
        categoria_id_guap <- c(
          "guap_Comercio" = 1, "guap_Industria" = 2, "guap_Servicios" = 3, "guap_Gobierno" = 4,
          "guap_Puertos_aeropuertos" = 5, "guap_Educacion" = 6, "guap_Universidades" = 7,
          "guap_Turisticas" = 8, "guap_Recreativos" = 9, "guap_Ciclovia" = 10, "guap_Cultura" = 11,
          "guap_Religioso" = 12, "guap_Centros_de_cuido" = 13, "guap_Salud" = 14
        )
        
        # Filtrar infraestructura
        categoria_seleccionada_guap <- categoria_id_guap[input$guap_infraestructura_variable]
        infra_filtrada_guap <- infraestructura_guap %>% filter(id == categoria_seleccionada_guap)
        
        # Añadir la capa de infraestructura urbana al mapa
        if (nrow(infra_filtrada_guap) > 0) {
          if (all(st_geometry_type(infra_filtrada_guap) == "POINT")) {
            proxy %>% addMarkers(data = infra_filtrada_guap, label = ~id, group = "dynamicLayer")
          } else {
            proxy %>% addPolygons(data = infra_filtrada_guap, fillColor = "#FF6600", fillOpacity = 0.5,
                                  weight = 1, color = "black", group = "dynamicLayer",
                                  highlightOptions = highlightOptions(color = "#FF6600", weight = 2, bringToFront = TRUE, opacity = 1))
          }
        }
        
      } else if (input$guap_variable == "guap_edificaciones") {
        proxy %>% addPolygons(data = edificaciones_guap, fillColor = "#666666", fillOpacity = 0.5,
                              weight = 1, color = "black", group = "dynamicLayer")
        
      } else if (input$guap_variable == "guap_inundacion") {
        proxy %>% addPolygons(data = inundaciones_guap, stroke = FALSE, fillColor = "#3e499a",
                              fillOpacity = 0.5, group = "dynamicLayer")
        
      } else if (input$guap_variable == "guap_asentamientos") {
        proxy %>% addPolygons(data = asentamientos_guap, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, color = "darkblue", group = "dynamicLayer")
        
      } else if (input$guap_variable == "guap_medidores") {
        proxy %>% addCircleMarkers(data = medidores_guap, radius = 2, color = "blue", opacity = 0.8)
        
      } else if (input$guap_variable == "guap_postes") {
        proxy %>% addCircleMarkers(data = postes_guap, radius = 2, color = "green", opacity = 0.8)
        
      } else if (input$guap_variable == "guap_transformadores") {
        proxy %>% addCircleMarkers(data = transformadores_guap, radius = 2, color = "red", opacity = 0.8)
        
      } else if (input$guap_variable == "guap_Potable") {
        proxy %>% addCircleMarkers(data = Agua_guap, radius = 2, color = "blue", opacity = 0.8)
        
      } else if (input$guap_variable == "guap_valorizables") {
        proxy %>% addPolygons(data = no_valorizables_guap, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, color = "green", group = "dynamicLayer")
        
      } else if (input$guap_variable == "guap_novalorizables") {
        proxy %>% addPolygons(data = no_valorizables_guap, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, color = "blue", group = "dynamicLayer")
        
      } else if (input$guap_variable == "guap_congestionamiento") {
        
        req(input$guap_congestionamiento_variable)
        
        # Selección del nivel de congestionamiento
        congestion_data <- switch(input$guap_congestionamiento_variable,
                                  "guap_1" = congestionamiento1_guap,
                                  "guap_2" = congestionamiento2_guap,
                                  "guap_3" = congestionamiento3_guap,
                                  "guap_4" = congestionamiento4_guap,
                                  "guap_5" = congestionamiento5_guap)
        color <- switch(input$guap_congestionamiento_variable,
                        "guap_1" = "blue", "guap_2" = "green", "guap_3" = "yellow",
                        "guap_4" = "#c45906", "guap_5" = "red")
        
        proxy %>% addPolygons(data = congestion_data, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, color = color, group = "dynamicLayer")
        
      } else if (input$guap_variable == "guap_accidentes") {
        proxy %>% addPolygons(data = accidentes_guap, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, group = "dynamicLayer")
        
      } else if (input$guap_variable == "guap_puentes") {
        proxy %>% addPolygons(data = puentes_guap, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, group = "dynamicLayer")
        
      } else if (input$guap_variable == "guap_RVN") {
        proxy %>% addPolygons(data = RVN_guap, stroke = FALSE, fillOpacity = 0.8,
                              smoothFactor = 0.5, group = "dynamicLayer")
      }
    })
  })
}
