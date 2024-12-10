turrialbaUI <- function(id) {
  ns <- NS(id)  # Namespace para evitar conflictos de identificadores
  
  tagList(
   fluidRow(
    column(
      width = 3,  # Especificamos el ancho de la primera columna
      
      # Panel de selección de variables principales
      box(
        width = 12, 
        title = "Seleccione la variable para visualizar en el mapa",
        
        # Radio buttons principales
        radioButtons(ns("turri_variable"), "",
                     choices = list(
                       "Sin capa" = "turri_sin_capa",
                       "Edificaciones" = "turri_edificaciones",
                       "Vulnerable a inundación" = "turri_inundacion",
                       "Infraestructura urbana" = "turri_infraestructura",
                       "Medidores inteligentes de electricidad" = "turri_medidores",
                       "Alumbrado público" = "turri_postes",
                       "Transformadores" = "turri_transformadores",
                       "Residuos valorizables" = "turri_valorizables",
                       "Residuos no valorizables" = "turri_novalorizables",
                       "Congestionamiento vial" = "turri_congestionamiento",
                       "Puentes" = "turri_puentes",
                       "Puntos de accidentes" = "turri_accidentes",
                       "Red vial nacional" = "turri_RVN"
                     ),
                     selected = "turri_sin_capa")
      ),
      
      # Subcategorías para "Infraestructura urbana"
      conditionalPanel(
        condition = sprintf("input['%s'] == 'turri_infraestructura'", ns("turri_variable")),
        box(
          width = 12,
          radioButtons(ns("turri_infraestructura_variable"), "Seleccione el tipo de infraestructura:",
                       choices = list(
                         "Comercio" = "turri_Comercio",
                         "Industria" = "turri_Industria",
                         "Servicios" = "turri_Servicios",
                         "Gobierno" = "turri_Gobierno",
                         "Educación" = "turri_Educacion",
                         "Universidades/institutos" = "turri_Universidades",
                         "Recreativos" = "turri_Recreativos",
                         "Cultura" = "turri_Cultura",
                         "Religioso" = "turri_Religioso",
                         "Centros de cuido" = "turri_Centros_de_cuido"
                       ))
        )
      ),
      
      # Subcategorías para "Congestionamiento vial"
      conditionalPanel(
        condition = sprintf("input['%s'] == 'turri_congestionamiento'", ns("turri_variable")),
        box(
          width = 12,
          radioButtons(ns("turri_congestionamiento_variable"), "Seleccione el nivel de congestionamiento:",
                       choices = list(
                         "Congestionamiento muy bajo" = "turri_1",
                         "Congestionamiento bajo" = "turri_2",
                         "Congestionamiento medio" = "turri_3",
                         "Congestionamiento alto" = "turri_4",
                         "Congestionamiento muy alto" = "turri_5"
                       ))
        )
      )
    ),
    
    # Mapa
    column(
      width = 8,
      withSpinner(leafletOutput(ns("turri_mapDisplay"), height = "800px"), type = 4, color = "#384a99", color.background = "white")
    )
   ),
   
   fluidRow(
     column(
       width = 12,
       tags$h4("Explicación de los clústers", 
               style = "color: #384A99; font-size: 24px; font-weight: bold; text-align: center;")
     )
   ),
   fluidRow(
     column(
       width = 4,  # Primera columna para la primera imagen
       img(src = "cl_turri.png", height = "200px", style = "display: block; margin: auto;")
     ),
     column(
       width = 4,  # Segunda columna para la segunda imagen
       img(src = "cl_pz1.png", height = "300px", style = "display: block; margin: auto;")
     ),
     column(
       width = 1,  # Columna vacía para espacio entre las imágenes y el texto
       ""
     ),
     column(
       width = 3,  # Tercera columna para el texto explicativo
       p("Esta imagen muestra los clústers identificados en la región según el análisis realizado. Cada color representa un grupo de características homogéneas, basado en variables seleccionadas como infraestructura, accesibilidad, y condiciones socioeconómicas.", 
         style = "font-size: 18px; text-align: justify;")
     )
   )
  )
}