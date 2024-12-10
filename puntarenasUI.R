puntarenasUI <- function(id) {
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
        radioButtons(ns("punt_variable"), "",
                     choices = list(
                       "Sin capa" = "punt_sin_capa",
                       "Edificaciones" = "punt_edificaciones",
                       "Vulnerable a inundación" = "punt_inundacion",
                       "Infraestructura urbana" = "punt_infraestructura",
                       "Medidores inteligentes de electricidad" = "punt_medidores",
                       "Alumbrado público" = "punt_postes",
                       "Transformadores" = "punt_transformadores",
                       "Agua Potable" = "punt_Potable",
                       "Saneamiento de aguas residuales" = "punt_saneamiento",
                       "Congestionamiento vial" = "punt_congestionamiento",
                       "Puentes" = "punt_puentes",
                       "Puntos de accidentes" = "punt_accidentes",
                       "Red vial nacional" = "punt_RVN"
                     ),
                     selected = "punt_sin_capa")
      ),
      
      # Subcategorías para "Infraestructura urbana"
      conditionalPanel(
        condition = sprintf("input['%s'] == 'punt_infraestructura'", ns("punt_variable")),
        box(
          width = 12,
          radioButtons(ns("punt_infraestructura_variable"), "Seleccione el tipo de infraestructura:",
                       choices = list(
                         "Comercio" = "punt_Comercio",
                         "Industria" = "punt_Industria",
                         "Servicios" = "punt_Servicios",
                         "Gobierno" = "punt_Gobierno",
                         "Puertos y Aeropuertos" = "punt_Puertos_aeropuertos",
                         "Educación" = "punt_Educacion",
                         "Universidades/institutos" = "punt_Universidades",
                         "Turísticas" = "punt_Turisticas",
                         "Recreativos" = "punt_Recreativos",
                         "Cultura" = "punt_Cultura",
                         "Religioso" = "punt_Religioso",
                         "Centros de cuido" = "punt_Centros_de_cuido",
                         "Salud" = "punt_Salud"
                       ))
        )
      ),
      
      # Subcategorías para "Congestionamiento vial"
      conditionalPanel(
        condition = sprintf("input['%s'] == 'punt_congestionamiento'", ns("punt_variable")),
        box(
          width = 12,
          radioButtons(ns("punt_congestionamiento_variable"), "Seleccione el nivel de congestionamiento:",
                       choices = list(
                         "Congestionamiento muy bajo" = "punt_1",
                         "Congestionamiento bajo" = "punt_2",
                         "Congestionamiento medio" = "punt_3",
                         "Congestionamiento alto" = "punt_4",
                         "Congestionamiento muy alto" = "punt_5"
                       ))
        )
      )
    ),
    
    # Mapa
    column(
      width = 8,
      withSpinner(leafletOutput(ns("punt_mapDisplay"), height = "800px"), type = 4, color = "#384a99", color.background = "white")
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
      img(src = "cl_punt.png", height = "300px", style = "display: block; margin: auto;")
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