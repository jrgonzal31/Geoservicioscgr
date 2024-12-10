scUI <- function(id) {
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
        radioButtons(ns("sc_variable"), "",
                     choices = list(
                       "Sin capa" = "sc_sin_capa",
                       "Edificaciones" = "sc_edificaciones",
                       "Infraestructura urbana" = "sc_infraestructura",
                       "Alumbrado público" = "sc_postes",
                       "Transformadores" = "sc_transformadores",
                       "Agua Potable" = "sc_Potable",
                       "Residuos valorizables" = "sc_valorizables",
                       "Residuos no valorizables" = "sc_novalorizables",
                       "Congestionamiento vial" = "sc_congestionamiento",
                       "Puentes" = "sc_puentes",
                       "Puntos de accidentes" = "sc_accidentes",
                       "Red vial nacional" = "sc_RVN"
                     ),
                     selected = "sc_sin_capa")
      ),
      
      # Subcategorías para "Infraestructura urbana"
      conditionalPanel(
        condition = sprintf("input['%s'] == 'sc_infraestructura'", ns("sc_variable")),
        box(
          width = 12,
          radioButtons(ns("sc_infraestructura_variable"), "Seleccione el tipo de infraestructura:",
                       choices = list(
                         "Comercio" = "sc_Comercio",
                         "Industria" = "sc_Industria",
                         "Servicios" = "sc_Servicios",
                         "Gobierno" = "sc_Gobierno",
                         "Educación" = "sc_Educacion",
                         "Universidades/institutos" = "sc_Universidades",
                         "Turísticas" = "sc_Turisticas",
                         "Recreativos" = "sc_Recreativos",
                         "Cultura" = "sc_Cultura",
                         "Religioso" = "sc_Religioso",
                         "Centros de cuido" = "sc_Centros_de_cuido",
                         "Salud" = "sc_Salud"
                       ))
        )
      ),
      
      # Subcategorías para "Congestionamiento vial"
      conditionalPanel(
        condition = sprintf("input['%s'] == 'sc_congestionamiento'", ns("sc_variable")),
        box(
          width = 12,
          radioButtons(ns("sc_congestionamiento_variable"), "Seleccione el nivel de congestionamiento:",
                       choices = list(
                         "Congestionamiento muy bajo" = "sc_1",
                         "Congestionamiento bajo" = "sc_2",
                         "Congestionamiento medio" = "sc_3",
                         "Congestionamiento alto" = "sc_4",
                         "Congestionamiento muy alto" = "sc_5"
                       ))
        )
      )
    ),
    
    # Mapa
    column(
      width = 8,
      withSpinner(leafletOutput(ns("sc_mapDisplay"), height = "800px"), type = 4, color = "#384a99", color.background = "white")
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
      img(src = "cl_sc.png", height = "200px", style = "display: block; margin: auto;")
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