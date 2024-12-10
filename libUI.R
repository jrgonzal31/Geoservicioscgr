# liberia_module.R
liberiaUI <- function(id) {
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
        radioButtons(ns("lib_variable"), "",
                     choices = list(
                       "Sin capa" = "lib_sin_capa",
                       "Edificaciones" = "lib_edificaciones",
                       "Vulnerable a inundación" = "lib_inundacion",
                       "Asentamientos informales" = "lib_asentamientos",
                       "Infraestructura urbana" = "lib_infraestructura",
                       "Medidores inteligentes de electricidad" = "lib_medidores",
                       "Alumbrado público" = "lib_postes",
                       "Transformadores" = "lib_transformadores",
                       "Agua Potable" = "lib_Potable",
                       "Saneamiento de aguas residuales" = "lib_saneamiento",
                       "Residuos valorizables" = "lib_valorizables",
                       "Residuos no valorizables" = "lib_novalorizables",
                       "Congestionamiento vial" = "lib_congestionamiento",
                       "Puentes" = "lib_puentes",
                       "Puntos de accidentes" = "lib_accidentes",
                       "Red vial nacional" = "lib_RVN"
                     ),
                     selected = "lib_sin_capa")
      ),
      
      # Subcategorías para "Infraestructura urbana"
      conditionalPanel(
        condition = sprintf("input['%s'] == 'lib_infraestructura'", ns("lib_variable")),
        box(
          width = 12,
          radioButtons(ns("lib_infraestructura_variable"), "Seleccione el tipo de infraestructura:",
                       choices = list(
                         "Comercio" = "lib_Comercio",
                         "Servicios" = "lib_Servicios",
                         "Gobierno" = "lib_Gobierno",
                         "Educación" = "lib_Educacion",
                         "Universidades/institutos" = "lib_Universidades",
                         "Turísticas" = "lib_Turisticas",
                         "Recreativos" = "lib_Recreativos",
                         "Cultura" = "lib_Cultura",
                         "Religioso" = "lib_Religioso",
                         "Centros de cuido" = "lib_Centros_de_cuido",
                         "Salud" = "lib_Salud"
                       ))
        )
      ),
      
      # Subcategorías para "Congestionamiento vial"
      conditionalPanel(
        condition = sprintf("input['%s'] == 'lib_congestionamiento'", ns("lib_variable")),
        box(
          width = 12,
          radioButtons(ns("lib_congestionamiento_variable"), "Seleccione el nivel de congestionamiento:",
                       choices = list(
                         "Congestionamiento muy bajo" = "lib_1",
                         "Congestionamiento bajo" = "lib_2",
                         "Congestionamiento medio" = "lib_3",
                         "Congestionamiento alto" = "lib_4",
                         "Congestionamiento muy alto" = "lib_5"
                       ))
        )
      )
    ),
    
    # Mapa
    column(
      width = 8,
      withSpinner(leafletOutput(ns("lib_mapDisplay"), height = "800px"), type = 4, color = "#384a99", color.background = "white")
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
      img(src = "cl_lib.png", height = "300px", style = "display: block; margin: auto;")
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