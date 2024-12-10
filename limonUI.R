# limon_module.R
limonUI <- function(id) {
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
        radioButtons(ns("lim_variable"), "",
                     choices = list(
                       "Sin capa" = "lim_sin_capa",
                       "Edificaciones" = "lim_edificaciones",
                       "Vulnerable a inundación" = "lim_inundacion",
                       "Infraestructura urbana" = "lim_infraestructura",
                       "Medidores inteligentes de electricidad" = "lim_medidores",
                       "Alumbrado público" = "lim_postes",
                       "Transformadores" = "lim_transformadores",
                       "Agua Potable" = "lim_Potable",
                       "Saneamiento de aguas residuales" = "lim_saneamiento",
                       "Residuos valorizables" = "lim_valorizables",
                       "Congestionamiento vial" = "lim_congestionamiento",
                       "Puentes" = "lim_puentes",
                       "Puntos de accidentes" = "lim_accidentes",
                       "Red vial nacional" = "lim_RVN"
                     ),
                     selected = "lim_sin_capa")
      ),
      
      # Subcategorías para "Infraestructura urbana"
      conditionalPanel(
        condition = sprintf("input['%s'] == 'lim_infraestructura'", ns("lim_variable")),
        box(
          width = 12,
          radioButtons(ns("lim_infraestructura_variable"), "Seleccione el tipo de infraestructura:",
                       choices = list(
                         "Comercio" = "lim_Comercio",
                         "Servicios" = "lim_Servicios",
                         "Gobierno" = "lim_Gobierno",
                         "Puertos y Aeropuertos" = "lim_Puertos_aeropuertos",
                         "Educación" = "lim_Educacion",
                         "Universidades/institutos" = "lim_Universidades",
                         "Turísticas" = "lim_Turisticas",
                         "Recreativos" = "lim_Recreativos",
                         "Cultura" = "lim_Cultura",
                         "Religioso" = "lim_Religioso",
                         "Centros de cuido" = "lim_Centros_de_cuido",
                         "Salud" = "lim_Salud"
                       ))
        )
      ),
      
      # Subcategorías para "Congestionamiento vial"
      conditionalPanel(
        condition = sprintf("input['%s'] == 'lim_congestionamiento'", ns("lim_variable")),
        box(
          width = 12,
          radioButtons(ns("lim_congestionamiento_variable"), "Seleccione el nivel de congestionamiento:",
                       choices = list(
                         "Congestionamiento muy bajo" = "lim_1",
                         "Congestionamiento bajo" = "lim_2",
                         "Congestionamiento medio" = "lim_3",
                         "Congestionamiento alto" = "lim_4",
                         "Congestionamiento muy alto" = "lim_5"
                       ))
        )
      )
    ),
    
    # Mapa
    column(
      width = 8,
      withSpinner(leafletOutput(ns("lim_mapDisplay"), height = "800px"), type = 4, color = "#384a99", color.background = "white")
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
      img(src = "cl_lim.png", height = "200px", style = "display: block; margin: auto;")
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