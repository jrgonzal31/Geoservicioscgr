# guapiles_module.R
guapilesUI <- function(id) {
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
        radioButtons(ns("guap_variable"), "",
                     choices = list(
                       "Sin capa" = "guap_sin_capa",
                       "Edificaciones" = "guap_edificaciones",
                       "Vulnerable a inundación" = "guap_inundacion",
                       "Asentamientos informales" = "guap_asentamientos",
                       "Infraestructura urbana" = "guap_infraestructura",
                       "Medidores inteligentes de electricidad" = "guap_medidores",
                       "Alumbrado público" = "guap_postes",
                       "Transformadores" = "guap_transformadores",
                       "Agua Potable" = "guap_Potable",
                       "Residuos valorizables" = "guap_valorizables",
                       "Residuos no valorizables" = "guap_novalorizables",
                       "Congestionamiento vial" = "guap_congestionamiento",
                       "Puentes" = "guap_puentes",
                       "Puntos de accidentes" = "guap_accidentes",
                       "Red vial nacional" = "guap_RVN"
                     ),
                     selected = "guap_sin_capa")
      ),
      
      # Subcategorías para "Infraestructura urbana"
      conditionalPanel(
        condition = sprintf("input['%s'] == 'guap_infraestructura'", ns("guap_variable")),
        box(
          width = 12,
          radioButtons(ns("guap_infraestructura_variable"), "Seleccione el tipo de infraestructura:",
                       choices = list(
                         "Comercio" = "guap_Comercio",
                         "Industria" = "guap_Industria",
                         "Servicios" = "guap_Servicios",
                         "Gobierno" = "guap_Gobierno",
                         "Puertos y Aeropuertos" = "guap_Puertos_aeropuertos",
                         "Educación" = "guap_Educacion",
                         "Universidades/institutos" = "guap_Universidades",
                         "Turísticas" = "guap_Turisticas",
                         "Recreativos" = "guap_Recreativos",
                         "Cultura" = "guap_Cultura",
                         "Religioso" = "guap_Religioso",
                         "Centros de cuido" = "guap_Centros_de_cuido",
                         "Salud" = "guap_Salud"
                       ))
        )
      ),
      
      # Subcategorías para "Congestionamiento vial"
      conditionalPanel(
        condition = sprintf("input['%s'] == 'guap_congestionamiento'", ns("guap_variable")),
        box(
          width = 12,
          radioButtons(ns("guap_congestionamiento_variable"), "Seleccione el nivel de congestionamiento:",
                       choices = list(
                         "Congestionamiento muy bajo" = "guap_1",
                         "Congestionamiento bajo" = "guap_2",
                         "Congestionamiento medio" = "guap_3",
                         "Congestionamiento alto" = "guap_4",
                         "Congestionamiento muy alto" = "guap_5"
                       ))
        )
      )
    ),
    
    # Mapa
    column(
      width = 8,
      withSpinner(leafletOutput(ns("guap_mapDisplay"), height = "800px"), type = 4, color = "#384a99", color.background = "white")
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
      img(src = "cl_guap.png", height = "300px", style = "display: block; margin: auto;")
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