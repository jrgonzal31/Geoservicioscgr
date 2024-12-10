pzUI <- function(id) {
  ns <- NS(id)  # Namespace para evitar conflictos de identificadores
  
  tagList( # Cambiar fluidRow a tagList para poder incluir varios elementos de diseño
    fluidRow(
      column(
        width = 3,  # Especificamos el ancho de la primera columna
        
        # Panel de selección de variables principales
        box(
          width = 12, 
          title = "Seleccione la variable para visualizar en el mapa",
          
          # Radio buttons principales
          radioButtons(ns("pz_variable"), "",
                       choices = list(
                         "Sin capa" = "pz_sin_capa",
                         "Edificaciones" = "pz_edificaciones",
                         "Vulnerable a inundación" = "pz_inundacion",
                         "Infraestructura urbana" = "pz_infraestructura",
                         "Medidores inteligentes de electricidad" = "pz_medidores",
                         "Alumbrado público" = "pz_postes",
                         "Transformadores" = "pz_transformadores",
                         "Agua Potable" = "pz_Potable",
                         "Saneamiento de aguas residuales" = "pz_saneamiento",
                         "Residuos valorizables" = "pz_valorizables",
                         "Residuos no valorizables" = "pz_novalorizables",
                         "Congestionamiento vial" = "pz_congestionamiento",
                         "Puentes" = "pz_puentes",
                         "Puntos de accidentes" = "pz_accidentes",
                         "Red vial nacional" = "pz_RVN"
                       ),
                       selected = "pz_sin_capa")
        ),
        
        # Subcategorías para "Infraestructura urbana"
        conditionalPanel(
          condition = sprintf("input['%s'] == 'pz_infraestructura'", ns("pz_variable")),
          box(
            width = 12,
            radioButtons(ns("pz_infraestructura_variable"), "Seleccione el tipo de infraestructura:",
                         choices = list(
                           "Comercio" = "pz_Comercio",
                           "Industria" = "pz_Industria",
                           "Servicios" = "pz_Servicios",
                           "Gobierno" = "pz_Gobierno",
                           "Puertos y Aeropuertos" = "pz_Puertos_aeropuertos",
                           "Educación" = "pz_Educacion",
                           "Universidades/institutos" = "pz_Universidades",
                           "Turísticas" = "pz_Turisticas",
                           "Recreativos" = "pz_Recreativos",
                           "Cultura" = "pz_Cultura",
                           "Religioso" = "pz_Religioso",
                           "Centros de cuido" = "pz_Centros_de_cuido",
                           "Salud" = "pz_Salud"
                         ))
          )
        ),
        
        # Subcategorías para "Congestionamiento vial"
        conditionalPanel(
          condition = sprintf("input['%s'] == 'pz_congestionamiento'", ns("pz_variable")),
          box(
            width = 12,
            radioButtons(ns("pz_congestionamiento_variable"), "Seleccione el nivel de congestionamiento:",
                         choices = list(
                           "Congestionamiento muy bajo" = "pz_1",
                           "Congestionamiento bajo" = "pz_2",
                           "Congestionamiento medio" = "pz_3",
                           "Congestionamiento alto" = "pz_4",
                           "Congestionamiento muy alto" = "pz_5"
                         ))
          )
        )
      ),
      
      # Mapa
      column(
        width = 8,
        withSpinner(leafletOutput(ns("pz_mapDisplay"), height = "800px"), type = 4, color = "#384a99", color.background = "white")
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
        img(src = "cl_pz.png", height = "300px", style = "display: block; margin: auto;")
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
        p("La técnica de clústering jerárquico permite agrupar los segmentos de la población que tienen mayor similitud basado en variables sobre los servicios públicos y otros datos para caracterizar a la ciudad, tales como la existencia de asentamientos informales, zonas propensas a inundación o la presencia de infraestructura urbana.", 
          style = "font-size: 18px; text-align: justify;")
      )
    )
  )
}