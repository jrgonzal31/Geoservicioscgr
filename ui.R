libraries <- c("sf", "leaflet", "leaflet.extras", "leaflet.providers", 
               "dplyr", "readxl", "readr", "RColorBrewer", "htmltools", 
               "ggplot2", "purrr", "tidyverse", "factoextra", "clValid", 
               "shiny", "shinydashboard", "shinythemes", "profvis", 
               "memoise", "future", "promises", "shinydashboard", "fresh", "redux", "shinycssloaders")

install_and_load <- function(libs) {
  new_libs <- libs[!(libs %in% installed.packages()[, "Package"])]
  if (length(new_libs)) {
    install.packages(new_libs, dependencies = TRUE, repos = "https://cran.rstudio.com/")
  }
  
  sapply(libs, require, character.only = TRUE)
}

install_and_load(libraries)

my_theme <- create_theme(
  adminlte_color(
    light_blue = "#384A99",
    yellow = "gray",
    gray = "white"
  ),
  adminlte_sidebar(
    dark_bg = "#363636",
    dark_hover_bg = "#384A99",
    dark_color = "white"
  ),
  adminlte_global(
    content_bg = "#ffffff",
    box_bg = "#f7f7f7",
    info_box_bg = "#384A99"
  )
)

source("guapilesUI.R")
source("pzUI.R")
source("libUI.R")
source("puntarenasUI.R")
source("limonUI.R")
source("turriUI.R")
source("scUI.R")


ui <- dashboardPage(
  dashboardHeader(
    title = "Geovisor CGR",
    tags$li(
      class = "dropdown",
      tags$div(
        tags$img(src = "logo.png", height = "50px", style = "margin-right: 10px;")
      )
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Inicio", tabName = "inicio", icon = icon("bars")),
      menuItem("Guápiles", tabName = "guapiles_analysis", icon = icon("map-marker-alt")),
      menuItem("Liberia", tabName = "liberia_analysis", icon = icon("map-marker-alt")),
      menuItem("Limón", tabName = "limon_analysis", icon = icon("map-marker-alt")),
      menuItem("Pérez Zeledón", tabName = "pz_analysis", icon = icon("map-marker-alt")),
      menuItem("Puntarenas", tabName = "puntarenas_analysis", icon = icon("map-marker-alt")),
      menuItem("San Carlos", tabName = "sancarlos_analysis", icon = icon("map-marker-alt")),
      menuItem("Turrialba", tabName = "turrialba_analysis", icon = icon("map-marker-alt"))
    )
  ),
  dashboardBody(
    use_theme(my_theme),
    tags$head(
      tags$title("Ciudades intermedias"),
      tags$style(HTML("
        body, .content-wrapper, .main-sidebar, .main-header, .modal-content, 
        .tab-content, .nav-tabs-custom, .skin-blue .main-header .navbar {
          font-family: 'Century Gothic', sans-serif;
        }
      "))
    ),
    tabItems(
      tabItem(
        tabName = "inicio",
        tags$br(),
        tags$br(),
        fluidRow(
          column(
            width = 11,
            tags$p(
              style = "font-size: 18px;",
              "En el Geovisor CGR encontrará información sobre la cobertura de servicios básicos (agua potable, saneamiento de aguas residuales, recolección de residuos, movilidad, electricidad y telecomunicaciones) en las ciudades intermedias."
            )
          )
        ),
        fluidRow(
          column(
            width = 11,
            tags$p(
              style = "font-size: 18px;",
              "Las ciudades intermedias son aquellas que son cabeza de cantón y tienen una función regional intermediaria entre la GAM y otras ciudades vecinas, así como una población superior a 22.000 hasta 69.800 habitantes y cuentan con servicios de salud y educación. En el presente estudio se estudiaron siete de estas ciudades intermedias ubicadas en diversos puntos del territorio nacional:"
            ),
            tags$ul(
              tags$li(style = "font-size: 18px;", "Guápiles"),
              tags$li(style = "font-size: 18px;", "Liberia"),
              tags$li(style = "font-size: 18px;", "Limón"),
              tags$li(style = "font-size: 18px;", "Pérez Zeledón"),
              tags$li(style = "font-size: 18px;", "Puntarenas"),
              tags$li(style = "font-size: 18px;", "San Carlos"),
              tags$li(style = "font-size: 18px;", "Turrialba")
            ),
            tags$p(
              style = "font-size: 18px;",
              "En términos generales, estas ciudades son clave para el desarrollo de una región ya que brindan servicios y empleo a las zonas o cantones menos desarrollados a su alrededor. Por otro lado, conectan a la oferta y demanda de los bienes y servicios de las grandes urbes con las economías locales y rurales."
            )
          )
        ),
        tags$br(),
        tags$br(),
        fluidRow(
          column(
            width = 3,
            tags$a(
              href = "informe.pdf",
              download = "informe.pdf",
              tags$img(src = "image.png", height = "300px")
            )
          ),
          column(
            width = 3,
            tags$a(
              href = "Reporte CI Guápiles.pdf",
              download = "Reporte CI Guápiles.pdf",
              tags$img(src = "portadaguapiles1.png", height = "300px")
            )
          ),
          column(
            width = 3,
            tags$a(
              href = "Reporte CI Liberia.pdf",
              download = "Reporte CI Liberia.pdf",
              tags$img(src = "portadaliberia1.png", height = "300px")
            )
          ),
          column(
            width = 3,
            tags$a(
              href = "Reporte CI Limón.pdf",
              download = "Reporte CI Limón.pdf",
              tags$img(src = "portadalimon1.png", height = "300px")
            )
          )
        ),
        tags$br(),
        fluidRow(
          column(
            width = 3,
            tags$a(
              href = "Reporte CI Pérez Zeledón.pdf",
              download = "Reporte CI Pérez Zeledón.pdf",
              tags$img(src = "portadapz1.png", height = "300px")
            )
          ),
          column(
            width = 3,
            tags$a(
              href = "Reporte CI Puntarenas.pdf",
              download = "Reporte CI Puntarenas.pdf",
              tags$img(src = "portadapuntarenas1.png", height = "300px")
            )
          ),
          column(
            width = 3,
            tags$a(
              href = "Reporte CI Ciudad Quesada.pdf",
              download = "Reporte CI Ciudad Quesada.pdf",
              tags$img(src = "portadasancarlos1.png", height = "300px")
            )
          ),
          column(
            width = 3,
            tags$a(
              href = "Reporte CI Turrialba.pdf",
              download = "Reporte CI Turrialba.pdf",
              tags$img(src = "portadaturri1.png", height = "300px")
            )
          )
        )
      ),
      tabItem(tabName = "guapiles_analysis", guapilesUI("guapiles")),
      tabItem(tabName = "liberia_analysis", liberiaUI("liberia")),
      tabItem(tabName = "limon_analysis", limonUI("limon")),
      tabItem(tabName = "pz_analysis", pzUI("pz")),
      tabItem(tabName = "puntarenas_analysis", puntarenasUI("puntarenas")),
      tabItem(tabName = "turrialba_analysis", turrialbaUI("turrialba")),
      tabItem(tabName = "sancarlos_analysis", scUI("sc"))
  )
)
)