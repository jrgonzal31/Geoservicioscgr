libraries <- c("sf", "leaflet", "leaflet.extras", "leaflet.providers", 
                "dplyr", "readxl", "janitor", "readr", "osmdata", 
                "RColorBrewer", "htmltools", "stringr", "ggplot2", "purrr",
                "spData", "tidyverse","scales", "plotly", "cluster","fpc", 
                "NbClust","clValid", "stats", "corrplot", "factoextra", "clValid", 
                "fmsb", "ineq", "shiny", "shinydashboard", "shinythemes", "profvis", 
                "memoise", "future", "promises", "fresh", "redux", "shinycssloaders", "rmarkdown")


install_and_load <- function(libs) {
  new_libs <- libs[!(libs %in% installed.packages()[, "Package"])]
  if (length(new_libs)) {
    install.packages(new_libs, dependencies = TRUE, repos = "https://cran.rstudio.com/")
  }
  
  sapply(libs, require, character.only = TRUE)
}

install_and_load(libraries)

totalCores <- availableCores() - 1  # Reserva un núcleo para el sistema
plan(multisession, workers = totalCores)
options(future.globals.maxSize = 8 * 1024^3)  # Aumenta el límite a 8 GB

source("global.R")


server <- (function(input, output, session) {
  
 
  # PÉREZ ZELEDÓN

    observeEvent(input$tabs == "pz_analysis", {
    source("pzServer.R", local = TRUE)
    pzServer("pz")
  }, ignoreInit = TRUE)
  
  # PUNTARENAS
  observeEvent(input$tabs == "puntarenas_analysis", {
    source("puntarenasServer.R", local = TRUE)
    puntarenasServer("puntarenas")
  }, ignoreInit = TRUE)
  
  # GUÁPILES
  observeEvent(input$tabs == "guapiles_analysis", {
    source("guapilesServer.R", local = TRUE)
    guapilesServer("guapiles")
  }, ignoreInit = TRUE)
  
  # LIBERIA
  observeEvent(input$tabs == "liberia_analysis", {
    source("libServer.R", local = TRUE)
    liberiaServer("liberia")
  }, ignoreInit = TRUE)
  
  # LIMÓN
  observeEvent(input$tabs == "limon_analysis", {
    source("limonServer.R", local = TRUE)
    limonServer("limon")
  }, ignoreInit = TRUE)
  
  # TURRIALBA
  observeEvent(input$tabs == "turrialba_analysis", {
    source("turriServer.R", local = TRUE)
    turrialbaServer("turrialba")
  }, ignoreInit = TRUE)
  
  # SAN CARLOS
  observeEvent(input$tabs == "sancarlos_analysis", {
    source("scServer.R", local = TRUE)
    scServer("sc")
  }, ignoreInit = TRUE)

})



#runApp( "app.R", host = "localhost", port = 4824, launch.browser = FALSE, display.mode = "fullscreen" ) #, port = 7704 , host = ip

#runApp(list(ui=ui, server=server),  host = getOption("shiny.host", "127.0.0.1"), port = 5055,launch.browser = TRUE)