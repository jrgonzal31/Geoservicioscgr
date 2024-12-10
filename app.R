setwd("C:/Users/jose.gonzalez/Documents/PerezZeledon/Analisis")

source("server.R")
source("ui.R")


shiny::runApp(".", host = "0.0.0.0", port = 1001)
