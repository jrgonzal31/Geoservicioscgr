# global.R
libraries <- c("sf", "leaflet", "leaflet.extras", "leaflet.providers", 
               "dplyr", "readxl", "janitor", "readr", "osmdata", 
               "RColorBrewer", "htmltools", "stringr", "ggplot2", "purrr",
               "spData", "tidyverse","scales", "plotly", "cluster","fpc", 
               "NbClust","clValid", "stats", "corrplot", "factoextra", "clValid", 
               "fmsb", "ineq", "shiny", "shinydashboard", "shinythemes", "profvis", 
               "memoise", "future", "promises", "fresh", "redux")

install_and_load <- function(libs) {
  new_libs <- libs[!(libs %in% installed.packages()[, "Package"])]
  if (length(new_libs)) {
    install.packages(new_libs, dependencies = TRUE, repos = "https://cran.rstudio.com/")
  }
  
  sapply(libs, require, character.only = TRUE)
}

install_and_load(libraries)



# Conectar a Redis
redis_conn <- hiredis(host = "10.100.1.98", port = 6379)

# Función memoizada para cachear la carga de datos desde Redis
cargar_desde_redis <- memoise(function(key) {
  unserialize(redis_conn$GET(key))
})

# Carga las bases de datos utilizando la función memoizada
UGM_pz <- cargar_desde_redis("UGM_pz")
inundaciones_pz <- cargar_desde_redis("inundaciones_pz")
infraestructura_pz <- cargar_desde_redis("infraestructura_pz")
medidores_pz <- cargar_desde_redis("medidores_pz") %>% st_centroid ()
transformadores_pz <- cargar_desde_redis("transformadores_pz") %>% st_centroid () 
postes_pz <- cargar_desde_redis("postes_pz") %>% st_centroid () 
Agua_pz <- cargar_desde_redis("Agua_pz") %>% st_centroid () 
no_valorizables_pz <- cargar_desde_redis("no_valorizables_pz")
congestionamiento_pz <- cargar_desde_redis("congestionamiento_pz")
accidentes_pz <- cargar_desde_redis("accidentes_pz") %>% st_centroid () 
RVN_pz <- cargar_desde_redis("RVN_pz")
puentes_pz <- cargar_desde_redis("puentes_pz")
UGM_punt <- cargar_desde_redis("UGM_punt")
inundaciones_punt <- cargar_desde_redis("inundaciones_punt")
infraestructura_punt <- cargar_desde_redis("infraestructura_punt")
asentamientos_punt <- cargar_desde_redis("asentamientos_punt")
medidores_punt <- cargar_desde_redis("medidores_punt") %>% st_centroid () 
transformadores_punt <- cargar_desde_redis("transformadores_punt") %>% st_centroid () 
postes_punt <- cargar_desde_redis("postes_punt") %>% st_centroid () 
Agua_punt <- cargar_desde_redis("Agua_punt") %>% st_centroid () 
congestionamiento_punt <- cargar_desde_redis("congestionamiento_punt")
accidentes_punt <- cargar_desde_redis("accidentes_punt") %>% st_centroid () 
RVN_punt <- cargar_desde_redis("RVN_punt")
puentes_punt <- cargar_desde_redis("puentes_punt")
edificaciones_pz <- cargar_desde_redis("edificaciones_pz")
edificaciones_turri <- cargar_desde_redis("edificaciones_turri")
edificaciones_lim <- cargar_desde_redis("edificaciones_lim")
edificaciones_lib <- cargar_desde_redis("edificaciones_lib")
edificaciones_punt <- cargar_desde_redis("edificaciones_punt")
edificaciones_sc <- cargar_desde_redis("edificaciones_sc")
congestionamiento1_pz <- cargar_desde_redis("congestionamiento1_pz")
congestionamiento2_pz <- cargar_desde_redis("congestionamiento2_pz")
congestionamiento3_pz <- cargar_desde_redis("congestionamiento3_pz")
congestionamiento4_pz <- cargar_desde_redis("congestionamiento4_pz")
congestionamiento5_pz <- cargar_desde_redis("congestionamiento5_pz")
congestionamiento1_punt <- cargar_desde_redis("congestionamiento1_punt")
congestionamiento2_punt <- cargar_desde_redis("congestionamiento2_punt")
congestionamiento3_punt <- cargar_desde_redis("congestionamiento3_punt")
congestionamiento4_punt <- cargar_desde_redis("congestionamiento4_punt")
congestionamiento5_punt <- cargar_desde_redis("congestionamiento5_punt")
UGM_turri <- cargar_desde_redis("UGM_turri")
inundaciones_turri <- cargar_desde_redis("inundaciones_turri")
infraestructura_turri <- cargar_desde_redis("infraestructura_turri")
asentamientos_turri <- cargar_desde_redis("asentamientos_turri")
medidores_turri <- cargar_desde_redis("medidores_turri") %>% st_centroid () 
transformadores_turri <- cargar_desde_redis("transformadores_turri") %>% st_centroid ()
postes_turri <- cargar_desde_redis("postes_turri") %>% st_centroid ()
congestionamiento1_turri <- cargar_desde_redis("congestionamiento1_turri")
congestionamiento2_turri <- cargar_desde_redis("congestionamiento2_turri")
congestionamiento3_turri <- cargar_desde_redis("congestionamiento3_turri")
congestionamiento4_turri <- cargar_desde_redis("congestionamiento4_turri")
congestionamiento5_turri <- cargar_desde_redis("congestionamiento5_turri")
congestionamiento_turri <- cargar_desde_redis("congestionamiento_turri")
accidentes_turri <- cargar_desde_redis("accidentes_turri")
RVN_turri <- cargar_desde_redis("RVN_turri")
puentes_turri <- cargar_desde_redis("puentes_turri")
valorizables_turri <- cargar_desde_redis("valorizables_turri")
UGM_lib <- cargar_desde_redis("UGM_lib")
inundaciones_lib <- cargar_desde_redis("inundaciones_lib")
infraestructura_lib <- cargar_desde_redis("infraestructura_lib")
asentamientos_lib <- cargar_desde_redis("asentamientos_lib")
medidores_lib <- cargar_desde_redis("medidores_lib") %>% st_centroid ()
transformadores_lib <- cargar_desde_redis("transformadores_lib") %>% st_centroid ()
postes_lib <- cargar_desde_redis("postes_lib") %>% st_centroid ()
Agua_lib <- cargar_desde_redis("Agua_lib") %>% st_centroid ()
no_valorizables_lib <- cargar_desde_redis("no_valorizables_lib")
valorizables_lib <- cargar_desde_redis("valorizables_lib")
congestionamiento1_lib <- cargar_desde_redis("congestionamiento1_lib")
congestionamiento2_lib <- cargar_desde_redis("congestionamiento2_lib")
congestionamiento3_lib <- cargar_desde_redis("congestionamiento3_lib")
congestionamiento4_lib <- cargar_desde_redis("congestionamiento4_lib")
congestionamiento5_lib <- cargar_desde_redis("congestionamiento5_lib")
congestionamiento_lib <- cargar_desde_redis("congestionamiento_lib")
accidentes_lib <- cargar_desde_redis("accidentes_lib")
RVN_lib <- cargar_desde_redis("RVN_lib")
puentes_lib <- cargar_desde_redis("puentes_lib")
UGM_sc <- cargar_desde_redis("UGM_sc")
infraestructura_sc <- cargar_desde_redis("infraestructura_sc")
asentamientos_sc <- cargar_desde_redis("asentamientos_sc")
transformadores_sc <- cargar_desde_redis("transformadores_sc") %>% st_centroid ()
postes_sc <- cargar_desde_redis("postes_sc") %>% st_centroid ()
congestionamiento1_sc <- cargar_desde_redis("congestionamiento1_sc")
congestionamiento2_sc <- cargar_desde_redis("congestionamiento2_sc")
congestionamiento3_sc <- cargar_desde_redis("congestionamiento3_sc")
congestionamiento4_sc <- cargar_desde_redis("congestionamiento4_sc")
congestionamiento5_sc <- cargar_desde_redis("congestionamiento5_sc")
congestionamiento_sc <- cargar_desde_redis("congestionamiento_sc")
accidentes_sc <- cargar_desde_redis("accidentes_sc") %>% st_centroid ()
RVN_sc <- cargar_desde_redis("RVN_sc")
puentes_sc <- cargar_desde_redis("puentes_sc")
Agua_sc <- cargar_desde_redis("Agua_sc")
novalorizables_map_sc <- cargar_desde_redis("novalorizables_map_sc")
UGM_lim <- cargar_desde_redis("UGM_lim")
asentamientos_lim <- cargar_desde_redis("asentamientos_lim")
inundaciones_lim <- cargar_desde_redis("inundaciones_lim")
infraestructura_lim <- cargar_desde_redis("infraestructura_lim")
medidores_lim <- cargar_desde_redis("medidores_lim") %>% st_centroid ()
transformadores_lim <- cargar_desde_redis("transformadores_lim") %>% st_centroid ()
postes_lim <- cargar_desde_redis("postes_lim") %>% st_centroid ()
Agua_lim <- cargar_desde_redis("Agua_lim") %>% st_centroid ()
no_valorizables_lim <- cargar_desde_redis("no_valorizables_lim")
congestionamiento1_lim <- cargar_desde_redis("congestionamiento1_lim")
congestionamiento2_lim <- cargar_desde_redis("congestionamiento2_lim")
congestionamiento3_lim <- cargar_desde_redis("congestionamiento3_lim")
congestionamiento4_lim <- cargar_desde_redis("congestionamiento4_lim")
congestionamiento5_lim <- cargar_desde_redis("congestionamiento5_lim")
congestionamiento_lim <- cargar_desde_redis("congestionamiento_lim")
accidentes_lim <- cargar_desde_redis("accidentes_lim") %>% st_centroid ()
RVN_lim <- cargar_desde_redis("RVN_lim")
puentes_lim <- cargar_desde_redis("puentes_lim")
novalorizables_sc <- cargar_desde_redis("novalorizables_sc")
UGM_guap <- cargar_desde_redis("UGM_guap")
inundaciones_guap <- cargar_desde_redis("inundaciones_guap")
infraestructura_guap <- cargar_desde_redis("infraestructura_guap")
asentamientos_guap <- cargar_desde_redis("asentamientos_guap")
medidores_guap <- cargar_desde_redis("medidores_guap") %>% st_centroid ()
transformadores_guap <- cargar_desde_redis("transformadores_guap") %>% st_centroid ()
postes_guap <- cargar_desde_redis("postes_guap") %>% st_centroid ()
edificaciones_guap <- cargar_desde_redis("edificaciones_guap")
no_valorizables_guap <- cargar_desde_redis("no_valorizables_guap")
valorizables_guap <- cargar_desde_redis("valorizables_guap")
congestionamiento1_guap <- cargar_desde_redis("congestionamiento1_guap")
congestionamiento2_guap <- cargar_desde_redis("congestionamiento2_guap")
congestionamiento3_guap <- cargar_desde_redis("congestionamiento3_guap")
congestionamiento4_guap <- cargar_desde_redis("congestionamiento4_guap")
congestionamiento5_guap <- cargar_desde_redis("congestionamiento5_guap")
congestionamiento_guap <- cargar_desde_redis("congestionamiento_guap")
accidentes_guap <- cargar_desde_redis("accidentes_guap")
RVN_guap <- cargar_desde_redis("RVN_guap")
puentes_guap <- cargar_desde_redis("puentes_guap")
Agua_guap <- cargar_desde_redis("Agua_guap") %>% st_centroid ()

PerezZeledon_with_clusters_ward <- read_csv("datos/PZ/datos/PerezZeledon_with_clusters_ward.csv")
UGM_pz <- UGM_pz %>%
  select(ID_MGN, geometry) %>%
  full_join(PerezZeledon_with_clusters_ward, by = "ID_MGN") %>%
  filter(!is.na(cluster)) %>%
  select(ID_MGN, everything(), -geometry) %>%
  select(everything(), geometry)

UGM_pz$cluster <- dplyr::recode(UGM_pz$cluster,
                                `1` = "A",
                                `2` = "B",
                                `3` = "C")



Puntarenas_with_clusters_ward <- read_csv("datos/Puntarenas/datos/Puntarenas_with_clusters_ward.csv")

UGM_punt <- UGM_punt %>%
  select(ID_MGN, geometry) %>%
  full_join(Puntarenas_with_clusters_ward, by = "ID_MGN") %>%
  filter(!is.na(cluster)) %>%
  select(ID_MGN, everything(), -geometry) %>%
  select(everything(), geometry)


UGM_punt$cluster <- dplyr::recode(UGM_punt$cluster,
                                  `1` = "A",
                                  `2` = "B",
                                  `3` = "C")

Guapiles_with_clusters_ward <- read_csv("datos/Pococi/datos/Pococi_with_clusters_ward.csv")

UGM_guap <- UGM_guap %>%
  select(ID_MGN, geometry) %>%
  full_join(Guapiles_with_clusters_ward, by = "ID_MGN") %>%
  filter(!is.na(cluster)) %>%
  select(ID_MGN, everything(), -geometry) %>%
  select(everything(), geometry)

UGM_guap$cluster <- dplyr::recode(UGM_guap$cluster,
                                  `2` = "A",
                                  `1` = "B",
                                  `3` = "C")

Turrialba_with_clusters_ward <- read_csv("datos/Turrialba/datos/Turrialba_with_clusters_ward.csv")

UGM_turri <- UGM_turri %>%
  select(ID_MGN, geometry) %>%
  full_join(Turrialba_with_clusters_ward, by = "ID_MGN") %>%
  filter(!is.na(cluster)) %>%
  select(ID_MGN, everything(), -geometry) %>%
  select(everything(), geometry)

UGM_turri$cluster <- dplyr::recode(UGM_turri$cluster,
                                   `2` = "A",
                                   `1` = "B")

Liberia_with_clusters_ward <- read_csv("datos/Liberia/datos/Liberia_with_clusters_ward.csv")

UGM_lib <- UGM_lib %>%
  select(ID_MGN, geometry) %>%
  full_join(Liberia_with_clusters_ward, by = "ID_MGN") %>%
  filter(!is.na(cluster)) %>%
  select(ID_MGN, everything(), -geometry) %>%
  select(everything(), geometry)

UGM_lib$cluster <- dplyr::recode(UGM_lib$cluster,
                                 `3` = "A",
                                 `2` = "B",
                                 `1` = "C")

SanCarlos_with_clusters_ward <- read_csv("datos/SanCarlos/datos/SanCarlos_with_clusters_ward.csv")

UGM_sc <- UGM_sc %>%
  select(ID_MGN, geometry) %>%
  full_join(SanCarlos_with_clusters_ward, by = "ID_MGN") %>%
  filter(!is.na(cluster)) %>%
  select(ID_MGN, everything(), -geometry) %>%
  select(everything(), geometry)

UGM_sc$cluster <- dplyr::recode(UGM_sc$cluster,
                                `2` = "A",
                                `1` = "B")

Limon_with_clusters_ward <- read_csv("datos/Limon/datos/Limon_with_clusters_ward.csv")

UGM_lim <- UGM_lim %>%
  select(ID_MGN, geometry) %>%
  full_join(Limon_with_clusters_ward, by = "ID_MGN") %>%
  filter(!is.na(cluster)) %>%
  select(ID_MGN, everything(), -geometry) %>%
  select(everything(), geometry)

UGM_lim$cluster <- dplyr::recode(UGM_lim$cluster,
                                 `1` = "A",
                                 `2` = "B")

# Cierra la conexión con Redis
redis_conn$QUIT()



generate_map <- memoise(function(data, pal) {
  leaflet(data = data) %>% 
    addTiles() %>%
    addPolygons(
      fillColor = ~pal(cluster),
      fillOpacity = 0.5,
      weight = 0.5,
      color = "black",
      highlightOptions = highlightOptions(
        color = "#CE1212",
        weight = 3,
        bringToFront = TRUE,
        opacity = 1
      ),
      label = ~ID_MGN
    ) %>%
    addLegend(
      pal = pal, 
      values = ~cluster, 
      title = "Clúster",
      opacity = 0.7,
      position = "bottomright"
    )
})




# # Cargar los datos
# 
# edificaciones_pz <- read_sf("datos/PZ/datos/edificaciones_pz_ugm1.geojson")
# edificaciones_pz <- edificaciones_pz %>%
#   mutate(col = 1) %>%
#   group_by(col) %>%
#   summarise(geometry = st_union(geometry))
# 
# edificaciones_punt <- read_sf("datos/Puntarenas/datos/Edificaciones puntarenas UGM1.geojson")
# edificaciones_punt <- edificaciones_punt %>%
#   mutate(col = 1) %>%
#   group_by(col) %>%
#   summarise(geometry = st_union(geometry))
# 
# edificaciones_guap <- read_sf("datos/Pococi/datos/edificaciones pococi ugm1.geojson")
# edificaciones_guap <- edificaciones_guap %>%
#   mutate(col = 1) %>%
#   group_by(col) %>%
#   summarise(geometry = st_union(geometry))
# 
# edificaciones_lib <- read_sf("datos/Liberia/datos/edificaciones liberia ugm1.geojson")
# edificaciones_lib <- edificaciones_lib %>%
#   mutate(col = 1) %>%
#   group_by(col) %>%
#   summarise(geometry = st_union(geometry))
# 
# edificaciones_turri <- read_sf("datos/Turrialba/datos/Turi edificaciones ugm1.geojson")
# edificaciones_turri <- edificaciones_turri%>%
#   mutate(col = 1) %>%
#   group_by(col) %>%
#   summarise(geometry = st_union(geometry))
# 
# edificaciones_lim <- read_sf("datos/Limon/datos/Edificaciones limon ugm1.geojson")
# edificaciones_lim <- edificaciones_lim %>%
#   mutate(col = 1) %>%
#   group_by(col) %>%
#   summarise(geometry = st_union(geometry))
# 
# edificaciones_sc <- read_sf("datos/SanCarlos/datos/edificaciones san carlos ugm1.geojson")
# edificaciones_sc <- edificaciones_sc%>%
#   mutate(col = 1) %>%
#   group_by(col) %>%
#   summarise(geometry = st_union(geometry))
# 
# UGM_pz <- st_read("datos/PZ/datos/PZ  UGM cortadas.geojson") %>%
#    st_transform(crs = 4326) 


#  
#  inundaciones_pz <- read_sf("datos/PZ/datos/PZ area inundacion UGM.geojson") %>%
#    st_transform(crs = 4326)
#  
#  infraestructura_pz <- read_sf("datos/PZ/datos/PZ infra urbana UGM.geojson") %>%
#    st_transform(crs = 4326)
#  
#  medidores_pz <- st_read("datos/PZ/datos/PZ medidores inteligentes ICE act UGM.geojson") %>%
#    st_transform(crs = 4326)
#  
#  transformadores_pz <- st_read("datos/PZ/datos/PZ trafos UGM.geojson") %>%
#    st_transform(crs = 4326)
#  
#  postes_pz <- st_read("datos/PZ/datos/PZ postes UGM.geojson") %>%
#    st_transform(crs = 4326)
#  
#  Agua_pz <- read_sf("datos/PZ/datos/PZ Agua potable UGM.geojson") %>%
#    st_transform(crs = 4326)
#  
#   no_valorizables_pz <- read_sf("datos/PZ/datos/PZ basura UGM.geojson") %>%
#     st_transform(crs = 4326) %>%
#     st_make_valid() %>%
#     st_simplify(dTolerance = 0.001, preserveTopology = TRUE) %>%
#     filter(st_is_valid(.)) 
# #  
#  congestionamiento1_pz <- read_sf("datos/PZ/datos/Nivel 1 PZ ugm.geojson") 
#  congestionamiento1_pz$id <- 1
#  
#  congestionamiento2_pz <- read_sf("datos/PZ/datos/Nivel 2 PZ ugm.geojson")
#  congestionamiento2_pz$id <- 2
#  
#  congestionamiento3_pz <- read_sf("datos/PZ/datos/Nivel 3 PZ ugm.geojson")
#  congestionamiento3_pz$id <- 3
#  
#  congestionamiento4_pz <- read_sf("datos/PZ/datos/nivel 4 PZ ugm.geojson")
#  congestionamiento4_pz$id <- 4
#  
#  congestionamiento5_pz <- read_sf("datos/PZ/datos/Nivel 5 PZ ugm.geojson")
#  congestionamiento5_pz$id <- 5
#  
#  congestionamiento_pz <- rbind(congestionamiento1_pz, congestionamiento2_pz, congestionamiento3_pz, congestionamiento4_pz, congestionamiento5_pz)
#  
#  accidentes_pz <- read_sf("datos/PZ/datos/PZ puntos accident UGM.geojson") %>%
#    st_transform(crs = 4326)
#  
#  RVN_pz <-  read_sf("datos/PZ/datos/PZ RVN UGM.geojson") %>%
#    st_transform(crs = 4326)
#  
#  puentes_pz <- read_sf("datos/PZ/datos/PZ puentes UGM.geojson") %>%
#    st_transform(crs = 4326)
#  
#  UGM_punt <- st_read("datos/Puntarenas/datos/Punt UGM cortada.geojson") %>%
#    st_transform(crs = 4326)
#  
#  inundaciones_punt <- read_sf("datos/Puntarenas/datos/Punt Area de inundacion UGM.geojson") %>%
#    st_transform(crs = 4326)
#  
#  infraestructura_punt <- read_sf("datos/Puntarenas/datos/Punt infra urba UGM.geojson") %>%
#    st_transform(crs = 4326)
#  
#  asentamientos_punt <- read_sf("datos/Puntarenas/datos/Punt Asentam informal UGM.geojson") %>%
#    st_transform(crs = 4326)
#  
#  medidores_punt <- st_read("datos/Puntarenas/datos/Punt medidor inteligente act ugm.geojson") %>%
#    st_transform(crs = 4326)
#  
#  transformadores_punt <- st_read("datos/Puntarenas/datos/Punt postes trafos UGM.geojson") %>%
#    st_transform(crs = 4326)
#  
#  postes_punt <- st_read("datos/Puntarenas/datos/Punt postes actual ugm.geojson") %>%
#    st_transform(crs = 4326)
#  
#  Agua_punt <- read_sf("datos/Puntarenas/datos/Punt agua potable calidad UGM.geojson") %>%
#    st_transform(crs = 4326)
#  
#  congestionamiento1_punt <- read_sf("datos/Puntarenas/datos/nivel 1 punt ugm.geojson")
#  congestionamiento1_punt$id <- 1
#  
#  congestionamiento2_punt <- read_sf("datos/Puntarenas/datos/nivel 2 punt ugm.geojson")
#  congestionamiento2_punt$id <- 2
#  
#  congestionamiento3_punt <- read_sf("datos/Puntarenas/datos/nivel 3 punt ugm.geojson")
#  congestionamiento3_punt$id <- 3
#  
#  congestionamiento4_punt <- read_sf("datos/Puntarenas/datos/Nivel 4 punt ugm.geojson")
#  congestionamiento4_punt$id <- 4
#  
#  congestionamiento5_punt <- read_sf("datos/Puntarenas/datos/Nivel 5 punt ugm.geojson")
#  congestionamiento5_punt$id <- 5
#  
#  congestionamiento_punt <- rbind(congestionamiento1_punt, congestionamiento2_punt, congestionamiento3_punt, congestionamiento4_punt, congestionamiento5_punt)
#  
#  accidentes_punt <- read_sf("datos/Puntarenas/datos/Punt punto accid UGM.geojson") %>%
#    st_transform(crs = 4326)
#  
#  RVN_punt <-  read_sf("datos/Puntarenas/datos/Punt RN evaluac UGM.geojson") %>%
#    st_transform(crs = 4326)
#  
#  puentes_punt <- read_sf("datos/Puntarenas/datos/Punt puentes RNUGM.geojson") %>%
#    st_transform(crs = 4326)
#  
#  UGM_guap <- st_read("datos/Pococi/datos/Guap UGM cortado.geojson") %>%
#    st_transform(crs = 4326)
#  
#  inundaciones_guap <- read_sf("datos/Pococi/datos/Guap potencial inundacn UGM.geojson") %>%
#    st_transform(crs = 4326)
#  
#  infraestructura_guap <- read_sf("datos/Pococi/datos/Guap infra urba UGM.geojson")%>%
#    st_transform(crs = 4326)
#  
#  asentamientos_guap <- read_sf("datos/Pococi/datos/Guap asentam informal UGM.geojson") %>%
#    st_transform(crs = 4326)
#  
#  medidores_guap <- st_read("datos/Pococi/datos/Guap medidor intelg UGM.geojson") %>%
#    st_transform(crs = 4326)
#  
#  transformadores_guap <- st_read("datos/Pococi/datos/Guap trafos UGM.geojson") %>%
#    st_transform(crs = 4326)
#  
#  postes_guap <- st_read("datos/Pococi/datos/Guap postes UGM.geojson") %>%
#    st_transform(crs = 4326)
#  
#  
#  Agua_guap <- read_sf("datos/Pococi/datos/Guap agua potable ugm.geojson") %>%
#    st_transform(crs = 4326)
#  
# no_valorizables_guap <- read_sf("datos/Pococi/datos/Guap recolecc no valor UGM.geojson") %>%
#   st_transform(crs = 4326) %>%
#   st_make_valid() %>%
#   st_simplify(dTolerance = 0.001, preserveTopology = TRUE) %>%
#   filter(st_is_valid(.)) 
# 
# no_valorizables_guap <- no_valorizables_guap %>%
#      mutate(col = 1) %>%
#      group_by(col) %>%
#      summarise(geometry = st_union(geometry))
#   
# #  
#   valorizables_guap <-  read_sf("datos/Pococi/datos/Guap recolecc valor UGM.geojson")%>%
#     st_transform(crs = 4326) %>%
#     st_make_valid() %>%
#     st_simplify(dTolerance = 0.001, preserveTopology = TRUE) %>%
#     filter(st_is_valid(.))
#   
#   valorizables_guap <- valorizables_guap %>%
#     mutate(col = 1) %>%
#     group_by(col) %>%
#     summarise(geometry = st_union(geometry))

#  
#  congestionamiento1_guap <- read_sf("datos/Pococi/datos/Nivel 1 Guap ugm.geojson")
#  congestionamiento1_guap$id <- 1
#  
#  congestionamiento2_guap <- read_sf("datos/Pococi/datos/Nivel 2 guap ugm.geojson")
#  congestionamiento2_guap$id <- 2
#  
#  congestionamiento3_guap <- read_sf("datos/Pococi/datos/Nivel 3 guap ugm.geojson")
#  congestionamiento3_guap$id <- 3
#  
#  congestionamiento4_guap <- read_sf("datos/Pococi/datos/nivel 4 guap ugm.geojson")
#  congestionamiento4_guap$id <- 4
#  
#  congestionamiento5_guap <- read_sf("datos/Pococi/datos/Nivel 5 ugm.geojson")
#  congestionamiento5_guap$id <- 5
#  
#  congestionamiento_guap <- rbind(congestionamiento1_guap, congestionamiento2_guap, congestionamiento3_guap, congestionamiento4_guap, congestionamiento5_guap)
#  
#  accidentes_guap <- read_sf("datos/Pococi/datos/Guap puntos de accidente UGM.geojson")%>%
#    st_transform(crs = 4326)
#  
#  RVN_guap <-  read_sf("datos/Pococi/datos/Guap RVN UGM.geojson") %>%
#    st_transform(crs = 4326)
#  
#  puentes_guap <- read_sf("datos/Pococi/datos/Guap puentes UGM.geojson") %>%
#    st_transform(crs = 4326)
#  
#  UGM_turri <- st_read("datos/Turrialba/datos/Turri UGM cortado.geojson") %>%
#    st_transform(crs = 4326)
#  
#  inundaciones_turri <- read_sf("datos/Turrialba/datos/Turri inundacion UGM.geojson") %>%
#    st_transform(crs = 4326)  # Transformar a EPSG:4326
#  
#  infraestructura_turri <- read_sf("datos/Turrialba/datos/Turri infra urbana UGM.geojson") %>%
#    st_transform(crs = 4326)  # Transformar a EPSG:4326
#  
#  asentamientos_turri <- read_sf("datos/Turrialba/datos/Asent Turri UGM.shp")%>%
#    st_transform(crs = 4326)
#  
#  medidores_turri <- st_read("datos/Turrialba/datos/Turri medidor inteligente act ugm.geojson")%>%
#    st_transform(crs = 4326)
#  
#  transformadores_turri <- st_read("datos/Turrialba/datos/Turri trafos UGM.geojson")%>%
#    st_transform(crs = 4326)
#  
#  postes_turri <- st_read("datos/Turrialba/datos/Turri postes UGM.geojson")%>%
#    st_transform(crs = 4326)
#  
#  congestionamiento1_turri <- read_sf("datos/Turrialba/datos/Nivel 1 Turri ugm.geojson")
#  congestionamiento1_turri$id <- 1
#  
#  congestionamiento2_turri <- read_sf("datos/Turrialba/datos/Nivel 2 Turri ugm.geojson")
#  congestionamiento2_turri$id <- 2
#  
#  congestionamiento3_turri <- read_sf("datos/Turrialba/datos/Nivel 3 Turri ugm.geojson")
#  congestionamiento3_turri$id <- 3
#  
#  congestionamiento4_turri <- read_sf("datos/Turrialba/datos/nivel 4 Turri ugm.geojson")
#  congestionamiento4_turri$id <- 4
#  
#  congestionamiento5_turri <- read_sf("datos/Turrialba/datos/Nivel 5 Turri ugm.geojson")
#  congestionamiento5_turri$id <- 5
#  
#  congestionamiento_turri <- rbind(congestionamiento1_turri, congestionamiento2_turri, congestionamiento3_turri, congestionamiento4_turri, congestionamiento5_turri)
#  
#  accidentes_turri <- read_sf("datos/Turrialba/datos/Turri puntos accident UGM.geojson") %>%
#    st_transform(crs = 4326)
#  
#  RVN_turri <-  read_sf("datos/Turrialba/datos/Turri RVN evaluacion UGM.geojson")%>%
#    st_transform(crs = 4326)
#  
#  puentes_turri <- read_sf("datos/Turrialba/datos/Turri puentes UGM.geojson")%>%
#    st_transform(crs = 4326)
#  
#   valorizables_turri <-  read_sf("datos/Turrialba/datos/Turri residuos valorizable UGM.geojson")%>%
#     st_transform(crs = 4326) %>%
#     st_make_valid() %>%
#     st_simplify(dTolerance = 0.001, preserveTopology = TRUE) %>%
#     filter(st_is_valid(.)) 
#   valorizables_turri <- valorizables_turri %>%
#     mutate(col = 1) %>%
#     group_by(col) %>%
#     summarise(geometry = st_union(geometry))
# #  
#  UGM_lib <- st_read("datos/Liberia/datos/Lib UGM cortado.geojson")%>%
#    st_transform(crs = 4326)
#  
#  inundaciones_lib <- read_sf("datos/Liberia/datos/Lib area inundacn UGM.geojson") %>%
#    st_transform(crs = 4326)  # Transformar a EPSG:4326
#  
#  infraestructura_lib <- read_sf("datos/Liberia/datos/Lib infra urbana UGM.geojson") %>%
#    st_transform(crs = 4326)  # Transformar a EPSG:4326
#  
#  asentamientos_lib <- read_sf("datos/Liberia/datos/Lib asentam informal UGM.geojson") %>%
#    st_transform(crs = 4326)  # Transformar a EPSG:4326
#  
#  medidores_lib <- st_read("datos/Liberia/datos/Lib medidor inteligente UGM.geojson")%>%
#    st_transform(crs = 4326)
#  
#  transformadores_lib <- st_read("datos/Liberia/datos/Lib trafos UGM.geojson")%>%
#    st_transform(crs = 4326)
#  
#  postes_lib <- st_read("datos/Liberia/datos/Lib postes UGM.geojson")%>%
#    st_transform(crs = 4326)
#  
#  Agua_lib <- read_sf("datos/Liberia/datos/Lib agua potable UGM.geojson")%>%
#    st_transform(crs = 4326)
#  
#   no_valorizables_lib <-  read_sf("datos/Liberia/datos/Lib recoleccion UGM.geojson")%>%
#     st_transform(crs = 4326) %>%
#     st_make_valid() %>%
#     st_simplify(dTolerance = 0.001, preserveTopology = TRUE) %>%
#     filter(st_is_valid(.)) 
#   no_valorizables_lib <- no_valorizables_lib %>%
#     mutate(col = 1) %>%
#     group_by(col) %>%
#     summarise(geometry = st_union(geometry))
# #  
#   valorizables_lib <-  read_sf("datos/Liberia/datos/Lib recoleccn RSV UGM.geojson")%>%
#     st_transform(crs = 4326) %>%
#     st_make_valid() %>%
#     st_simplify(dTolerance = 0.001, preserveTopology = TRUE) %>%
#     filter(st_is_valid(.))
#   valorizables_lib <- valorizables_lib %>%
#     mutate(col = 1) %>%
#     group_by(col) %>%
#     summarise(geometry = st_union(geometry))
#  
#  congestionamiento1_lib <- read_sf("datos/Liberia/datos/Nivel 1 Lib ugm.geojson")
#  congestionamiento1_lib$id <- 1
#  
#  congestionamiento2_lib <- read_sf("datos/Liberia/datos/Nivel 2 ugm.geojson")
#  congestionamiento2_lib$id <- 2
#  
#  congestionamiento3_lib <- read_sf("datos/Liberia/datos/Nivel 3 Lib ugm.geojson")
#  congestionamiento3_lib$id <- 3
#  
#  congestionamiento4_lib <- read_sf("datos/Liberia/datos/nivel 4 Lib ugm.geojson")
#  congestionamiento4_lib$id <- 4
#  
#  congestionamiento5_lib <- read_sf("datos/Liberia/datos/Nivel 5 Lib ugm.geojson")
#  congestionamiento5_lib$id <- 5
#  
#  congestionamiento_lib <- rbind(congestionamiento1_lib, congestionamiento2_lib, congestionamiento3_lib, congestionamiento4_lib, congestionamiento5_lib)
#  
#  accidentes_lib <- read_sf("datos/Liberia/datos/Lib puntos accid UGM.geojson")%>%
#    st_transform(crs = 4326)
#  
#  RVN_lib <-  read_sf("datos/Liberia/datos/Lib RVN UGM.geojson")%>%
#    st_transform(crs = 4326)
#  
#  puentes_lib <- read_sf("datos/Liberia/datos/Lib puentes UGM.geojson") %>%
#    st_transform(crs = 4326)
#  
#  UGM_sc <- st_read("datos/SanCarlos/datos/SC UGM cortada.geojson") %>%
#    st_transform(crs = 4326)
# 
#  infraestructura_sc <- read_sf("datos/SanCarlos/datos/SC infra urbana finca UGM.geojson") %>%
#    st_transform(crs = 4326)
#  
#  asentamientos_sc <- st_read("datos/SanCarlos/datos/SC Asentamiento informal finca UGM.geojson") %>%
#    st_transform(crs = 4326)
#  
#  transformadores_sc <- st_read("datos/SanCarlos/datos/SC trafos finca UGM.geojson") %>%
#    st_transform(crs = 4326)
#  
#  postes_sc <- st_read("datos/SanCarlos/datos/SC postes finca UGM.geojson") %>%
#    st_transform(crs = 4326)
#  
#  congestionamiento1_sc <- read_sf("datos/SanCarlos/datos/Nivel 1 SC UGM.geojson")
#  congestionamiento1_sc$id <- 1
#  
#  congestionamiento2_sc <- read_sf("datos/SanCarlos/datos/Nivel 2 SC ugm.geojson")
#  congestionamiento2_sc$id <- 2
#  
#  congestionamiento3_sc <- read_sf("datos/SanCarlos/datos/Nivel 3 SC ugm.geojson")
#  congestionamiento3_sc$id <- 3
#  
#  congestionamiento4_sc <- read_sf("datos/SanCarlos/datos/nivel 4 SC ugm.geojson")
#  congestionamiento4_sc$id <- 4
#  
#  congestionamiento5_sc <- read_sf("datos/SanCarlos/datos/Nivel 5 SC ugm.geojson")
#  congestionamiento5_sc$id <- 5
#  
#  congestionamiento_sc <- rbind(congestionamiento1_sc, congestionamiento2_sc, congestionamiento3_sc, congestionamiento4_sc, congestionamiento5_sc)
#  
#  accidentes_sc <- read_sf("datos/SanCarlos/datos/SC puntos accidente finca UGM.geojson") %>%
#    st_transform(crs = 4326)
# 
#  RVN_sc <-  read_sf("datos/SanCarlos/datos/SC RVN finca UGM.shp") %>%
#    st_transform(crs = 4326)
# 
#  puentes_sc <- read_sf("datos/SanCarlos/datos/SC puentes finca UGM.shp") %>%
#    st_transform(crs = 4326)
#  
#  Agua_sc <- read_sf("datos/SanCarlos/datos/SC fincas UGM para AD final.shp") %>%
#    st_transform(crs = 4326)
#  
# novalorizables_map_sc <-  read_sf("datos/SanCarlos/datos/SC fincas UGM para AD final.shp") %>%
#   st_transform(crs = 4326) %>%
#   st_make_valid() %>%
#   st_simplify(dTolerance = 0.001, preserveTopology = TRUE) %>%
#   filter(st_is_valid(.)) 
# novalorizables_map_sc <- novalorizables_map_sc %>%
#   mutate(col = 1) %>%
#   group_by(col) %>%
#   summarise(geometry = st_union(geometry))
#  

#  
#  UGM_lim <- st_read("datos/Limon/datos/Lim UGM cortado.geojson") %>%
#    st_transform(crs = 4326)
#  
#  asentamientos_lim <- read_sf("datos/Limon/datos/Lim asent inf mivah ugm.geojson") %>%
#    st_transform(crs = 4326) 
#  
#  inundaciones_lim <- read_sf("datos/Limon/datos/Lim area inundacn ugm.geojson") %>%
#    st_transform(crs = 4326)  
#  
#  infraestructura_lim <- read_sf("datos/Limon/datos/Lim infra urbana ugm.geojson") %>%
#    st_transform(crs = 4326)  
#  
#  medidores_lim <- st_read("datos/Limon/datos/Lim medidor intelig act ugm.geojson") %>%
#    st_transform(crs = 4326)
#  
#  transformadores_lim <- st_read("datos/Limon/datos/Lim trafos ugm.geojson") %>%
#    st_transform(crs = 4326)
#  
#  postes_lim <- st_read("datos/Limon/datos/Lim postes ugm.geojson") %>%
#    st_transform(crs = 4326)
# 
#  Agua_lim <- read_sf("datos/Limon/datos/Lim agua potable UGM.geojson") %>%
#    st_transform(crs = 4326)
#  
# no_valorizables_lim <-  read_sf("datos/Limon/datos/Lim recoleccn ugm.geojson")%>%
#   st_transform(crs = 4326) %>%
#   st_make_valid() %>%
#   st_simplify(dTolerance = 0.001, preserveTopology = TRUE) %>%
#   filter(st_is_valid(.)) 
# 
# no_valorizables_lim <- no_valorizables_lim %>%
#   mutate(col = 1) %>%
#   group_by(col) %>%
#   summarise(geometry = st_union(geometry))


#  congestionamiento1_lim <- read_sf("datos/Limon/datos/Nivel 1 Lim ugm.geojson")
#  congestionamiento1_lim$id <- 1
#  
#  congestionamiento2_lim <- read_sf("datos/Limon/datos/nivel 2 UGM final.geojson")
#  congestionamiento2_lim$id <- 2
#  
#  congestionamiento3_lim <- read_sf("datos/Limon/datos/Nivel 3 Lim ugm.geojson")
#  congestionamiento3_lim$id <- 3
#  
#  congestionamiento4_lim <- read_sf("datos/Limon/datos/Nivel 4 Lim ugm.geojson")
#  congestionamiento4_lim$id <- 4
#  
#  congestionamiento5_lim <- read_sf("datos/Limon/datos/nivel 5 lim ugm.geojson")
#  congestionamiento5_lim$id <- 5
#  
#  congestionamiento_lim <- rbind(congestionamiento1_lim, congestionamiento2_lim, congestionamiento3_lim, congestionamiento4_lim, congestionamiento5_lim)
#  
#  accidentes_lim <- read_sf("datos/Limon/datos/Lim puntos accid ugm.geojson")  %>%
#    st_transform(crs = 4326)
#  
#  RVN_lim <-  read_sf("datos/Limon/datos/Lim RVN ugm.geojson") %>%
#    st_transform(crs = 4326)
#  
#  puentes_lim <- read_sf("datos/Limon/datos/Lim puentes ugm.geojson") %>%
#    st_transform(crs = 4326)
#  
# novalorizables_sc <-  read_sf("datos/SanCarlos/datos/SC fincas UGM para AD final.shp") %>%
#   st_transform(crs = 4326) %>%
#   st_make_valid() %>%
#   st_simplify(dTolerance = 0.001, preserveTopology = TRUE) %>%
#   filter(st_is_valid(.)) 





# Guardar datos en Redis
# redis_conn$SET("edificaciones_pz", serialize(edificaciones_pz, NULL))
# redis_conn$SET("edificaciones_lim", serialize(edificaciones_lim, NULL))
# redis_conn$SET("edificaciones_turri", serialize(edificaciones_turri, NULL))
# redis_conn$SET("edificaciones_guap", serialize(edificaciones_guap, NULL))
# redis_conn$SET("edificaciones_lib", serialize(edificaciones_lib, NULL))
# redis_conn$SET("edificaciones_punt", serialize(edificaciones_punt, NULL))
# redis_conn$SET("edificaciones_sc", serialize(edificaciones_sc, NULL))
# redis_conn$SET("UGM_pz", serialize(UGM_pz, NULL))
# redis_conn$SET("inundaciones_pz", serialize(inundaciones_pz, NULL))
# redis_conn$SET("infraestructura_pz", serialize(infraestructura_pz, NULL))
# redis_conn$SET("medidores_pz", serialize(medidores_pz, NULL))
# redis_conn$SET("transformadores_pz", serialize(transformadores_pz, NULL))
# redis_conn$SET("postes_pz", serialize(postes_pz, NULL))
# redis_conn$SET("Agua_pz", serialize(Agua_pz, NULL))
# redis_conn$SET("no_valorizables_pz", serialize(no_valorizables_pz, NULL))
# redis_conn$SET("congestionamiento_pz", serialize(congestionamiento_pz, NULL))
# redis_conn$SET("accidentes_pz", serialize(accidentes_pz, NULL))
# redis_conn$SET("RVN_pz", serialize(RVN_pz, NULL))
# redis_conn$SET("puentes_pz", serialize(puentes_pz, NULL))
# redis_conn$SET("congestionamiento1_pz", serialize(congestionamiento1_pz, NULL))
# redis_conn$SET("congestionamiento2_pz", serialize(congestionamiento2_pz, NULL))
# redis_conn$SET("congestionamiento3_pz", serialize(congestionamiento3_pz, NULL))
# redis_conn$SET("congestionamiento4_pz", serialize(congestionamiento4_pz, NULL))
# redis_conn$SET("congestionamiento5_pz", serialize(congestionamiento5_pz, NULL))
# redis_conn$SET("congestionamiento1_punt", serialize(congestionamiento1_punt, NULL))
# redis_conn$SET("congestionamiento2_punt", serialize(congestionamiento2_punt, NULL))
# redis_conn$SET("congestionamiento3_punt", serialize(congestionamiento3_punt, NULL))
# redis_conn$SET("congestionamiento4_punt", serialize(congestionamiento4_punt, NULL))
# redis_conn$SET("congestionamiento5_punt", serialize(congestionamiento5_punt, NULL))
# redis_conn$SET("UGM_punt", serialize(UGM_punt, NULL))
# redis_conn$SET("inundaciones_punt", serialize(inundaciones_punt, NULL))
# redis_conn$SET("infraestructura_punt", serialize(infraestructura_punt, NULL))
# redis_conn$SET("asentamientos_punt", serialize(asentamientos_punt, NULL))
# redis_conn$SET("medidores_punt", serialize(medidores_punt, NULL))
# redis_conn$SET("transformadores_punt", serialize(transformadores_punt, NULL))
# redis_conn$SET("postes_punt", serialize(postes_punt, NULL))
# redis_conn$SET("Agua_punt", serialize(Agua_punt, NULL))
# redis_conn$SET("congestionamiento_punt", serialize(congestionamiento_punt, NULL))
# redis_conn$SET("accidentes_punt", serialize(accidentes_punt, NULL))
# redis_conn$SET("RVN_punt", serialize(RVN_punt, NULL))
# redis_conn$SET("puentes_punt", serialize(puentes_punt, NULL))
# redis_conn$SET("UGM_guap", serialize(UGM_guap, NULL))
# redis_conn$SET("inundaciones_guap", serialize(inundaciones_guap, NULL))
# redis_conn$SET("infraestructura_guap", serialize(infraestructura_guap, NULL))
# redis_conn$SET("asentamientos_guap", serialize(asentamientos_guap, NULL))
# redis_conn$SET("medidores_guap", serialize(medidores_guap, NULL))
# redis_conn$SET("transformadores_guap", serialize(transformadores_guap, NULL))
# redis_conn$SET("postes_guap", serialize(postes_guap, NULL))
# redis_conn$SET("no_valorizables_guap", serialize(no_valorizables_guap, NULL))
# redis_conn$SET("valorizables_guap", serialize(valorizables_guap, NULL))
# redis_conn$SET("congestionamiento1_guap", serialize(congestionamiento1_guap, NULL))
# redis_conn$SET("congestionamiento2_guap", serialize(congestionamiento2_guap, NULL))
# redis_conn$SET("congestionamiento3_guap", serialize(congestionamiento3_guap, NULL))
# redis_conn$SET("congestionamiento4_guap", serialize(congestionamiento4_guap, NULL))
# redis_conn$SET("congestionamiento5_guap", serialize(congestionamiento5_guap, NULL))
# redis_conn$SET("congestionamiento_guap", serialize(congestionamiento_guap, NULL))
# redis_conn$SET("accidentes_guap", serialize(accidentes_guap, NULL))
# redis_conn$SET("RVN_guap", serialize(RVN_guap, NULL))
# redis_conn$SET("puentes_guap", serialize(puentes_guap, NULL))
# redis_conn$SET("UGM_turri", serialize(UGM_turri, NULL))
# redis_conn$SET("inundaciones_turri", serialize(inundaciones_turri, NULL))
# redis_conn$SET("infraestructura_turri", serialize(infraestructura_turri, NULL))
# redis_conn$SET("asentamientos_turri", serialize(asentamientos_turri, NULL))
# redis_conn$SET("medidores_turri", serialize(medidores_turri, NULL))
# redis_conn$SET("transformadores_turri", serialize(transformadores_turri, NULL))
# redis_conn$SET("postes_turri", serialize(postes_turri, NULL))
# redis_conn$SET("congestionamiento1_turri", serialize(congestionamiento1_turri, NULL))
# redis_conn$SET("congestionamiento2_turri", serialize(congestionamiento2_turri, NULL))
# redis_conn$SET("congestionamiento3_turri", serialize(congestionamiento3_turri, NULL))
# redis_conn$SET("congestionamiento4_turri", serialize(congestionamiento4_turri, NULL))
# redis_conn$SET("congestionamiento5_turri", serialize(congestionamiento5_turri, NULL))
# redis_conn$SET("congestionamiento_turri", serialize(congestionamiento_turri, NULL))
# redis_conn$SET("accidentes_turri", serialize(accidentes_turri, NULL))
# redis_conn$SET("RVN_turri", serialize(RVN_turri, NULL))
# redis_conn$SET("puentes_turri", serialize(puentes_turri, NULL))
# redis_conn$SET("valorizables_turri", serialize(valorizables_turri, NULL))
# redis_conn$SET("UGM_lib", serialize(UGM_lib, NULL))
# redis_conn$SET("inundaciones_lib", serialize(inundaciones_lib, NULL))
# redis_conn$SET("infraestructura_lib", serialize(infraestructura_lib, NULL))
# redis_conn$SET("asentamientos_lib", serialize(asentamientos_lib, NULL))
# redis_conn$SET("medidores_lib", serialize(medidores_lib, NULL))
# redis_conn$SET("transformadores_lib", serialize(transformadores_lib, NULL))
# redis_conn$SET("postes_lib", serialize(postes_lib, NULL))
# redis_conn$SET("Agua_lib", serialize(Agua_lib, NULL))
# redis_conn$SET("no_valorizables_lib", serialize(no_valorizables_lib, NULL))
# redis_conn$SET("valorizables_lib", serialize(valorizables_lib, NULL))
# redis_conn$SET("congestionamiento1_lib", serialize(congestionamiento1_lib, NULL))
# redis_conn$SET("congestionamiento2_lib", serialize(congestionamiento2_lib, NULL))
# redis_conn$SET("congestionamiento3_lib", serialize(congestionamiento3_lib, NULL))
# redis_conn$SET("congestionamiento4_lib", serialize(congestionamiento4_lib, NULL))
# redis_conn$SET("congestionamiento5_lib", serialize(congestionamiento5_lib, NULL))
# redis_conn$SET("congestionamiento_lib", serialize(congestionamiento_lib, NULL))
# redis_conn$SET("accidentes_lib", serialize(accidentes_lib, NULL))
# redis_conn$SET("RVN_lib", serialize(RVN_lib, NULL))
# redis_conn$SET("puentes_lib", serialize(puentes_lib, NULL))
# redis_conn$SET("UGM_sc", serialize(UGM_sc, NULL))
# redis_conn$SET("infraestructura_sc", serialize(infraestructura_sc, NULL))
# redis_conn$SET("asentamientos_sc", serialize(asentamientos_sc, NULL))
# redis_conn$SET("transformadores_sc", serialize(transformadores_sc, NULL))
# redis_conn$SET("postes_sc", serialize(postes_sc, NULL))
# redis_conn$SET("congestionamiento1_sc", serialize(congestionamiento1_sc, NULL))
# redis_conn$SET("congestionamiento2_sc", serialize(congestionamiento2_sc, NULL))
# redis_conn$SET("congestionamiento3_sc", serialize(congestionamiento3_sc, NULL))
# redis_conn$SET("congestionamiento4_sc", serialize(congestionamiento4_sc, NULL))
# redis_conn$SET("congestionamiento5_sc", serialize(congestionamiento5_sc, NULL))
# redis_conn$SET("congestionamiento_sc", serialize(congestionamiento_sc, NULL))
# redis_conn$SET("accidentes_sc", serialize(accidentes_sc, NULL))
# redis_conn$SET("RVN_sc", serialize(RVN_sc, NULL))
# redis_conn$SET("puentes_sc", serialize(puentes_sc, NULL))
# redis_conn$SET("Agua_sc", serialize(Agua_sc, NULL))
# redis_conn$SET("novalorizables_map_sc", serialize(novalorizables_map_sc, NULL))
# redis_conn$SET("UGM_lim", serialize(UGM_lim, NULL))
# redis_conn$SET("asentamientos_lim", serialize(asentamientos_lim, NULL))
# redis_conn$SET("inundaciones_lim", serialize(inundaciones_lim, NULL))
# redis_conn$SET("infraestructura_lim", serialize(infraestructura_lim, NULL))
# redis_conn$SET("medidores_lim", serialize(medidores_lim, NULL))
# redis_conn$SET("transformadores_lim", serialize(transformadores_lim, NULL))
# redis_conn$SET("postes_lim", serialize(postes_lim, NULL))
# redis_conn$SET("Agua_lim", serialize(Agua_lim, NULL))
# redis_conn$SET("no_valorizables_lim", serialize(no_valorizables_lim, NULL))
# redis_conn$SET("congestionamiento1_lim", serialize(congestionamiento1_lim, NULL))
# redis_conn$SET("congestionamiento2_lim", serialize(congestionamiento2_lim, NULL))
# redis_conn$SET("congestionamiento3_lim", serialize(congestionamiento3_lim, NULL))
# redis_conn$SET("congestionamiento4_lim", serialize(congestionamiento4_lim, NULL))
# redis_conn$SET("congestionamiento5_lim", serialize(congestionamiento5_lim, NULL))
# redis_conn$SET("congestionamiento_lim", serialize(congestionamiento_lim, NULL))
# redis_conn$SET("accidentes_lim", serialize(accidentes_lim, NULL))
# redis_conn$SET("RVN_lim", serialize(RVN_lim, NULL))
# redis_conn$SET("puentes_lim", serialize(puentes_lim, NULL))
# redis_conn$SET("Agua_guap", serialize(Agua_guap, NULL))
# redis_conn$SET("novalorizables_sc", serialize(novalorizables_sc, NULL))
# 
