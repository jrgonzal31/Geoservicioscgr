# Cargar librerías necesarias
library(dplyr)
library(readr)
library(ineq)
library(sf)# Para el cálculo del índice de Gini

# Cargar datos
PerezZeledon_with_clusters_ward <- read_csv("datos/PZ/datos/PerezZeledon_with_clusters_ward.csv")
PerezZeledon_clustering <- read_csv("datos/PZ/datos/PerezZeledon_clustering.csv")
UGM_pz <- st_read("datos/PZ/datos/PZ  UGM cortadas.geojson")  # Cargar directamente el archivo base

# Unir y limpiar datos
UGM_pz <- UGM_pz %>%
  select(ID_MGN, geometry) %>%
  full_join(PerezZeledon_with_clusters_ward, by = "ID_MGN") %>%
  filter(!is.na(cluster)) %>%
  select(ID_MGN, everything(), -geometry) %>%
  select(everything(), geometry)

# Filtrar variables
variables_filtradas_pz <- c('cluster', 'Telecomunicaciones_4Gvehiculos',
                            'Telecomunicaciones_4Gexteriores', 'cantidad_empresas', "Telecomunicaciones_4Ginteriores",
                            "Residuos_noValorizables", "Movilidad_accidentes_Calle_buen_estado",
                            "Movilidad_accidentes_buenTiempo", "Movilidad_accidentes_pavimento",
                            "Agua_conexiones_edific", "Saneamiento_conexiones_edific",
                            "Electricidad_medidoresint_edific", "Electricidad_postes_edific",
                            "Electricidad_transformadores_edific", "Movilidad_congestionamiento", "inundaciones")

# Normalización y modificación
UGM_escala_pz <- as.data.frame(UGM_pz) %>%
  select(-geometry) %>%
  mutate(Movilidad_congestionamiento = case_when(
    Movilidad_congestionamiento == 0 ~ 6,
    Movilidad_congestionamiento == 1 ~ 5,
    Movilidad_congestionamiento == 2 ~ 4,
    Movilidad_congestionamiento == 3 ~ 3,
    Movilidad_congestionamiento == 4 ~ 2,
    Movilidad_congestionamiento == 5 ~ 1,
    TRUE ~ Movilidad_congestionamiento)) %>%
  mutate(across(
    .cols = !matches(c("Movilidad_accidentes_Calle_buen_estado", "Movilidad_accidentes_buenTiempo", "Movilidad_accidentes_pavimento")),
    .fns = ~ (. - min(., na.rm = TRUE)) / (max(., na.rm = TRUE) - min(., na.rm = TRUE)) * 100
  )) %>%
  mutate(Telecomunicaciones = Telecomunicaciones_4Ginteriores,
         Residuos = Residuos_noValorizables,
         Movilidad =  Movilidad_congestionamiento,
         Agua = Agua_conexiones_edific,
         Saneamiento = Saneamiento_conexiones_edific,
         Electricidad = Electricidad_medidoresint_edific) %>%
  select(-variables_filtradas_pz)

# Calcular índice de Gini
gini_indices_pz <- sapply(UGM_escala_pz[-1], Gini)
gini_df_pz <- data.frame(Variable = names(gini_indices_pz), Gini_Index = gini_indices_pz)

# Guardar datos procesados
write_csv(gini_df_pz, "datos/PZ/datos/gini_df_pz.csv")
saveRDS(UGM_escala_pz, "datos/PZ/datos/UGM_escala_pz.rds")





Puntarenas_with_clusters_ward <- read_csv("datos/Puntarenas/datos/Puntarenas_with_clusters_ward.csv")
Puntarenas_clustering <- read_csv("datos/Puntarenas/datos/Puntarenas_clustering.csv")
clusters_ward_punt <- Puntarenas_with_clusters_ward$cluster
UGM_punt <- st_read("datos/Puntarenas/datos/Punt UGM cortada.geojson")


UGM_punt <- UGM_punt %>%
  select(ID_MGN, geometry) %>%
  full_join(Puntarenas_with_clusters_ward, by = "ID_MGN") %>%
  filter(!is.na(cluster)) %>%
  select(ID_MGN, everything(), -geometry) %>%
  select(everything(), geometry)

variables_filtradas_punt <- c('cluster', 'Telecomunicaciones_4Gvehiculos',
                              'Telecomunicaciones_4Gexteriores', 'cantidad_empresas', "Telecomunicaciones_4Ginteriores",
                              "Movilidad_accidentes_Calle_buen_estado", "Movilidad_accidentes_buenTiempo", "Movilidad_accidentes_pavimento",
                              "Agua_conexiones_edific", "Saneamiento_conexiones_edific", "Saneamiento_Sinconexiones",
                              "Movilidad_congestionamiento", "Electricidad_medidoresint_edific", "Electricidad_postes_edific",
                              "Electricidad_transformadores_edific")

UGM_escala_punt <- as.data.frame(UGM_punt) %>%
  select(-geometry) %>%
  mutate(Movilidad_congestionamiento = case_when(
    Movilidad_congestionamiento == 0 ~ 6,
    Movilidad_congestionamiento == 1 ~ 5,
    Movilidad_congestionamiento == 2 ~ 4,
    Movilidad_congestionamiento == 3 ~ 3,
    Movilidad_congestionamiento == 4 ~ 2,
    Movilidad_congestionamiento == 5 ~ 1,
    TRUE ~ Movilidad_congestionamiento)) %>%
  mutate(across(
    .cols = !matches(c("Movilidad_accidentes_Calle_buen_estado", "Movilidad_accidentes_buenTiempo", "Movilidad_accidentes_pavimento")),
    .fns = ~ (. - min(., na.rm = TRUE)) / (max(., na.rm = TRUE) - min(., na.rm = TRUE)) * 100
  )) %>%
  mutate(Telecomunicaciones = Telecomunicaciones_4Ginteriores,
         Movilidad =  Movilidad_congestionamiento,
         Agua = Agua_conexiones_edific,
         Saneamiento = Saneamiento_conexiones_edific,
         Electricidad = Electricidad_medidoresint_edific) %>%
  select(-variables_filtradas_punt)

gini_indices_punt <- sapply(UGM_escala_punt[-1], Gini)

gini_df_punt <- data.frame(Variable = names(gini_indices_punt), Gini_Index = gini_indices_punt)

write_csv(gini_df_punt, "datos/Puntarenas/datos/gini_df_punt.csv")
saveRDS(UGM_escala_punt, "datos/Puntarenas/datos/UGM_escala_punt.rds")





# Cargar datos
Guapiles_with_clusters_ward <- read_csv("datos/Pococi/datos/Pococi_with_clusters_ward.csv")
Guapiles_clustering <- read_csv("datos/Pococi/datos/Pococi_clustering.csv")
clusters_ward_guap <- Guapiles_with_clusters_ward$cluster
UGM_guap <- st_read("datos/Pococi/datos/Guap UGM cortado.geojson")

# Índice de Gini

UGM_guap <- UGM_guap %>%
  select(ID_MGN, geometry) %>%
  full_join(Guapiles_with_clusters_ward, by = "ID_MGN") %>%
  filter(!is.na(cluster)) %>%
  select(ID_MGN, everything(), -geometry) %>%
  select(everything(), geometry)

variables_filtradas_guap <- c('cluster', 'Telecomunicaciones_4Gvehiculos',
                              'Telecomunicaciones_4Gexteriores', 'cantidad_empresas', "Telecomunicaciones_4Ginteriores",
                              "Movilidad_accidentes_Calle_buen_estado", "Movilidad_accidentes_buenTiempo", "Movilidad_accidentes_pavimento",
                              "Agua_conexiones_edific", "Movilidad_congestionamiento", "Electricidad_medidoresint_edific", "Electricidad_postes_edific",
                              "Electricidad_transformadores_edific", "Residuos_recoleccion")

UGM_escala_guap <- as.data.frame(UGM_guap) %>%
  select(-geometry) %>%
  mutate(Movilidad_congestionamiento = case_when(
    Movilidad_congestionamiento == 0 ~ 6,
    Movilidad_congestionamiento == 1 ~ 5,
    Movilidad_congestionamiento == 2 ~ 4,
    Movilidad_congestionamiento == 3 ~ 3,
    Movilidad_congestionamiento == 4 ~ 2,
    Movilidad_congestionamiento == 5 ~ 1,
    TRUE ~ Movilidad_congestionamiento)) %>%
  mutate(across(
    .cols = !matches(c("Movilidad_accidentes_Calle_buen_estado", "Movilidad_accidentes_buenTiempo", "Movilidad_accidentes_pavimento")),
    .fns = ~ (. - min(., na.rm = TRUE)) / (max(., na.rm = TRUE) - min(., na.rm = TRUE)) * 100
  )) %>%
  mutate(Telecomunicaciones = Telecomunicaciones_4Ginteriores,
         Movilidad = Movilidad_congestionamiento,
         Agua = Agua_conexiones_edific,
         Residuos = Residuos_recoleccion,
         Electricidad = Electricidad_medidoresint_edific) %>%
  select(-variables_filtradas_guap)


gini_indices_guap <- sapply(UGM_escala_guap[-1], Gini)

gini_df_guap <- data.frame(Variable = names(gini_indices_guap), Gini_Index = gini_indices_guap)


write_csv(gini_df_guap, "datos/Pococi/datos/gini_df_guap.csv")
saveRDS(UGM_escala_guap, "datos/Pococi/datos/UGM_escala_guap.rds")



# Cargar datos
Turrialba_with_clusters_ward <- read_csv("datos/Turrialba/datos/Turrialba_with_clusters_ward.csv")
Turrialba_clustering <- read_csv("datos/Turrialba/datos/Turrialba_clustering.csv")
clusters_ward_turri <- Turrialba_with_clusters_ward$cluster
UGM_turri <- st_read("datos/Turrialba/datos/Turri UGM cortado.geojson")



# Índice de Gini

UGM_turri <- UGM_turri %>%
  select(ID_MGN, geometry) %>%
  full_join(Turrialba_with_clusters_ward, by = "ID_MGN") %>%
  filter(!is.na(cluster)) %>%
  select(ID_MGN, everything(), -geometry) %>%
  select(everything(), geometry)

variables_filtradas_turri <- c('cluster', 'Telecomunicaciones_4Gvehiculos',
                               'Telecomunicaciones_4Gexteriores', 'cantidad_empresas', "Telecomunicaciones_4Ginteriores",
                               "Movilidad_accidentes_Calle_buen_estado", "Movilidad_accidentes_buenTiempo", "Movilidad_accidentes_pavimento"
                               , "Movilidad_congestionamiento", "Electricidad_medidoresIntel_edific", "Electricidad_postes_edific",
                               "Electricidad_transform_edific", "RVN", "inundaciones")

UGM_escala_turri <- as.data.frame(UGM_turri) %>%
  select(-geometry) %>%
  mutate(Movilidad_congestionamiento = case_when(
    Movilidad_congestionamiento == 0 ~ 6,
    Movilidad_congestionamiento == 1 ~ 5,
    Movilidad_congestionamiento == 2 ~ 4,
    Movilidad_congestionamiento == 3 ~ 3,
    Movilidad_congestionamiento == 4 ~ 2,
    Movilidad_congestionamiento == 5 ~ 1,
    TRUE ~ Movilidad_congestionamiento)) %>%
  mutate(across(
    .cols = !matches(c("Movilidad_accidentes_Calle_buen_estado", "Movilidad_accidentes_buenTiempo", "Movilidad_accidentes_pavimento")),
    .fns = ~ (. - min(., na.rm = TRUE)) / (max(., na.rm = TRUE) - min(., na.rm = TRUE)) * 100
  )) %>%
  mutate(Telecomunicaciones = Telecomunicaciones_4Ginteriores,
         Movilidad = Movilidad_congestionamiento,
         Electricidad = Electricidad_medidoresIntel_edific) %>%
  select(-variables_filtradas_turri)

gini_indices_turri <- sapply(UGM_escala_turri[-1], Gini)

gini_df_turri <- data.frame(Variable = names(gini_indices_turri), Gini_Index = gini_indices_turri)


write_csv(gini_df_turri, "datos/Turrialba/datos/gini_df_turri.csv")
saveRDS(UGM_escala_turri, "datos/Turrialba/datos/UGM_escala_turri.rds")




# Cargar datos
Liberia_with_clusters_ward <- read_csv("datos/Liberia/datos/Liberia_with_clusters_ward.csv")
Liberia_clustering <- read_csv("datos/Liberia/datos/Liberia_clustering.csv")
clusters_ward_lib <- Liberia_with_clusters_ward$cluster
UGM_lib <- st_read("datos/Liberia/datos/Lib UGM cortado.shp")

# Índice de Gini


UGM_lib <- UGM_lib %>%
  select(ID_MGN, geometry) %>%
  full_join(Liberia_with_clusters_ward, by = "ID_MGN") %>%
  filter(!is.na(cluster)) %>%
  select(ID_MGN, everything(), -geometry) %>%
  select(everything(), geometry)

variables_filtradas_lib <- c('cluster', 'Telecomunicaciones_4Gvehiculos',
                             'Telecomunicaciones_4Gexteriores', 'cantidad_empresas', "Telecomunicaciones_4Ginteriores",
                             "Residuos_noValorizables", "Movilidad_accidentes_Calle_buen_estado",
                             "Movilidad_accidentes_buenTiempo", "Movilidad_accidentes_pavimento",
                             "Agua_conexiones_edific", "saneamiento_conexiones_edific",
                             "Electricidad_medidoresint_edific", "Electricidad_postes_edific",
                             "Electricidad_transformadores_edific", "Movilidad_congestionamiento", "Residuos_Valorizables")

UGM_escala_lib <- as.data.frame(UGM_lib) %>%
  select(-geometry) %>%
  mutate(Movilidad_congestionamiento = case_when(
    Movilidad_congestionamiento == 0 ~ 6,
    Movilidad_congestionamiento == 1 ~ 5,
    Movilidad_congestionamiento == 2 ~ 4,
    Movilidad_congestionamiento == 3 ~ 3,
    Movilidad_congestionamiento == 4 ~ 2,
    Movilidad_congestionamiento == 5 ~ 1,
    TRUE ~ Movilidad_congestionamiento)) %>%
  mutate(across(
    .cols = !matches(c("Movilidad_accidentes_Calle_buen_estado", "Movilidad_accidentes_buenTiempo", "Movilidad_accidentes_pavimento")),
    .fns = ~ (. - min(., na.rm = TRUE)) / (max(., na.rm = TRUE) - min(., na.rm = TRUE)) * 100
  )) %>%
  mutate(Telecomunicaciones = Telecomunicaciones_4Ginteriores,
         Residuos = rowMeans(select(., Residuos_noValorizables, Residuos_Valorizables), na.rm = TRUE),
         Movilidad = Movilidad_congestionamiento,
         Agua = Agua_conexiones_edific,
         Saneamiento = saneamiento_conexiones_edific,
         Electricidad = Electricidad_medidoresint_edific) %>%
  select(-variables_filtradas_lib)

gini_indices_lib <- sapply(UGM_escala_lib[-1], Gini)

gini_df_lib <- data.frame(Variable = names(gini_indices_lib), Gini_Index = gini_indices_lib)


write_csv(gini_df_lib, "datos/Liberia/datos/gini_df_lib.csv")
saveRDS(UGM_escala_lib, "datos/Liberia/datos/UGM_escala_lib.rds")




# Cargar datos
SanCarlos_with_clusters_ward <- read_csv("datos/SanCarlos/datos/SanCarlos_with_clusters_ward.csv")
SanCarlos_clustering <- read_csv("datos/SanCarlos/datos/SanCarlos_clustering.csv")
clusters_ward_sc <- SanCarlos_with_clusters_ward$cluster
UGM_sc <- st_read("datos/SanCarlos/datos/SC UGM cortada.geojson")


UGM_sc <- UGM_sc %>%
  select(ID_MGN, geometry) %>%
  full_join(SanCarlos_with_clusters_ward, by = "ID_MGN") %>%
  filter(!is.na(cluster)) %>%
  select(ID_MGN, everything(), -geometry) %>%
  select(everything(), geometry)

variables_filtradas <- c('cluster', 'Telecomunicaciones_4Gvehiculos',
                         'Telecomunicaciones_4Gexteriores', 'cantidad_empresas', "Telecomunicaciones_4Ginteriores", "Movilidad_accidentes_Calle_buen_estado", "Movilidad_accidentes_buenTiempo", "Movilidad_accidentes_pavimento", "conexiones_agua_edific", "postes_edific", "transformadores_edific", "Movilidad_congestionamiento")


UGM_escala_sc <- as.data.frame(UGM_sc) %>%
  select(-geometry) %>%
  mutate(Movilidad_congestionamiento = case_when(
    Movilidad_congestionamiento == 0 ~ 6,
    Movilidad_congestionamiento == 1 ~ 5,
    Movilidad_congestionamiento == 2 ~ 4,
    Movilidad_congestionamiento == 3 ~ 3,
    Movilidad_congestionamiento == 4 ~ 2,
    Movilidad_congestionamiento == 5 ~ 1,
    TRUE ~ Movilidad_congestionamiento)) %>%
  mutate(across(
    .cols = !matches(c("Movilidad_accidentes_Calle_buen_estado", "Movilidad_accidentes_buenTiempo", "Movilidad_accidentes_pavimento")),
    .fns = ~ (. - min(., na.rm = TRUE)) / (max(., na.rm = TRUE) - min(., na.rm = TRUE)) * 100
  )) %>%
  mutate(Telecomunicaciones = Telecomunicaciones_4Ginteriores,
         Movilidad = Movilidad_congestionamiento,
         Agua = conexiones_agua_edific) %>%
  select(-variables_filtradas)

gini_indices_sc <- sapply(UGM_escala_sc[-1], Gini)

gini_df_sc <- data.frame(Variable = names(gini_indices_sc), Gini_Index = gini_indices_sc)


write_csv(gini_df_sc, "datos/SanCarlos/datos/gini_df_sc.csv")
saveRDS(UGM_escala_sc, "datos/SanCarlos/datos/UGM_escala_sc.rds")





# Cargar datos
Limon_with_clusters_ward <- read_csv("datos/Limon/datos/Limon_with_clusters_ward.csv")
Limon_clustering <- read_csv("datos/Limon/datos/Limon_clustering.csv")
clusters_ward_lim <- Limon_with_clusters_ward$cluster
UGM_lim <- st_read("datos/Limon/datos/Lim UGM cortado.geojson")

# Índice de Gini para Pérez Zeledón

UGM_lim <- UGM_lim %>%
  select(ID_MGN, geometry) %>%
  full_join(Limon_with_clusters_ward, by = "ID_MGN") %>%
  filter(!is.na(cluster)) %>%
  select(ID_MGN, everything(), -geometry) %>%
  select(everything(), geometry)

variables_filtradas_lim <- c('cluster', 'Telecomunicaciones_4Gvehiculos',
                             'Telecomunicaciones_4Gexteriores', 'cantidad_empresas', "Telecomunicaciones_4Ginteriores",
                             "Residuos_noValorizables", "Movilidad_accidentes_Calle_buen_estado",
                             "Movilidad_accidentes_buenTiempo", "Movilidad_accidentes_pavimento",
                             "Agua_conexiones_edific", "saneamiento_conexiones_edific",
                             "Electricidad_medidoresint_edific", "Electricidad_postes_edific",
                             "Electricidad_transformadores_edific", "Movilidad_congestionamiento", "Movilidad_RVN")

UGM_escala_lim <- as.data.frame(UGM_lim) %>%
  select(-geometry) %>%
  mutate(Movilidad_congestionamiento = case_when(
    Movilidad_congestionamiento == 0 ~ 6,
    Movilidad_congestionamiento == 1 ~ 5,
    Movilidad_congestionamiento == 2 ~ 4,
    Movilidad_congestionamiento == 3 ~ 3,
    Movilidad_congestionamiento == 4 ~ 2,
    Movilidad_congestionamiento == 5 ~ 1,
    TRUE ~ Movilidad_congestionamiento)) %>%
  mutate(across(
    .cols = !matches(c("Movilidad_accidentes_Calle_buen_estado", "Movilidad_accidentes_buenTiempo", "Movilidad_accidentes_pavimento")),
    .fns = ~ (. - min(., na.rm = TRUE)) / (max(., na.rm = TRUE) - min(., na.rm = TRUE)) * 100
  )) %>%
  mutate(Telecomunicaciones = Telecomunicaciones_4Ginteriores,
         Residuos = Residuos_noValorizables,
         Movilidad = Movilidad_congestionamiento,
         Agua = Agua_conexiones_edific,
         Saneamiento = saneamiento_conexiones_edific,
         Electricidad = Electricidad_medidoresint_edific) %>%
  select(-variables_filtradas_lim)

gini_indices_lim <- sapply(UGM_escala_lim[-1], Gini)

gini_df_lim <- data.frame(Variable = names(gini_indices_lim), Gini_Index = gini_indices_lim)


write_csv(gini_df_lim, "datos/Limon/datos/gini_df_lim.csv")
saveRDS(UGM_escala_lim, "datos/Limon/datos/UGM_escala_lim.rds")

