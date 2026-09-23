# 1. Cargar librerías
library(tidyverse)
library(leaflet)

# 2. Definir Nodos con Coordenadas Reales de Puente Alto y Fórmulas de Impacto
nodos_geo <- tibble(
  id = 1:6,
  name = c("Recicladores Base Bajos de Mena", 
           "Punto Limpio Plaza Puente Alto", 
           "DIMAO Municipalidad (Concha y Toro)", 
           "Punto Verde Nonato Coo",
           "Comunidad San Gerónimo",
           "Centro de Acopio CMPC"),
  # Coordenadas geográficas en Puente Alto
  lng = c(-70.6120, -70.5755, -70.5790, -70.5480, -70.6010, -70.6250),
  lat = c(-33.6150, -33.6145, -33.5975, -33.6020, -33.6090, -33.5850),
  tipo = c("Social", "Infraestructura", "Institucional", "Infraestructura", "Social", "Económico"),
  
  # Variables para las Fórmulas de Impacto
  toneladas_mes = c(45, 20, 0, 15, 8, 120),       # (TM) Material procesado
  recicladores_activos = c(32, 4, 0, 2, 15, 0),   # (N) Capital humano
  factor_laboral = c(1.8, 1.2, 1.0, 1.1, 1.5, 1.0) # (C) Escala de dignidad laboral
) %>%
  # Aplicación de Fórmulas de Impacto
  mutate(
    impacto_calculado = case_when(
      tipo == "Social" ~ recicladores_activos * factor_laboral * 2, 
      tipo %in% c("Infraestructura", "Económico") ~ toneladas_mes * 0.4, 
      TRUE ~ 15 
    ),
    color_nodo = case_when(
      tipo == "Social" ~ "#2ecc71",        # Verde
      tipo == "Infraestructura" ~ "#3498db", # Azul
      tipo == "Institucional" ~ "#9b59b6",   # Morado
      tipo == "Económico" ~ "#e67e22"        # Naranja
    )
  )

# 3. Definir Enlaces Geográficos
enlaces_geo <- tibble(
  from_id = c(1, 1, 5, 2, 4),
  to_id   = c(2, 6, 1, 3, 6)
) %>%
  left_join(nodos_geo %>% select(id, lng_origen = lng, lat_origen = lat), by = c("from_id" = "id")) %>%
  left_join(nodos_geo %>% select(id, lng_destino = lng, lat_destino = lat), by = c("to_id" = "id"))

# 4. Construir el Mapa Rizomático (USANDO OPENSTREETMAP, LIBRE DE API KEY)
mapa_puente_alto <- leaflet(nodos_geo) %>%
  # addTiles() por defecto carga OpenStreetMap público y 100% gratuito sin keys
  addTiles() %>% 
  
  # Dibujar los Nodos (Círculos proporcionales al impacto)
  addCircleMarkers(
    lng = ~lng, lat = ~lat,
    radius = ~impacto_calculado, 
    color = ~color_nodo,
    fillOpacity = 0.7,
    stroke = TRUE, weight = 2,
    popup = ~paste0("<b>Actor:</b> ", name, "<br>",
                    "<b>Dimensión:</b> ", tipo, "<br>",
                    "<b>Impacto de Radio:</b> ", round(impacto_calculado, 1))
  )

# 5. Dibujar los Enlaces Rizomáticos (Corregido 'enlaces_geo')
for(i in 1:nrow(enlaces_geo)) {
  mapa_puente_alto <- mapa_puente_alto %>%
    addPolylines(
      lng = c(enlaces_geo$lng_origen[i], templates = enlaces_geo$lng_destino[i]),
      lat = c(enlaces_geo$lat_origen[i], templates = enlaces_geo$lat_destino[i]),
      color = "#2c3e50", weight = 3, opacity = 0.6
    )
}

# 6. Desplegar en RStudio
mapa_puente_alto
