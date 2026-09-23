


# 1. Cargar librerías
install.packages(networkD3)
install.packages("visNetwork")
library(tidyverse)
library(visNetwork)

# 2. Configurar Nodos (visNetwork requiere columnas específicas: 'id', 'label', 'group')
nodos_vis <- tibble(
  id = 1:12,
  label = c("Recicladores Base\nBajos de Mena", "Punto Limpio\nPlaza Puente Alto", 
            "Municipalidad\nPuente Alto", "DIMAO\nPuente Alto", "CMPC\n(Empresa)", 
            "Juntas de Vecinos", "Ferias Libres", "Material\nPET/Cartón", 
            "Rutas de\nRecolección", "Condiciones\nLaborales", 
            "Subsidio\nMunicipal", "Trazabilidad\nAmbiental"),
  group = c("Social", "Infraestructura", "Institucional", "Institucional",
            "Económico", "Social", "Social", "Material", 
            "Flujo", "Atributo", "Incentivo", "Información"),
  # El tamaño del nodo puede mutar según su peso o importancia teórica
  size = c(30, 25, 35, 30, 35, 20, 20, 25, 25, 20, 20, 25)
)

# 3. Configurar Enlaces (Añadimos 'value' para el grosor de la asimetría de poder)
enlaces_vis <- tibble(
  from = c(1, 1, 1, 2, 4, 3, 6, 7, 1, 5, 8, 9),
  to   = c(2, 6, 10, 4, 3, 11, 2, 1, 9, 8, 12, 12),
  # Grosor del enlace: Visibiliza relaciones fuertes (3) vs débiles o precarizadas (1)
  value = c(1, 2, 1, 3, 3, 3, 1, 2, 2, 3, 2, 1),
  # Título flotante que aparece al pasar el mouse sobre la línea
  title = c("Vínculo débil", "Tejido comunitario", "Demanda laboral", 
            "Línea institucional", "Gobernanza formal", "Financiamiento",
            "Uso de espacio", "Suministro directo", "Flujo físico",
            "Convenio industrial", "Monitoreo", "Flujo de datos")
)

# 4. Construir la red interactiva con físicas fluidas
rizoma_profesional <- visNetwork(nodos_vis, enlaces_vis, 
                                 main = "Rizoma Socio-Técnico de Reciclaje: Puente Alto",
                                 submain = "Modelo dinámico de gobernanza y asimetrías territoriales") %>%
  # Elegimos un layout de fuerzas suaves
  visPhysics(solver = "forceAtlas2Based", 
             forceAtlas2Based = list(gravitationalConstant = -50)) %>%
  # Opciones visuales y leyendas automáticas por grupo
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLegend(main = "Dimensiones") %>%
  visInteraction(navigationButtons = TRUE, zoomView = TRUE, dragNodes = TRUE)

# 5. DESPLEGAR EN RSTUDIO
rizoma_profesional

# 6. SOLUCIÓN AL ERROR DE GUARDADO (Usando ruta absoluta limpia)
# Forzamos a R a entender la ruta exacta para que htmlwidgets no falle
ruta_salida <- file.path(getwd(), "visualization", "rizoma_interactivo_pa.html")

# Si por alguna razón no creaste la carpeta 'visualization' previamente, R la crea aquí:
if(!dir.exists("visualization")) dir.create("visualization")

htmlwidgets::saveWidget(rizoma_profesional, file = ruta_salida, selfcontained = TRUE)
