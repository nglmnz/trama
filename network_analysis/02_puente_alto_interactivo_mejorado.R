
# MAPA ESTATICO -----------------------------------------------------------


# 1. Instalar y cargar librerías necesarias
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, tidygraph, ggraph, networkD3, htmlwidgets)

# 2. Definición de Nodos adaptados al contexto de Puente Alto
nodos <- tibble(
  name = c(
    "Recicladores de base", "Puntos limpios / jaulas", "Municipalidad de Puente Alto", 
    "Empresas generadoras", "Organizaciones comunitarias", "Gestores de residuos",
    "Materiales recuperados", "Volúmenes y pesos", "Rutas de recolección", 
    "Condiciones laborales", "Incentivos", "Trazabilidad", 
    "Datos territoriales", "Impacto ambiental y social"
  ),
  tipo = c(
    "Humano/Actor", "Infraestructura", "Institucional", 
    "Institucional", "Humano/Actor", "Institucional",
    "Flujo/Material", "Métrica/Dato", "Flujo/Material", 
    "Dimensión Social", "Métrica/Dato", "Métrica/Dato", 
    "Métrica/Dato", "Dimensión Social"
  ),
  # Esta será la información que se desplegará al interactuar con el nodo
  definicion = c(
    "Recicladores de base: Sindicatos y recolectores independientes clave en sectores como Bajos de Mena y el centro de la comuna.",
    "Puntos limpios / jaulas: Estaciones de reciclaje municipales y comunitarias distribuidas en plazas y avenidas principales de Puente Alto.",
    "Municipalidad de Puente Alto: Dirección de Medio Ambiente, Aseo y Ornato (DIMAO). Coordina normativas locales y licitaciones.",
    "Empresas generadoras: Comercio local, supermercados y malls (ej. Plaza Tobalaba) obligados a declarar por Ley REP.",
    "Organizaciones comunitarias: Juntas de vecinos y clubes de adulto mayor que articulan la educación ambiental a nivel barrial.",
    "Gestores de residuos: Empresas autorizadas que retiran, compactan y transportan los materiales hacia plantas de valorización.",
    "Materiales recuperados: PET, cartón, vidrio y latas recolectados en el territorio puentealtino.",
    "Volúmenes y pesos: Datos métricos de toneladas recuperadas mensualmente en la comuna.",
    "Rutas de recolección: Circuitos de los camiones municipales y recorridos cotidianos de los recicladores de base.",
    "Condiciones laborales: Acceso a elementos de protección, formalización, contratos y seguridad en las calles de la comuna.",
    "Incentivos: Programas de rebaja de derechos de aseo, eco-monedas o reconocimiento municipal por reciclar.",
    "Trazabilidad: Monitoreo que asegura que el residuo del vecino de Puente Alto realmente termine reciclado.",
    "Datos territoriales: Capas GIS de la comuna, microbasurales identificados, densidad poblacional y cuadrantes de retiro.",
    "Impacto ambiental y social: Toneladas de CO2 evitadas y dignificación del empleo verde en la zona sur de Santiago."
  )
)

# Convertir IDs a base 0 para networkD3
nodos <- nodos %>% mutate(id = row_number() - 1)

# 3. Matriz de Enlaces (Las conexiones del Rizoma)
enlaces_nombres <- tribble(
  ~from, ~to, ~relacion,
  "Recicladores de base", "Puntos limpios / jaulas", "Operan / Abastecen",
  "Recicladores de base", "Condiciones laborales", "Determinan calidad de vida",
  "Recicladores de base", "Rutas de recolección", "Recorren cotidianamente",
  "Puntos limpios / jaulas", "Volúmenes y pesos", "Registran flujos",
  "Puntos limpios / jaulas", "Municipalidad de Puente Alto", "Co-gestionan / Autorizan",
  "Municipalidad de Puente Alto", "Datos territoriales", "Planifican con",
  "Municipalidad de Puente Alto", "Incentivos", "Regulan / Subvencionan",
  "Organizaciones comunitarias", "Puntos limpios / jaulas", "Sostienen / Activan",
  "Organizaciones comunitarias", "Incentivos", "Reciben / Distribuyen",
  "Empresas generadoras", "Gestores de residuos", "Contratan retiro",
  "Empresas generadoras", "Trazabilidad", "Reportan cumplimiento (Ley REP)",
  "Gestores de residuos", "Materiales recuperados", "Clasifican / Procesan",
  "Materiales recuperados", "Volúmenes y pesos", "Se cuantifican en",
  "Volúmenes y pesos", "Trazabilidad", "Insuman datos a",
  "Rutas de recolección", "Datos territoriales", "Se mapean espacialmente",
  "Trazabilidad", "Impacto ambiental y social", "Evidencian el",
  "Condiciones laborales", "Impacto ambiental y social", "Alimentan el eje social",
  "Incentivos", "Recicladores de base", "Movilizan la base de"
)

# Convertir los nombres de los enlaces a los IDs numéricos que requiere networkD3
enlaces <- enlaces_nombres %>%
  left_join(nodos %>% select(name, id), by = c("from" = "name")) %>%
  rename(source = id) %>%
  left_join(nodos %>% select(name, id), by = c("to" = "name")) %>%
  rename(target = id)

## ==============================================================================
# 4. EXPORTAR 1: GRÁFICO ESTÁTICO MEJORADO
# ==============================================================================

library(ggraph)
library(tidygraph)
library(ggplot2)
library(dplyr)
library(stringr)
library(grid)

# ==============================================================================
# 4.1 PREPARAR NODOS
# ==============================================================================

nodos_grafico <- nodos %>%
  mutate(
    etiqueta = stringr::str_wrap(name, width = 20)
  )


# ==============================================================================
# 4.2 CREAR GRAFO
# ==============================================================================

red_tidy <- tidygraph::tbl_graph(
  nodes = nodos_grafico,
  edges = enlaces_nombres,
  directed = FALSE
)


# ==============================================================================
# 4.3 OBTENER COORDENADAS DEL LAYOUT
# ==============================================================================

# Creamos el layout Fruchterman-Reingold
# y lo guardamos para poder reutilizar las coordenadas.

layout_red <- ggraph::create_layout(
  red_tidy,
  layout = "fr"
)


# ==============================================================================
# 4.4 CREAR COORDENADAS PARA LAS ETIQUETAS DE LAS RELACIONES
# ==============================================================================

# Obtenemos las coordenadas de origen y destino de cada enlace.

coordenadas_nodos <- layout_red %>%
  as.data.frame() %>%
  mutate(id = row_number() - 1) %>%
  select(id, x, y)


# Coordenadas de origen
relaciones_grafico <- enlaces %>%
  left_join(
    coordenadas_nodos,
    by = c("source" = "id")
  ) %>%
  rename(
    x_inicio = x,
    y_inicio = y
  ) %>%
  
  # Coordenadas de destino
  left_join(
    coordenadas_nodos,
    by = c("target" = "id")
  ) %>%
  rename(
    x_final = x,
    y_final = y
  ) %>%
  
  # Punto medio de cada relación
  mutate(
    x_medio = (x_inicio + x_final) / 2,
    y_medio = (y_inicio + y_final) / 2
  )


# ==============================================================================
# 4.5 CREAR EL GRÁFICO
# ==============================================================================

grafico_estatico <- ggraph::ggraph(
  layout_red
) +
  
  # --------------------------------------------------------------------------
# CONEXIONES
# --------------------------------------------------------------------------

ggraph::geom_edge_link(
  colour = "grey70",
  alpha = 0.55,
  width = 0.8
) +
  
  # --------------------------------------------------------------------------
# ETIQUETAS DE LAS RELACIONES
# --------------------------------------------------------------------------

# Las colocamos manualmente en el punto medio de cada conexión.
#
# check_overlap = TRUE evita mostrar etiquetas cuando se superponen
# demasiado. Esto es preferible a tener 17 textos ilegibles.

geom_text(
  data = relaciones_grafico,
  aes(
    x = x_medio,
    y = y_medio,
    label = relacion
  ),
  size = 2.3,
  colour = "grey25",
  fontface = "plain",
  check_overlap = TRUE
) +
  
  # --------------------------------------------------------------------------
# NODOS
# --------------------------------------------------------------------------

ggraph::geom_node_point(
  aes(color = tipo),
  size = 7,
  alpha = 0.95
) +
  
  # --------------------------------------------------------------------------
# ETIQUETAS DE LOS NODOS
# --------------------------------------------------------------------------

ggraph::geom_node_label(
  aes(label = etiqueta),
  repel = TRUE,
  size = 3.1,
  fontface = "bold",
  label.size = 0.25,
  label.padding = grid::unit(0.25, "lines"),
  fill = "white",
  alpha = 0.96,
  lineheight = 0.9
) +
  
  # --------------------------------------------------------------------------
# COLORES
# --------------------------------------------------------------------------

scale_color_brewer(
  palette = "Set2"
) +
  
  # --------------------------------------------------------------------------
# TÍTULOS
# --------------------------------------------------------------------------

labs(
  title = "Ecosistema de Reciclaje — Puente Alto",
  subtitle = "Actores, infraestructuras, flujos, métricas y dimensiones sociales",
  color = "Naturaleza del nodo"
) +
  
  # --------------------------------------------------------------------------
# TEMA
# --------------------------------------------------------------------------

theme_void() +
  
  theme(
    
    plot.title = element_text(
      size = 20,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 5)
    ),
    
    plot.subtitle = element_text(
      size = 11,
      hjust = 0.5,
      colour = "grey35",
      margin = margin(b = 15)
    ),
    
    legend.position = "bottom",
    
    legend.title = element_text(
      face = "bold",
      size = 10
    ),
    
    legend.text = element_text(
      size = 9
    ),
    
    plot.margin = margin(
      25, 40, 25, 40
    )
  )


# ==============================================================================
# 4.6 MOSTRAR
# ==============================================================================

grafico_estatico


# ==============================================================================
# 4.7 EXPORTAR
# ==============================================================================

ggsave(
  filename = "red_reciclaje_puente_alto_v2.png",
  plot = grafico_estatico,
  width = 16,
  height = 12,
  units = "in",
  dpi = 300,
  bg = "white"
)

# INTERACTIVO HTML --------------------------------------------------------


# 1. Instalar y cargar visNetwork y las librerías base
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, visNetwork, htmlwidgets)

# 2. Configuración estética de los Nodos (Alineados al territorio de Puente Alto)
nodos_vis <- tibble(
  id = 1:14,
  label = c(
    "Recicladores de base", "Puntos limpios / jaulas", "Municipalidad de Puente Alto", 
    "Empresas generadoras", "Organizaciones comunitarias", "Gestores de residuos",
    "Materiales recuperados", "Volúmenes y pesos", "Rutas de recolección", 
    "Condiciones laborales", "Incentivos", "Trazabilidad", 
    "Datos territoriales", "Impacto ambiental y social"
  ),
  # El grupo define el color según la paleta moderna 'Set2'
  group = c(
    "Humano/Actor", "Infraestructura", "Institucional", 
    "Institucional", "Humano/Actor", "Institucional",
    "Flujo/Material", "Métrica/Dato", "Flujo/Material", 
    "Dimensión Social", "Métrica/Dato", "Métrica/Dato", 
    "Métrica/Dato", "Dimensión Social"
  ),
  # El título 'title' genera una tarjeta flotante (tooltip) estilizada en HTML
  title = c(
    "<b>Sindicatos y recolectores independientes</b> clave en sectores como Bajos de Mena y el centro de la comuna.",
    "<b>Estaciones de reciclaje municipales y comunitarias</b> distribuidas en plazas y avenidas de Puente Alto.",
    "<b>Dirección de Medio Ambiente, Aseo y Ornato (DIMAO)</b>. Coordina normativas locales y licitaciones.",
    "<b>Comercio local, supermercados y malls</b> (ej. Plaza Tobalaba) obligados a declarar por Ley REP.",
    "<b>Juntas de vecinos y clubes de adulto mayor</b> que articulan la educación ambiental a nivel barrial.",
    "<b>Empresas autorizadas</b> que retiran, compactan y transportan los materiales hacia plantas de valorización.",
    "<b>PET, cartón, vidrio y latas</b> recolectados en el territorio puentealtino.",
    "<b>Datos métricos</b> de toneladas recuperadas mensualmente en la comuna.",
    "<b>Circuitos de los camiones</b> municipales y recorridos cotidianos de los recicladores de base.",
    "<b>Acceso a elementos de protección</b>, formalización, contratos y seguridad en las calles.",
    "<b>Programas de rebaja de derechos de aseo</b>, eco-monedas o reconocimiento municipal.",
    "<b>Monitoreo</b> que asegura que el residuo del vecino realmente termine reciclado.",
    "<b>Capas GIS de la comuna</b>, microbasurales identificados y cuadrantes de retiro.",
    "<b>Ton de CO2 evitadas</b> y dignificación del empleo verde en la zona sur de Santiago."
  )
)

# 3. Configuración de Conexiones (Aristas) suaves y orgánicas
enlaces_nombres <- tribble(
  ~from_name, ~to_name, ~label,
  "Recicladores de base", "Puntos limpios / jaulas", "Operan / Abastecen",
  "Recicladores de base", "Condiciones laborales", "Determinan calidad de vida",
  "Recicladores de base", "Rutas de recolección", "Recorren cotidianamente",
  "Puntos limpios / jaulas", "Volúmenes y pesos", "Registran flujos",
  "Puntos limpios / jaulas", "Municipalidad de Puente Alto", "Co-gestionan / Autorizan",
  "Municipalidad de Puente Alto", "Datos territoriales", "Planifican con",
  "Municipalidad de Puente Alto", "Incentivos", "Regulan / Subvencionan",
  "Organizaciones comunitarias", "Puntos limpios / jaulas", "Sostienen / Activan",
  "Organizaciones comunitarias", "Incentivos", "Reciben / Distribuyen",
  "Empresas generadoras", "Gestores de residuos", "Contratan retiro",
  "Empresas generadoras", "Trazabilidad", "Reportan cumplimiento (Ley REP)",
  "Gestores de residuos", "Materiales recuperados", "Clasifican / Procesan",
  "Materiales recuperados", "Volúmenes y pesos", "Se cuantifican en",
  "Volúmenes y pesos", "Trazabilidad", "Insuman datos a",
  "Rutas de recolección", "Datos territoriales", "Se mapean espacialmente",
  "Trazabilidad", "Impacto ambiental y social", "Evidencian el",
  "Condiciones laborales", "Impacto ambiental y social", "Alimentan el eje social",
  "Incentivos", "Recicladores de base", "Movilizan la base de"
)

# Traducir nombres a los IDs numéricos correspondientes
enlaces_vis <- enlaces_nombres %>%
  left_join(nodos_vis %>% select(id, label), by = c("from_name" = "label")) %>%
  rename(from = id) %>%
  left_join(nodos_vis %>% select(id, label), by = c("to_name" = "label")) %>%
  rename(to = id) %>%
  select(from, to, label)

# 4. Construcción del Mapa de Red Interactivo de Alta Fidelidad
mapa_estilizado <- visNetwork(nodos_vis, enlaces_vis, 
                              main = "Ecosistema de Reciclaje - Puente Alto",
                              submain = "Mapeo Socio-Técnico Rizomático") %>%
  # Elegimos una paleta de colores moderna y pastel, similar a Set2 de RColorBrewer
  visGroups(groupname = "Humano/Actor", color = "#FC8D62") %>%
  visGroups(groupname = "Infraestructura", color = "#66C2A5") %>%
  visGroups(groupname = "Institucional", color = "#8DA0CB") %>%
  visGroups(groupname = "Flujo/Material", color = "#E78AC3") %>%
  visGroups(groupname = "Métrica/Dato", color = "#A6D854") %>%
  visGroups(groupname = "Dimensión Social", color = "#FFD92F") %>%
  # Ajustes globales de nodos (sombras, bordes y fuentes limpias)
  visNodes(shape = "dot", size = 25, 
           shadow = TRUE,
           font = list(face = "Helvetica", size = 16, color = "#2c3e50")) %>%
  # Ajustes globales de enlaces (curvatura física del rizoma y comportamiento del texto)
  visEdges(color = list(color = "#bdc3c7", highlight = "#2980b9"),
           smooth = list(type = "continuous"), # Hace las líneas curvas y suaves
           font = list(align = "horizontal", size = 11, color = "#7f8c8d")) %>%
  # Opciones del motor físico (Fuerza de repulsión elástica)
  visPhysics(solver = "forceAtlas2Based", 
             forceAtlas2Based = list(gravitationalConstant = -50)) %>%
  # Interacción: Resalta los nodos conectados al hacer clic y añade herramientas de navegación
  visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = FALSE),
             nodesIdSelection = list(enabled = TRUE, main = "Seleccionar Actor/Variable")) %>%
  visInteraction(navigationButtons = TRUE, hover = TRUE)

# 5. LINEA DE EXPORTACIÓN INTERACTIVA: Guarda la red como un archivo HTML de nivel profesional
saveWidget(mapa_estilizado, file = "mapa_rizoma_profesional.html", selfcontained = TRUE)

# Para visualizarlo inmediatamente en el panel de RStudio
mapa_estilizado
