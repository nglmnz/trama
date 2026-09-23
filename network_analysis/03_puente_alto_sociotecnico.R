# 1. Instalar y cargar librerías
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, visNetwork, htmlwidgets)

# ==========================================
# DATA INTERNA DE PUENTE ALTO (LÓGICA TERRITORIAL)
# ==========================================

# Base de datos de Cuadrantes Territoriales
data_cuadrantes <- tibble(
  cuadrante = c("Cuadrante 1: Centro-Norte (Plaza/Vicuña)", 
                "Cuadrante 2: Bajos de Mena / Sur-Poniente", 
                "Cuadrante 3: San Carlos / Oriente (Tobalaba)", 
                "Cuadrante 4: Rural / Extensión Sur (Nocedal/Cajón)"),
  ton_generadas_mes = c(8500, 6200, 4800, 500),
  recicladores_activos = c(45, 75, 30, 10),
  tasa_recuperacion = c("2.4%", "1.1%", "4.8%", "0.5%")
)

# Base de datos de Actores y Cumplimiento Ley REP
data_actores_rep <- tibble(
  tipo_actor = c("Domiciliario", "Empresa/Comercio (Sujeto REP)", "Organizaciones/Instituciones"),
  ton_anuales = c(185000, 45000, 10000),
  estado_trazabilidad = c("Baja (Mezclado en camión DIMAO)", "Media-Alta (Declarado en Ventanilla Única RETC)", "Baja-Media (Esfuerzos locales)"),
  cumplimiento_rep = c("No aplica directamente", "25% de la meta de valorización 2026", "Apoya piloto de orgánicos")
)

# ==========================================
# INYECCIÓN DE DATOS EN LOS NODOS DEL RIZOMA
# ==========================================

nodos_vis <- tibble(
  id = 1:14,
  label = c(
    "Recicladores de base", "Puntos limpios / jaulas", "Municipalidad de Puente Alto", 
    "Empresas generadoras", "Organizaciones comunitarias", "Gestores de residuos",
    "Materiales recuperados", "Volúmenes y pesos", "Rutas de recolección", 
    "Condiciones laborales", "Incentivos", "Trazabilidad", 
    "Datos territoriales", "Impacto ambiental y social"
  ),
  group = c(
    "Humano/Actor", "Infraestructura", "Institucional", 
    "Institucional", "Humano/Actor", "Institucional",
    "Flujo/Material", "Métrica/Dato", "Flujo/Material", 
    "Dimensión Social", "Métrica/Dato", "Métrica/Dato", 
    "Métrica/Dato", "Dimensión Social"
  ),
  # Construimos los Tooltips dinámicamente usando HTML con los datos de Puente Alto
  title = c(
    paste0("<b>Recicladores de Base:</b><br>",
           "• Total Comunal: ", sum(data_cuadrantes$recicladores_activos), " recicladores catastrados.<br>",
           "• Mayor concentración: Bajos de Mena (", data_cuadrantes$recicladores_activos[2], " activos).<br>",
           "• Rol: Capturan el cartón y PET antes del camión municipal."),
    
    "<b>Puntos Limpios y Estaciones:</b><br>• 4 Puntos Limpios de alta densidad y +120 jaulas vecinales de metal para botellas plásticas.",
    
    paste0("<b>Municipalidad (DIMAO):</b><br>",
           "• Basura Comunal Total: ~<b>240,000 Ton/año</b>.<br>",
           "• Gasto logístico: Uno de los ítems presupuestarios más altos de la comuna.<br>",
           "• Meta: Desviar el 10% de residuos domiciliarios para 2030."),
    
    paste0("<b>Empresas Generadoras (Comercio/Mall):</b><br>",
           "• Generación: ", data_actores_rep$ton_anuales[2], " Ton/año.<br>",
           "• Regulación: Ley 20.920 (REP). Malls como Plaza Tobalaba e industrias locales deben financiar sistemas de gestión."),
    
    "<b>Organizaciones Comunitarias:</b><br>• Juntas de vecinos liderando el compostaje en huertos urbanos y el acopio de vidrio.",
    
    "<b>Gestores de Residuos:</b><br>• Empresas de transporte y tolvas autorizadas. Conectan los puntos industriales de la comuna con las plantas de Santiago Quilicura/Pudahuel.",
    
    "<b>Materiales Recuperados:</b><br>• Predomina el Cartón (62%), seguido por PET/Plásticos rígidos (21%), Vidrio (12%) y Latas de aluminio (5%).",
    
    paste0("<b>Volúmenes por Cuadrante (Mensual):</b><br>",
           "• C1 (Plaza): ", data_cuadrantes$ton_generadas_mes[1], " Ton<br>",
           "• C2 (Bajos de Mena): ", data_cuadrantes$ton_generadas_mes[2], " Ton<br>",
           "• C3 (San Carlos): ", data_cuadrantes$ton_generadas_mes[3], " Ton<br>",
           "• C4 (Rural): ", data_cuadrantes$ton_generadas_mes[4], " Ton"),
    
    "<b>Rutas de Recolección:</b><br>• Optimización de micro-rutas para el triciclo/carro de recicladores, evitando competir con el camión recolector tradicional.",
    
    "<b>Condiciones Laborales:</b><br>• Brecha crítica: El 68% opera en informalidad en las calles. Urge un sistema de pago por servicio de recolección.",
    
    "<b>Incentivos:</b><br>• Piloto de 'Eco-puntos' canjeables por beneficios municipales o rebajas en los derechos de aseo domiciliario.",
    
    paste0("<b>Trazabilidad del Flujo:</b><br>",
           "• Domiciliario: ", data_actores_rep$estado_trazabilidad[1], "<br>",
           "• Industrial/REP: ", data_actores_rep$estado_trazabilidad[2]),
    
    "<b>Datos Territoriales (SIG):</b><br>• Cruce crítico: Mapeo de microbasurales vs ubicaciones de tolvas comunitarias para erradicación de focos.",
    
    "<b>Impacto Ambiental y Social:</b><br>• Reducción de huella de carbono comunal y formalización del empleo vulnerable (Sindicatos de recicladores)."
  )
)

# ==========================================
# CONEXIONES Y RENDERIZADO (VISNETWORK)
# ==========================================

enlaces_nombres <- tribble(
  ~from_name, ~to_name, ~label,
  "Recicladores de base", "Puntos limpios / jaulas", "Operan / Abastecen",
  "Recicladores de base", "Condiciones laborales", "Determinan",
  "Recicladores de base", "Rutas de recolección", "Recorren",
  "Puntos limpios / jaulas", "Volúmenes y pesos", "Registran",
  "Puntos limpios / jaulas", "Municipalidad de Puente Alto", "Co-gestionan",
  "Municipalidad de Puente Alto", "Datos territoriales", "Planifican con",
  "Municipalidad de Puente Alto", "Incentivos", "Regulan",
  "Organizaciones comunitarias", "Puntos limpios / jaulas", "Sostienen",
  "Organizaciones comunitarias", "Incentivos", "Reciben",
  "Empresas generadoras", "Gestores de residuos", "Contratan",
  "Empresas generadoras", "Trazabilidad", "Reportan (Ley REP)",
  "Gestores de residuos", "Materiales recuperados", "Procesan",
  "Materiales recuperados", "Volúmenes y pesos", "Se miden en",
  "Volúmenes y pesos", "Trazabilidad", "Insuman",
  "Rutas de recolección", "Datos territoriales", "Se mapean",
  "Trazabilidad", "Impacto ambiental y social", "Evidencian",
  "Condiciones laborales", "Impacto ambiental y social", "Alimentan",
  "Incentivos", "Recicladores de base", "Movilizan"
)

enlaces_vis <- enlaces_nombres %>%
  left_join(nodos_vis %>% select(id, label), by = c("from_name" = "label")) %>%
  rename(from = id) %>%
  left_join(nodos_vis %>% select(id, label), by = c("to_name" = "label")) %>%
  rename(to = id) %>%
  select(from, to, label)

# Crear el grafo interactivo avanzado
mapa_puente_alto <- visNetwork(nodos_vis, enlaces_vis, 
                               main = "Sistema Socio-Técnico de Reciclaje",
                               submain = "Plataforma de Trazabilidad Territorial - Puente Alto") %>%
  visGroups(groupname = "Humano/Actor", color = "#FC8D62", shape = "dot") %>%
  visGroups(groupname = "Infraestructura", color = "#66C2A5", shape = "database") %>%
  visGroups(groupname = "Institucional", color = "#8DA0CB", shape = "hexagon") %>%
  visGroups(groupname = "Flujo/Material", color = "#E78AC3", shape = "diamond") %>%
  visGroups(groupname = "Métrica/Dato", color = "#A6D854", shape = "icon") %>%
  visGroups(groupname = "Dimensión Social", color = "#FFD92F", shape = "star") %>%
  visNodes(size = 30, shadow = TRUE, font = list(face = "Helvetica", size = 15)) %>%
  visEdges(color = list(color = "#d3d3d3", highlight = "#3498db"), 
           smooth = list(type = "curvedND"), width = 2) %>%
  visPhysics(solver = "barnesHut", barnesHut = list(gravitationalConstant = -4000)) %>%
  visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE),
             nodesIdSelection = list(enabled = TRUE, main = "Buscar Variable Comunal")) %>%
  visInteraction(navigationButtons = TRUE, tooltipDelay = 100)

# Exportar el ecosistema de datos
saveWidget(mapa_puente_alto, file = "plataforma_trazabilidad_puente_alto.html", selfcontained = TRUE)
mapa_puente_alto
