
# Comparativa V1 ----------------------------------------------------------


# 1. Instalar y cargar librerías necesarias
if(!require(tidyverse)) install.packages("tidyverse")
library(ggplot2)
library(dplyr)
library(tidyr)

# 2. Construir la estructura de datos comparativa basada en tus notas
# Valores de Cobertura: 
# 0 = No disponible / No aplica
# 1 = Cobertura Parcial / Roadmap o Requiere adaptación
# 2 = Cobertura Completa / Core de la solución
datos_comparativos <- data.frame(
  Etapa_Flujo = c(
    "1. Captura en Ruta (Offline-first)",
    "2. Identificación Ciudadano/Reciclador",
    "3. Evidencia Inmutable (Backend/Fotos)",
    "4. Control de Inventario/Stock",
    "5. Cuenta/Saldo de Beneficios",
    "6. Reportabilidad Fiscal (REP/ESG)"
  ),
  PetCloud = c(2, 1, 2, 1, 0, 2), 
  CicloBem = c(1, 2, 2, 2, 2, 1)
)

# 3. Transformar los datos a formato largo para ggplot2
datos_largos <- datos_comparativos %>%
  pivot_longer(cols = c(PetCloud, CicloBem), 
               names_to = "Plataforma", 
               values_to = "Nivel_Cobertura") %>%
  mutate(
    Etapa_Flujo = factor(Etapa_Flujo, levels = rev(datos_comparativos$Etapa_Flujo)),
    Etiqueta_Texto = case_when(
      Nivel_Cobertura == 2 ~ "Core / Completo",
      Nivel_Cobertura == 1 ~ "Parcial / Roadmap",
      Nivel_Cobertura == 0 ~ "No disponible"
    )
  )

# 4. Crear el mapa de calor comparativo
ggplot(datos_largos, aes(x = Plataforma, y = Etapa_Flujo, fill = as.factor(Nivel_Cobertura))) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = Etiqueta_Texto), color = "black", fontface = "bold", size = 4) +
  scale_fill_manual(
    values = c("2" = "#81c784", "1" = "#fff176", "0" = "#e57373"),
    labels = c("No disponible", "Parcial / En desarrollo", "Core / Implementado de Raíz")
  ) +
  labs(
    title = "Matriz Comparativa de Soluciones Tecnológicas",
    subtitle = "Análisis de cobertura funcional: PetCloud vs. CicloBem",
    x = "Plataforma",
    y = "Etapa del Flujo Operativo",
    fill = "Estado de la Capacidad"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 5)),
    plot.subtitle = element_text(color = "gray40", margin = margin(b = 15)),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.text.y = element_text(size = 11),
    legend.position = "bottom",
    panel.grid = element_blank()
  )


# Comparativa V2 ----------------------------------------------------------

# 1. Instalar y cargar librerías necesarias
if(!require(tidyverse)) install.packages("tidyverse")
library(ggplot2)
library(dplyr)
library(tidyr)

# 2. Definir el nuevo DataFrame con las 3 variantes
# 0 = No disponible | 1 = Parcial/En desarrollo | 2 = Cobertura Completa
datos_evolucionados <- data.frame(
  Etapa_Flujo = c(
    "1. Captura en Ruta (Offline-first)",
    "2. Identificación del Reciclador",
    "3. Evidencia Inmutable (Backend)",
    "4. Control de Inventario/Stock",
    "5. Cuenta Virtual (Dinero/Créditos)",
    "6. Reportabilidad Fiscal (Ley REP)"
  ),
  PetCloud_Original = c(2, 1, 2, 1, 0, 2),
  CicloBem_Original = c(1, 2, 2, 2, 2, 1),
  Trama = c(2, 2, 2, 2, 2, 2) # La propuesta evolutiva completa todo el ciclo
)

# 3. Transformar los datos a formato largo para ggplot2
datos_largos <- datos_evolucionados %>%
  pivot_longer(cols = c(PetCloud_Original, CicloBem_Original, Trama), 
               names_to = "Solucion", 
               values_to = "Nivel_Cobertura") %>%
  mutate(
    # Mantener el orden lógico de las etapas del flujo
    Etapa_Flujo = factor(Etapa_Flujo, levels = rev(datos_evolucionados$Etapa_Flujo)),
    # Renombrar las columnas para que se vean bien en la gráfica
    Solucion = case_when(
      Solucion == "PetCloud_Original" ~ "PetCloud (Original)",
      Solucion == "CicloBem_Original" ~ "CicloBem (Original)",
      Solucion == "Trama" ~ "Trama (Nueva Propuesta)"
    ),
    Solucion = factor(Solucion, levels = c("PetCloud (Original)", "CicloBem (Original)", "Trama (Nueva Propuesta)")),
    # Etiquetas de texto para las celdas
    Etiqueta_Texto = case_when(
      Nivel_Cobertura == 2 ~ "Core / Completo",
      Nivel_Cobertura == 1 ~ "Parcial / Adaptable",
      Nivel_Cobertura == 0 ~ "Ausente"
    )
  )
# 4. Generar la matriz gráfica comparativa (CÓDIGO CORREGIDO)
ggplot(datos_largos, aes(x = Solucion, y = Etapa_Flujo, fill = as.factor(Nivel_Cobertura))) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = Etiqueta_Texto), color = "black", fontface = "bold", size = 3.5) +
  scale_fill_manual(
    values = c("2" = "#a5d6a7", "1" = "#ffe082", "0" = "#ef9a9a"),
    guide = "none" 
  ) +
  labs(
    title = "Evolución del Flujo: Integración de la Dimensión Financiera",
    subtitle = "Comparativa del impacto al enfocar el sistema de créditos en el Reciclador de Base",
    x = "Enfoque de la Plataforma",
    y = "Capacidad del Ecosistema"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15, color = "#1a252f"),
    plot.subtitle = element_text(size = 11, color = "gray30", margin = margin(b = 20)),
    axis.text.x = element_text(face = "bold", size = 11, color = "#2c3e50"),
    axis.text.y = element_text(size = 11, face = "bold"), # <- CORREGIDO: cambiado 'fontface' por 'face'
    panel.grid = element_blank(),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15))
  )



# tabla ley rep -----------------------------------------------------------

# Instalar el paquete si no lo tienes: install.packages("gt")
library(gt)


# 1. Crear el dataframe con los datos de la tabla
datos_rep <- data.frame(
  Dimension = c("Rol en el sistema", "Formalidad", "Sustento económico"),
  Teoria = c(
    "Son reconocidos como **gestores clave** de la cadena de valor.",
    "Mecanismos de capacitación y **certificación gratuita**.",
    "**Tarifas justas** pagadas por los sistemas de gestión por su servicio."
  ),
  Practica = c(
    "Se enfrentan a asimetrías de negociación frente a los macro-sistemas corporativos.",
    "El proceso de certificación avanza lento debido a brechas digitales y de escolaridad.",
    "Al privatizarse las rutas domiciliarias disminuye el material disponible en la vía pública."
  )
)

# 2. Generar la tabla estilizada con gt
tabla_ley_rep <- datos_rep |> 
  gt() |> 
  tab_header(
    title = md("**Impacto de la Ley REP en Recicladores de Base**"),
    subtitle = "Comparativa entre el diseño institucional y la realidad operativa"
  ) |> 
  cols_label(
    Dimension = md("**Dimensión**"),
    Teoria = md("**En la Teoría (La Ley)**"),
    Practica = md("**En la Práctica (La Realidad)**")
  ) |> 
  fmt_markdown(columns = everything()) |> 
  tab_options(
    table.width = pct(100),
    heading.title.font.size = px(18),
    heading.subtitle.font.size = px(14),
    column_labels.background.color = "#f4f4f4",
    table.border.top.color = "black",
    table.border.bottom.color = "black"
  ) |> 
  cols_width(
    Dimension ~ pct(20),
    Teoria ~ pct(40),
    Practica ~ pct(40)
  )

# 3. Visualizar la tabla en el panel de Viewer de RStudio
tabla_ley_rep

# MAPEO -------------------------------------------------------------------

library(ggplot2)
library(ggrepel)

# 1. Crear el dataframe con el mapeo de actores
mapeo_actores <- data.frame(
  Actor = c(
    "Sistemas de Gestión\n(GRANSIC)", 
    "Recicladores de Base\n(ANARCH)", 
    "Municipalidades", 
    "Empresas de Camiones\n(Industriales)"
  ),
  Poder_Negociacion = c(8.5, 3.0, 6.0, 7.5), # Escala 1 al 10
  Impacto_Sufrido = c(4.0, 9.0, 5.5, 3.0),   # Grado de alteración a su modelo
  Nivel_Disputa = c("Alta", "Crítica", "Media", "Media"),
  Interes_Principal = c("Metas e Industrialización", "Sustento y Zonas Exclusivas", "Gobernanza Territorial", "Contratos de Ruta")
)

# 2. Generar el gráfico de matriz de poder e impacto
grafico_mapeo <- ggplot(mapeo_actores, aes(x = Poder_Negociacion, y = Impacto_Sufrido)) +
  geom_point(aes(size = Impacto_Sufrido, color = Nivel_Disputa), alpha = 0.7) +
  scale_size_continuous(range = c(8, 18), guide = "none") +
  scale_color_manual(values = c("Crítica" = "#d9534f", "Alta" = "#f0ad4e", "Media" = "#5bc0de")) +
  
  # Añadir etiquetas dinámicas para que no se topen
  geom_text_repel(aes(label = Actor), fontface = "bold", size = 4.5, box.padding = 0.8) +
  
  # Líneas de cuadrante promedio de referencia
  geom_vline(xintercept = 5, linetype = "dashed", color = "gray60") +
  geom_hline(yintercept = 5, linetype = "dashed", color = "gray60") +
  
  # Configuración de ejes y títulos
  labs(
    title = "Mapeo de Actores bajo la Ley REP en Chile",
    subtitle = "Asimetría entre el Impacto del Cambio y el Poder de Negociación Real",
    x = "◄ Menor Poder — PODER DE NEGOCIACIÓN EN EL SISTEMA — Mayor Poder ►",
    y = "◄ Menor Impacto — IMPACTO EN SU MODELO DE SUSTENTO — Mayor Impacto ►",
    color = "Intensidad de la Disputa"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  ) +
  
  # Límites fijos para representar la escala 1-10
  xlim(1, 10) + 
  ylim(1, 10)

# 3. Desplegar el mapa en el panel de RStudio
print(grafico_mapeo)


# TRAMA -------------------------------------------------------------------

# 1. Instalar y cargar la librería necesaria
install.packages("DiagrammeR")
library(DiagrammeR)

# 2. Generar el diagrama estructural
grViz("
digraph TramaFlujo {
  
  # Configuración global del gráfico
  graph [layout = dot, rankdir = TB, nodesep = 0.4, ranksep = 0.5]

  # Configuración de los estilos de los nodos
  node [shape = box, fontname = 'Helvetica', style = 'filled, rounded', 
        color = '#2c3e50', fillcolor = '#ecf0f1', penwidth = 2]
  
  # Estilos específicos por niveles
  TRAMA [fillcolor = '#34495e', fontcolor = 'white', shape = ellipse]
  DATOS [fillcolor = '#2980b9', fontcolor = 'white', shape = ellipse]
  DECISIONES [fillcolor = '#27ae60', fontcolor = 'white', shape = ellipse]
  
  # Agrupadores de pilares principales
  node [fillcolor = '#3498db', fontcolor = 'white']
  TERRITORIO; ACTORES; MATERIALES;
  
  # Elementos de Territorio
  node [fillcolor = '#e8f4f8', fontcolor = 'black']
  geolocalizacion [label = 'Geolocalización']; estaciones; rutas; barrios;
  
  # Elementos de Actores
  node [fillcolor = '#fcf3cf', fontcolor = 'black']
  recicladores; cooperativas; municipios_act [label = 'Municipios']; empresas_act [label = 'Empresas'];
  
  # Elementos de Materiales
  node [fillcolor = '#fadbd8', fontcolor = 'black']
  PET; vidrio; carton [label = 'Cartón']; organicos [label = 'Orgánicos'];
  
  # Fase de Datos y Decisiones
  node [fillcolor = '#eaeded', fontcolor = 'black']
  MONITOREO; ANÁLISIS;
  
  node [fillcolor = '#e8f8f5', fontcolor = 'black']
  municipio_dec [label = 'Municipio']; comunidad; empresas_dec [label = 'Empresas'];

  # --- CONEXIONES (FLUJO) ---
  
  # Trama se divide en los 3 pilares
  TRAMA -> {TERRITORIO; ACTORES; MATERIALES}
  
  # Flujo vertical de Territorio
  TERRITORIO -> geolocalizacion -> estaciones -> rutas -> barrios
  
  # Flujo vertical de Actores
  ACTORES -> recicladores -> cooperativas -> municipios_act -> empresas_act
  
  # Flujo vertical de Materiales
  MATERIALES -> PET -> vidrio -> carton -> organicos
  
  # Convergencia de los tres pilares hacia DATOS
  {barrios; empresas_act; organicos} -> DATOS
  
  # División en Monitoreo y Análisis
  DATOS -> {MONITOREO; ANÁLISIS}
  
  # Convergencia hacia DECISIONES
  {MONITOREO; ANÁLISIS} -> DECISIONES
  
  # Salidas finales de Decisiones
  DECISIONES -> {municipio_dec; comunidad; empresas_dec}
  
  # Alineación forzada para mantener las 3 columnas simétricas arriba
  {rank = same; TERRITORIO; ACTORES; MATERIALES}
  {rank = same; geolocalizacion; recicladores; PET}
  {rank = same; barrios; empresas_act; organicos}
}
")

# LAB ---------------------------------------------------------------------

# 1. Asegurar que la librería está cargada
library(DiagrammeR)

# 2. Generar el diagrama de Arquitectura de Marca
grViz("
digraph ArquitecturaMarca {
  
  # Configuración global del lienzo
  graph [layout = dot, rankdir = TB, nodesep = 0.5, ranksep = 0.4]

  # Configuración de estilos para los nodos de texto/bloques
  node [shape = box, fontname = 'Helvetica', style = 'filled, rounded', 
        color = '#2c3e50', fillcolor = '#f8f9fa', penwidth = 2, fixedsize = false]
  
  # Nodo Raíz: Marca Principal
  TRAMA [
    label = 'TRAMA\n\nInfraestructura socio-técnica\npara territorios circulares',
    fillcolor = '#2c3e50', fontcolor = 'white', fontsize = 14, style = 'filled', shape = rectangle
  ]
  
  # Nodos de Segundo Nivel: Componentes de la Arquitectura
  SAREA [
    label = 'SAREA\n\nRed territorial\nde colaboración',
    fillcolor = '#3498db', fontcolor = 'white', fontsize = 12
  ]
  
  CIRCULAB [
    label = 'CIRCULAB\n\nLaboratorio de\ninvestigación aplicada',
    fillcolor = '#9b59b6', fontcolor = 'white', fontsize = 12
  ]
  
  # Nodo de Convergencia: Operación
  PILOTOS [
    label = 'PILOTOS LOCALES',
    fillcolor = '#16a085', fontcolor = 'white', fontsize = 13, shape = ellipse
  ]
  
  # Nodos Finales: Despliegue Geográfico
  node [fillcolor = '#eaeded', fontcolor = '#2c3e50', fontsize = 11, shape = box, style = 'filled']
  Chile; Brasil; Euskadi;

  # --- CONEXIONES Y FLUJOS ---
  
  # De la marca principal a sus dos brazos estratégicos
  TRAMA -> {SAREA; CIRCULAB} [color = '#7f8c8d', penwidth = 1.5]
  
  # Convergencia de la red y el laboratorio en los pilotos
  {SAREA; CIRCULAB} -> PILOTOS [color = '#7f8c8d', penwidth = 1.5]
  
  # Despliegue hacia los territorios
  PILOTOS -> {Chile; Brasil; Euskadi} [color = '#16a085', penwidth = 1.5]
  
  # Relaciones e intercambios bidireccionales entre los pilotos locales
  Chile -> Brasil [dir = both, color = '#e67e22', style = dashed, constraint = false]
  Brasil -> Euskadi [dir = both, color = '#e67e22', style = dashed, constraint = false]
  Euskadi -> Chile [dir = both, color = '#e67e22', style = dashed, constraint = false]

  # Alineaciones forzadas para mantener la estructura simétrica
  {rank = same; SAREA; CIRCULAB}
  {rank = same; Chile; Brasil; Euskadi}
}
")

# V2 ----------------------------------------------------------------------

# 1. Asegurar que la librería está cargada
library(DiagrammeR)

# 2. Generar el diagrama de Arquitectura de Marca Actualizado
grViz("
digraph ArquitecturaMarcaActualizada {
  
  # Configuración global del lienzo
  graph [layout = dot, rankdir = TB, nodesep = 0.6, ranksep = 0.4]

  # Configuración de estilos para los nodos de texto/bloques
  node [shape = box, fontname = 'Helvetica', style = 'filled, rounded', 
        color = '#2c3e50', fillcolor = '#f8f9fa', penwidth = 2, fixedsize = false]
  
  # Nodo Raíz: Marca Principal
  TRAMA [
    label = 'TRAMA\n\nInfraestructura socio-técnica\npara territorios circulares',
    fillcolor = '#2c3e50', fontcolor = 'white', fontsize = 14, style = 'filled', shape = rectangle
  ]
  
  # Nodos de Segundo Nivel: Componentes de la Arquitectura
  SAREA [
    label = 'SAREA\n\nRed territorial\nde colaboración',
    fillcolor = '#3498db', fontcolor = 'white', fontsize = 12
  ]
  
  CIRCULAB [
    label = 'CIRCULAB\n\nLaboratorio de\ninvestigación aplicada',
    fillcolor = '#9b59b6', fontcolor = 'white', fontsize = 12
  ]
  
  # Nodo de Convergencia: Operación
  PILOTOS [
    label = 'PILOTOS LOCALES',
    fillcolor = '#16a085', fontcolor = 'white', fontsize = 13, shape = ellipse
  ]
  
  # Nodos de Despliegue Territorial
  node [fillcolor = '#eaeded', fontcolor = '#2c3e50', fontsize = 11, shape = box, style = 'filled']
  Chile [label = 'Chile', fontwidth = 'bold']; 
  Euskadi [label = 'Euskadi', fontwidth = 'bold'];
  
  # Foco local específico
  PuenteAlto [
    label = 'Puente Alto\n(Foco Local)', 
    fillcolor = '#fadbd8', color = '#e74c3c', fontcolor = '#c0392b'
  ]

  # --- CONEXIONES Y FLUJOS ---
  
  # De la marca principal a sus dos brazos estratégicos
  TRAMA -> {SAREA; CIRCULAB} [color = '#7f8c8d', penwidth = 1.5]
  
  # Convergencia de la red y el laboratorio en los pilotos
  {SAREA; CIRCULAB} -> PILOTOS [color = '#7f8c8d', penwidth = 1.5]
  
  # Despliegue hacia los territorios activos
  PILOTOS -> {Chile; Euskadi} [color = '#16a085', penwidth = 1.5]
  
  # Dependencia territorial en Chile
  Chile -> PuenteAlto [color = '#e74c3c', penwidth = 2]
  
  # Puente bidireccional de cooperación horizontal entre los territorios principales
  Chile -> Euskadi [dir = both, color = '#e67e22', style = dashed, constraint = false]

  # Alineaciones forzadas para mantener la simetría
  {rank = same; SAREA; CIRCULAB}
  {rank = same; Chile; Euskadi}
}
")


# cuadro  -----------------------------------------------------------------
# 1. Cargar las librerías necesarias
library(knitr)
library(kableExtra)

# 2. Crear la base de datos
datos_marca <- data.frame(
  Componente = c("TRAMA", "SAREA", "CIRCULAB"),
  Rol = c("Marca principal / Infraestructura digital", "Red territorial y despliegue de actores", "Laboratorio de innovación e investigación"),
  Fortalezas = c("Evoca relaciones, territorio y prácticas sistémicas", "Significa 'red' en euskera; alta pertinencia en Euskadi", "Comunica experimentación, circularidad y desarrollo"),
  Precauciones = c("Comprobar disponibilidad legal de marca y dominio web", "Requiere pedagogía o explicación fuera del contexto vasco", "Suena más a programa o laboratorio que a infraestructura"),
  Accion = c("Validar IP y comprar dominios (.cl, .eus)", "Explicar su origen en el puente con Puente Alto", "Vincularlo como el motor de datos de TRAMA")
)

# 3. Generar la tabla con el argumento corregido (background)
kbl(datos_marca, col.names = c("Componente", "Rol en la Arquitectura", "Fortalezas", "Precauciones", "Acción Inmediata")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(0, bold = TRUE, background = "#2c3e50", color = "white") %>% # <- Corrección aquí
  column_spec(1, bold = TRUE, color = "#2980b9")



# DIMENSIONES -------------------------------------------------------------

# 1. Asegurar que la librería está cargada
library(DiagrammeR)

# 2. Generar el flujo secuencial de valor de datos
grViz("
digraph CadenaValorTrama {
  
  # Configuración general del lienzo (Diseño estrictamente vertical)
  graph [layout = dot, rankdir = TB, nodesep = 0.5, ranksep = 0.5]

  # Configuración base para todos los bloques
  node [shape = box, fontname = 'Helvetica', style = 'filled, rounded', 
        color = '#2c3e50', penwidth = 2, fixedsize = false, width = 4]
        
  # Configuración de las flechas de conexión
  edge [color = '#7f8c8d', penwidth = 2, arrowhead = normal, arrowsize = 1.2]

  # --- DEFINICIÓN DE NODOS CON SU CONTENIDO ---
  
  ACTORES [
    label = 'ACTORES\n\n• Recicladores   • Cooperativas   • Municipios\n• Empresas   • Comunidades   • Investigadores',
    fillcolor = '#ebf5fb', fontcolor = '#2c3e50', fontsize = 12
  ]
  
  PRACTICAS [
    label = 'PRÁCTICAS\n\n• Recolección   • Clasificación   • Registro\n• Cooperación   • Comercialización   • Decisiones',
    fillcolor = '#e8f8f5', fontcolor = '#2c3e50', fontsize = 12
  ]
  
  INFRAESTRUCTURA [
    label = 'INFRAESTRUCTURA\n\n• Estaciones   • Rutas\n• Equipamiento   • Espacios',
    fillcolor = '#fef9e7', fontcolor = '#2c3e50', fontsize = 12
  ]
  
  DATOS [
    label = 'DATOS\n\n• Materiales   • Peso   • Ubicación   • Tiempo\n• Actores   • Contexto   • Evidencias   • Destinos',
    fillcolor = '#fcf3cf', fontcolor = '#2c3e50', fontsize = 12
  ]
  
  INTERPRETACION [
    label = 'INTERPRETACIÓN\n\n• Paneles   • Indicadores   • Informes   • Análisis\n• Coordinación   • Investigación   • Decisiones',
    fillcolor = '#f4ecf7', fontcolor = '#2c3e50', fontsize = 12
  ]

  # --- CONEXIONES SECUENCIALES ---
  
  ACTORES -> PRACTICAS -> INFRAESTRUCTURA -> DATOS -> INTERPRETACION

}
")


# matriz de comparación ---------------------------------------------------

# 1. Cargar librerías
library(knitr)
library(kableExtra)

# 2. Construir la estructura de datos comparativa
matriz_territorios <- data.frame(
  Dimension = c(
    "Organización del reciclaje",
    "Rol del reciclador",
    "Infraestructura",
    "Digitalización",
    "Trazabilidad",
    "Relación municipio–reciclador",
    "Valorización económica",
    "Datos",
    "Capital social",
    "Gobernanza"
  ),
  Chile = c(
    "Por investigar según territorio",
    "Actor económico y territorial",
    "Puntos limpios, jaulas, rutas y equipamiento",
    "Nivel y usos por identificar",
    "Registros y sistemas existentes por estudiar",
    "Variable según comuna y modelo institucional",
    "Mercados, compradores y condiciones locales",
    "Fragmentados según institución y territorio",
    "Redes territoriales y organizativas",
    "Por investigar"
  ),
  Rio_de_Janeiro = c(
    "Por investigar en cooperativas, catadores y otros actores",
    "Actor económico, ambiental y organizativo",
    "Cooperativas, puntos de recuperación y redes de recolección",
    "Experiencias digitales documentadas",
    "Casos de plataformas de registro y seguimiento",
    "Variable según territorio y organización",
    "Mercados, cooperativas y cadenas de valorización",
    "Diversos sistemas y experiencias",
    "Redes de catadores, cooperativas y comunidades",
    "Por investigar"
  ),
  Euskadi = c(
    "Por investigar según municipio y sistema de gestión",
    "Debe estudiarse según las formas locales de gestión",
    "Infraestructuras municipales, sistemas de recogida y gestión",
    "Nivel y usos por identificar",
    "Sistemas existentes por estudiar",
    "Variable según municipio y modelo de gestión",
    "Cadenas de valorización y actores locales",
    "Sistemas institucionales y organizativos por mapear",
    "Redes municipales, comunitarias y organizativas",
    "Por investigar"
  )
)

# 3. Renderizar la tabla estilizada
kbl(
  matriz_territorios,
  col.names = c("Dimensión de Análisis", "Chile", "Río de Janeiro", "Euskadi")
) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(0, bold = TRUE, background = "#2c3e50", color = "white") %>%
  column_spec(1, bold = TRUE, color = "#2980b9", width = "22%") %>%
  column_spec(2:4, width = "26%")

