
##Script 0 Analisis de redes


# Propuesta de mapeo v0 ---------------------------------------------------


#Creación de propuestas de mapeos 

#Librerías
library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)

# Definir los Nodos (Los actores del sistema)
nodos <- tibble(
  id = 1:11,
  name = c("Recicladores de Base", "Puntos Limpios", "Municipios", 
           "Empresas Generadoras", "Organizaciones Comunitarias", 
           "Gestores", "Materiales", "Rutas", "Condiciones Laborales", 
           "Incentivos", "Trazabilidad"),
  tipo = c("Social", "Infraestructura", "Institucional", 
           "Económico", "Social", "Económico", "Material", 
           "Flujo", "Atributo", "Incentivo", "Información")
)

#  Definir los Edges (Las conexiones del rizoma)
# Aquí conectas el ID de origen (from) con el ID de destino (to)
enlaces <- tibble(
  from = c(1, 1, 2, 3, 4, 5, 1, 6, 7, 8, 9),
  to   = c(2, 8, 3, 1, 6, 1, 9, 7, 11, 11, 10)
)

#  Crear el objeto de red rizomática
red_rizoma <- tbl_graph(nodes = nodos, edges = enlaces, directed = FALSE)

#  Graficar con corte rizomático (Layout de fuerza/orgánico)
ggraph(red_rizoma, layout = "nicely") + # 'nicely' o 'fr' dan ese look orgánico/rizomático
  geom_edge_link(aes(alpha = 0.6), show.legend = FALSE) +
  geom_node_point(aes(color = tipo), size = 8) +
  geom_node_text(aes(label = name), repel = TRUE, vjust = 1.5, fontface = "bold") +
  theme_graph() +
  labs(title = "Mapa Socio-Técnico del Ecosistema de Reciclaje",
       subtitle = "Mapeo de actores, flujos e impactos territoriales")

# 6. ¡GUARDAR EL GRÁFICO! (La línea que faltaba)
# Se guarda automáticamente en tu carpeta de salidas
# 6. Guardar el gráfico en tu carpeta de trabajo
ggsave(
  filename = "rizoma_reciclajev0.png", 
  plot = mi_grafico, 
  width = 12, 
  height = 8, 
  dpi = 300
)



# Propuesta de mapeo Puente Alto v0.1 -------------------------------------

# 1. Cargar librerías
library(tidyverse)
library(tidygraph)
library(ggraph)

# 2. Definir Nodos conceptuales adaptados a Puente Alto
nodos_pa <- tibble(
  id = 1:12,
  name = c("Recicladores Base Bajos de Mena", "Punto Limpio Plaza Puente Alto", 
           "Municipalidad de Puente Alto", "DIMAO Puente Alto", "CMPC (Empresa)", 
           "Juntas de Vecinos", "Ferias Libres", "Material PET/Cartón", 
           "Rutas de Recolección", "Condiciones Laborales", 
           "Subsidio Municipal", "Trazabilidad Ambiental"),
  tipo = c("Social", "Infraestructura", "Institucional", "Institucional",
           "Económico", "Social", "Social", "Material", 
           "Flujo", "Atributo", "Incentivo", "Información")
)

# 3. Enlaces (Conexiones rizomáticas del ecosistema)
enlaces_pa <- tibble(
  from = c(1, 1, 1, 2, 4, 3, 6, 7, 1, 5, 8, 9),
  to   = c(2, 6, 10, 4, 3, 11, 2, 1, 9, 8, 12, 12)
)

# 4. Crear el objeto de red
red_pa <- tbl_graph(nodes = nodos_pa, edges = enlaces_pa, directed = FALSE)

# 5. Graficar el rizoma territorial
grafico_estatico <- ggraph(red_pa, layout = "fr") + # Fruchterman-Reingold para look orgánico
  geom_edge_link(alpha = 0.4, color = "gray40") +
  geom_node_point(aes(color = tipo), size = 6) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3, fontface = "bold") +
  theme_graph() +
  labs(title = "Mapeo Socio-Técnico de Reciclaje: Puente Alto",
       subtitle = "Modelo conceptual de gobernanza y flujos territoriales",
       color = "Dimensión del Sistema")

# 6. ¡GUARDAR EL GRÁFICO! (La línea que faltaba)
# Se guarda automáticamente en tu carpeta de salidas
ggsave(filename = "visualization/rizoma_puente_alto.png", 
       plot = grafico_estatico, 
       width = 10, height = 8, dpi = 300, bg = "white")



# Propuesta de mapeo puente alto v0.2 -------------------------------------
# Calculando métricas sociológicas dentro de la red
red_analizada <- red_pa %>%
  activate(nodes) %>%
  mutate(
    # 1. Centralidad de Grado: ¿Quién tiene más conexiones directas?
    conexiones_directas = centrality_degree(),
    
    # 2. Intermediación (Betweenness): ¿Quién es el puente/articulador crítico?
    es_puente_social = centrality_betweenness(),
    
    # 3. Comunidades (Clustering): ¿Cómo se agrupan orgánicamente los actores?
    comunidad_subterritorial = as.factor(group_infomap())
  )

# Ver los resultados sociológicos en una tabla limpia
resultados_sociales <- red_analizada %>% 
  as_tibble() %>% 
  arrange(desc(es_puente_social))

print(resultados_sociales)


#Si "Recicladores Base Bajos de Mena" tiene el mayor puntaje de es_puente_social, 
#el modelo teórico demuestra que, si ellos entran en paro o no asisten, 
#el sistema completo se quiebra porque la Municipalidad no tiene llegada directa 
#al material de los hogares. 
#El poder real del rizoma reside en la base.
#Si la "DIMAO Puente Alto" y "CMPC" caen en la misma comunidad_subterritorial pero
#los recicladores quedan en otra, hay una fractura de diseño socio-técnico: 
#la industria y el decreto municipal van por un lado, 
#y la realidad comunitaria por otro.




