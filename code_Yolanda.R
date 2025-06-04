#Code Yolanda

library(readxl)
library(tidyverse)
library(janitor)
fauna <- read_excel("MEGAFAUNA/FAUNA_MUESTREOS_AVANZADA.xlsx", 
                                       sheet = "DATOS")
glimpse(fauna)

fauna2 <- clean_names(fauna)
glimpse(fauna2)

library(tidyverse)

# Limpieza y transformación
df_limpio <- fauna2 %>%
  filter(!is.na(familia), !is.na(n_individuos), !is.na(punto)) %>%
  filter(punto %in% c("E234", "E232 R2", "E249", "E232")) %>% 
  mutate(n_individuos = as.numeric(n_individuos)) %>%
  group_by(punto, familia) %>%
  summarise(abundancia = sum(n_individuos))


comunidad <- df_limpio %>%
  pivot_wider(names_from = familia, values_from = abundancia, values_fill = 0) %>%
  column_to_rownames("punto")

library(vegan)

# Shannon, Simpson y Riqueza
diversidad <- data.frame(
  punto = rownames(comunidad),
  riqueza = specnumber(comunidad),
  shannon = diversity(comunidad, index = "shannon"),
  simpson = diversity(comunidad, index = "simpson"),
  equitatividad = diversity(comunidad)/log(specnumber(comunidad))
)

ggplot(diversidad, aes(x = punto, y = )) +
  geom_col(fill = "steelblue") +
  labs(title = "Índice de Shannon por punto", x = "Punto", y = "Shannon") +
  theme_minimal() +
  coord_flip()




## Cluster

# Resumen de variables ambientales por punto
ambiente <- fauna2 %>%
  dplyr::select(punto, tipo_sedimento, depth_m) %>%
  filter(!is.na(punto)) %>%
  group_by(punto) %>%
  summarise(
    tipo_sedimento = first(tipo_sedimento),
    profundidad = as.numeric(first(depth_m))
  ) %>%
  column_to_rownames("punto")


# Transformar variable categórica en variables dummy si es necesario
ambiente_dummy <- ambiente %>%
  mutate(tipo_sedimento = factor(tipo_sedimento)) %>%
  model.matrix(~ tipo_sedimento - 1, data = .) %>%
  as.data.frame() %>%
  bind_cols(profundidad = ambiente$profundidad)



nmds <- metaMDS(comunidad, distance = "bray", k = 2, trymax = 100)

# Graficar NMDS
nmds_df <- as.data.frame(nmds$points) %>%
  rownames_to_column("punto")

ggplot(nmds_df, aes(MDS1, MDS2, label = punto)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_text(vjust = -0.5, size = 3) +
  labs(title = "NMDS de composición de familias") +
  theme_minimal()


#familia dom por punto
dominancia <- df_limpio %>%
  group_by(punto) %>%
  mutate(prop = abundancia / sum(abundancia)) %>%
  filter(prop == max(prop)) %>%
  select(punto, familia, prop)

top_familias <- df_limpio %>%
  group_by(familia) %>%
  summarise(total = sum(abundancia)) %>%
  top_n(10, total)

df_top <- df_limpio %>%
  filter(familia %in% top_familias$familia)

ggplot(df_limpio, aes(x = reorder(familia, -abundancia), y = abundancia, fill = punto)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Familias más abundantes por punto", x = "Familia", y = "Abundancia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


library(vegan)

nmds <- metaMDS(comunidad, distance = "bray", k = 2, trymax = 100)

plot(nmds, type = "t", main = "nMDS Bray-Curtis - Familias")



# Dendrograma con método UPGMA
distancia <- vegdist(comunidad, method = "bray")
cluster <- hclust(distancia, method = "average")

plot(cluster, labels = rownames(comunidad), main = "Clúster UPGMA por composición")

# Transformar variable categórica en variables dummy si es necesario
ambiente_dummy <- ambiente %>%
  filter(!is.na(tipo_sedimento) & !is.na(profundidad)) %>%
  mutate(tipo_sedimento = factor(tipo_sedimento),
         profundidad = as.numeric(profundidad)) %>%
  {
    dummy <- model.matrix(~ tipo_sedimento - 1, data = .)
    cbind(as.data.frame(dummy), profundidad = .$profundidad)
  }
# Buscar puntos comunes
sitios_comunes <- intersect(rownames(comunidad), rownames(ambiente_dummy))

# Filtrar y alinear
comunidad_alineada <- comunidad[sitios_comunes, , drop = FALSE]
ambiente_alineado <- ambiente_dummy[sitios_comunes, , drop = FALSE]

# Verifica dimensiones
dim(comunidad_alineada)
dim(ambiente_alineado)


bioenv_result <- bioenv(comunidad, ambiente_dummy)
bioenv_result  # variables ambientales mejor correlacionadas

# 4. Indicadores ambientales
# 4.1. Identificar grupos indicadores

# Primero, necesitas asignar cada familia (o especie, si la tienes) a un grupo trófico o categoría de sensibilidad. Esto lo puedes hacer creando una tabla de referencia, por ejemplo:
  
  indicadores <- tibble::tibble(
    familia = c("SPIONIDAE", "CAPITELLIDAE", "GLYCERIDAE", "HESIONIDAE"),
    grupo_indicador = c("Oportunista", "Oportunista", "Sensibles", "Intermedios"),
    sensibilidad = c(5, 5, 1, 3)  # estilo AMBI: 1=sensible, 5=oportunista
  )
# 4.2. Unir con la base de datos
# 
# Asumiendo que tu df tiene la columna familia y n_individuos:
#   
  df <- df %>%
  filter(!is.na(familia), !is.na(n_individuos)) %>%
  left_join(indicadores, by = "familia") %>%
  mutate(n_individuos = as.numeric(n_individuos))
# 4.3. Calcular proporciones por grupo trófico por punto
# 
# Esto te da una idea de la dominancia de grupos indicadores por sitio de muestreo:
  
  indicadores_por_punto <- df %>%
  group_by(punto, grupo_indicador) %>%
  summarise(abundancia = sum(n_individuos, na.rm = TRUE), .groups = "drop") %>%
  group_by(punto) %>%
  mutate(proporcion = abundancia / sum(abundancia))
# 4.4. Calcular un índice tipo AMBI (promedio ponderado de sensibilidad)

ambi_por_punto <- df %>%
  group_by(punto) %>%
  summarise(AMBI = weighted.mean(sensibilidad, n_individuos, na.rm = TRUE))



# Proporciones de grupos indicadores
ggplot(indicadores_por_punto, aes(x = punto, y = proporcion, fill = grupo_indicador)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Composición de grupos indicadores por punto",
       y = "Proporción", x = "Punto")

# AMBI por punto
ggplot(ambi_por_punto, aes(x = punto, y = AMBI)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Índice AMBI por punto", y = "AMBI", x = "Punto")
# ¿Y si no tengo todos los grupos definidos?
#   Puedes:
#   
#   Usar solo los grupos que conoces.
# Agregar una categoría como "Desconocido" para los que no están clasificados.
# ¿Quieres que te ayude a construir la tabla de indicadores en base a los datos que tienes (puedo ver qué familias aparecen en tu dataset)?



# Test estadisticos


diversidad <- fauna2 %>%
  filter(!is.na(punto), !is.na(familia)) %>%
  group_by(punto, tipo_sedimento) %>%
  summarise(
    riqueza = n_distinct(familia),
    abundancia = sum(as.numeric(n_individuos), na.rm = TRUE),
    .groups = "drop"
  )


# Comparación de riqueza entre hábitats
anova_riqueza <- aov(riqueza ~ tipo_sedimento, data = diversidad)
summary(anova_riqueza)

# Comparación de abundancia entre hábitats
anova_abundancia <- aov(abundancia ~ tipo_sedimento, data = diversidad)
summary(anova_abundancia)



library(ggplot2)

ggplot(diversidad, aes(x = tipo_sedimento, y = riqueza)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Riqueza por tipo de sedimento", 
       x = "Tipo de sedimento", 
       y = "Riqueza de familias")

ggplot(diversidad, aes(x = tipo_sedimento, y = abundancia)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Abundancia por tipo de sedimento", 
       x = "Tipo de sedimento", 
       y = "N° de individuos")


# permanova

comunidad <- fauna2 %>%
  filter(!is.na(punto), !is.na(familia)) %>%
  group_by(punto, familia) %>%
  summarise(abundancia = sum(as.numeric(n_individuos), na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = familia, values_from = abundancia, values_fill = 0) %>%
  tibble::column_to_rownames("punto")


metadata <- fauna2 %>%
  filter(!is.na(punto), !is.na(tipo_sedimento)) %>%
  group_by(punto) %>%
  summarise(tipo_sedimento = first(tipo_sedimento)) %>%
  column_to_rownames("punto")
nrow(comunidad)
nrow(metadata)


library(vegan)

permanova <- adonis2(comunidad ~ tipo_sedimento, data = metadata, method = "bray")
print(permanova)
