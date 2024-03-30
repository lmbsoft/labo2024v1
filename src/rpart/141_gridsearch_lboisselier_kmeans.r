library(data.table)
library(cluster)
library(factoextra)  # Para visualización de clusters
library(ggplot2)

#Cambia el directorio de trabajo según donde tengas los datos
setwd("~/Documentos/2024/universidad austral/laboratorio1")
#setwd("~/buckets/b1/")

# Cambia el path al archivo según donde lo hayas guardado
archivo_resultados <- "./exp/HT2020/gridsearch.txt"

# Leer los resultados del grid search
resultados <- fread(archivo_resultados)

# Preparación de los datos (opcionalmente podrías normalizarlos)
resultados_norm <- scale(resultados[, .(cp, max_depth, min_split, min_bucket)])

# Determinar el número óptimo de clusters (Ejemplo con el método del codo)
fviz_nbclust(resultados_norm, kmeans, method = "wss") + theme_minimal()

# Una vez decidido el número óptimo de clusters, realizar k-means clustering
# Reemplaza '4' por el número de clusters que hayas elegido
set.seed(123)  # Asegura reproducibilidad
kmeans_result <- kmeans(resultados_norm, centers = 4)

# Añadir la asignación de clusters al conjunto de datos original
resultados$cluster <- kmeans_result$cluster

# Analizar los resultados por cluster
resultados_summary <- resultados[, .(
  ganancia_promedio_mean = mean(ganancia_promedio),
  cp_mean = mean(cp),
  max_depth_mean = mean(max_depth),
  min_split_mean = mean(min_split),
  min_bucket_mean = mean(min_bucket)
), by = cluster]

# Visualizar los clusters con respecto a las ganancias promedio y otro hiperparámetro
ggplot(resultados, aes(x = cp, y = ganancia_promedio, color = as.factor(cluster))) +
  geom_point() + theme_minimal() +
  labs(color = "Cluster", title = "Clusters de Configuración de Hiperparámetros vs. Ganancia Promedio",
       x = "Parámetro de Complejidad (cp)", y = "Ganancia Promedio")

# Puedes ajustar los ejes 'x' e 'y' para visualizar contra diferentes hiperparámetros

# Imprimir el resumen por cluster para análisis
print(resultados_summary)
