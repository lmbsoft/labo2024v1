# Cargar las librerías necesarias
library(data.table)

setwd("~/Documentos/2024/universidad austral/laboratorio1") # Establezco el Working Directory

# Leer el dataset
dataset <- fread("./datasets/dataset_pequeno.csv")

# Calcular y mostrar el número total de registros
total_records <- nrow(dataset)
cat("Número total de registros:", total_records, "\n")

# Calcular y mostrar el número total de características (excluyendo la variable objetivo)
total_features <- ncol(dataset) - 1 # Asumiendo que la última columna es la variable objetivo
cat("Número total de características:", total_features, "\n")

# Ver la distribución de la variable objetivo 'clase_ternaria'
target_distribution <- table(dataset$clase_ternaria)
cat("Distribución de la variable objetivo 'clase_ternaria':", "\n")
print(target_distribution)

# También puede ser útil ver un resumen de algunas variables para entender mejor su distribución
summary(dataset)
