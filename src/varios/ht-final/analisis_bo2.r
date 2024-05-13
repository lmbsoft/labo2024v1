# Instalar y cargar la librería data.table si no está ya instalada
if (!require(data.table)) {
  install.packages("data.table")
}
library(data.table)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Leer el archivo BO_log.txt
datos <- fread("BO_log.txt")

# Inicializa las nuevas columnas
datos[, tendencia_overfitting := "TBD"]
datos[, descripcion_estructura := "TBD"]

# Definir la clasificación y descripción basada en los parámetros
for (i in 1:nrow(datos)) {
  # Analizar tendencia al overfitting
  if (datos$num_iterations[i] > 500 || datos$learning_rate[i] > 0.4 || datos$num_leaves[i] > 1000) {
    datos$tendencia_overfitting[i] <- "tendencia alta"
  } else if (datos$min_data_in_leaf[i] > 1000 || datos$feature_fraction[i] < 0.6) {
    datos$tendencia_overfitting[i] <- "tendencia baja"
  } else {
    datos$tendencia_overfitting[i] <- "tendencia moderada"
  }
  
  # Descripción de las características estructurales
  if (datos$num_iterations[i] > 500) {
    estructura <- "muchos árboles"
  } else if (datos$num_iterations[i] < 100) {
    estructura <- "pocos árboles"
  } else {
    estructura <- "cantidad moderada de árboles"
  }
  
  if (datos$num_leaves[i] > 1000) {
    estructura <- paste(estructura, ", alta complejidad de árboles")
  } else if (datos$num_leaves[i] < 100) {
    estructura <- paste(estructura, ", baja complejidad de árboles")
  } else {
    estructura <- paste(estructura, ", complejidad moderada de árboles")
  }
  
  if (datos$min_data_in_leaf[i] > 1500) {
    estructura <- paste(estructura, ", hojas con muchos datos")
  } else if (datos$min_data_in_leaf[i] < 500) {
    estructura <- paste(estructura, ", hojas con pocos datos")
  } else {
    estructura <- paste(estructura, ", cantidad moderada de datos en hojas")
  }
  
  datos$descripcion_estructura[i] <- estructura
}

# Guardar el resultado en analisis_bo.csv
fwrite(datos, file = "analisis_bo.csv", sep = ",")
