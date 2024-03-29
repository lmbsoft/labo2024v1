# Arbol elemental con libreria rpart
# Debe tener instaladas las librerias data.table, rpart y rpart.plot

# Carga las librerías necesarias
require("data.table")
require("rpart")
require("rpart.plot")

# Aquí se debe poner la carpeta de la materia de SU computadora local
setwd("~/Documentos/2024/universidad austral/laboratorio1") # Establezco el Working Directory

# Carga el dataset
dataset <- fread("./datasets/dataset_pequeno.csv")

# Define dónde voy a entrenar
dtrain <- dataset[foto_mes == 202107]
# Define dónde voy a aplicar el modelo
dapply <- dataset[foto_mes == 202109]

# Define las combinaciones de parámetros
minsplit_values <- c(15, 25, 30)
minbucket_values <- c(8, 13, 15)
maxdepth_values <- 5:10

# Itera sobre las combinaciones de parámetros
for (minsplit in minsplit_values) {
  for (minbucket in minbucket_values) {
    if (minsplit >= 2 * minbucket) { # Asegura la relación deseada
      for (maxdepth in maxdepth_values) {
        
        # Entrena el modelo con los parámetros actuales
        modelo <- rpart(
          formula = "clase_ternaria ~ .",
          data = dtrain,
          xval = 0,
          cp = -0.3, # Mantén cp en negativo como mencionaste
          minsplit = minsplit,
          minbucket = minbucket,
          maxdepth = maxdepth
        )
        
        # Aplica el modelo a los datos nuevos
        prediccion <- predict(
          object = modelo,
          newdata = dapply,
          type = "prob"
        )
        
        # Agrega a dapply una columna nueva que es la probabilidad de BAJA+2
        dapply[, prob_baja2 := prediccion[, "BAJA+2"]]
        
        # Solo envía estímulo a los registros con probabilidad de BAJA+2 mayor a 1/40
        dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]
        
        # Genera el archivo para Kaggle con el nombre ajustado a los parámetros
        file_name <- paste0("./exp/KA2001/K101_001-mp_", minsplit, "-mb_", minbucket, "-md_", maxdepth, ".csv")
        fwrite(dapply[, .(numero_de_cliente, Predicted)], file = file_name, sep = ",")
        
        # Puedes incluir aquí código para evaluar el modelo y decidir cuál configuración es la mejor
      }
    }
  }
}

# Asegúrate de que los directorios "./exp/" y "./exp/KA2001/" existen antes de correr este script
# o incluye la creación de estos directorios dentro del script.

