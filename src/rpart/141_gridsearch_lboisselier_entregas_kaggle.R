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
#Boisselier Leandro	
# 1	-1	9	1000	500	49452000
#	2	-1	10	1000	500	49452000
#	5	-0.5	9	1000	500	49452000
#	10	-0.75	9	1000	100	47328000
#	50	-0.75	10	1000	100	46848000
#	100	-0.5	9	100	10	46084000

minsplit_values <- c(100, 1000)
minbucket_values <- c(10, 100, 500)
maxdepth_values <- c(9, 10)
cp_values <- c(-1, -0.5, -0.75)

# Itera sobre las combinaciones de parámetros
for (minsplit in minsplit_values) {
  for (minbucket in minbucket_values) {
    if (minsplit >= 2 * minbucket) { # Asegura la relación deseada
      for (maxdepth in maxdepth_values) {
        for (cpvalue in cp_values) {  
        
          # Entrena el modelo con los parámetros actuales
          modelo <- rpart(
            formula = "clase_ternaria ~ .",
            data = dtrain,
            xval = 0,
            cp = cpvalue, # Mantén cp en negativo como mencionaste
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
          file_name <- paste0("./exp/HT2020/K2020_001-cp_", cpvalue, "-md_", maxdepth, "-ms_", minsplit, "-mb_", minbucket, ".csv")
          fwrite(dapply[, .(numero_de_cliente, Predicted)], file = file_name, sep = ",")
          
          # Puedes incluir aquí código para evaluar el modelo y decidir cuál configuración es la mejor
        }
      }
    }
  }
}

# Asegúrate de que los directorios "./exp/" y "./exp/KA2001/" existen antes de correr este script
# o incluye la creación de estos directorios dentro del script.


