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


#cambio la clase ternaria por binaria, trabajo directamente sobre el dataset de 202107
dtrain[, clase_binaria := ifelse(clase_ternaria == "BAJA+2", "pos", "neg")]
dtrain[, clase_ternaria := NULL]

# Define las combinaciones de parámetros
#Boisselier Leandro	
# 1	-1	7	1000	500	48872000
#	2	-1	7	500	250	48732000
#	5	-1	8	1000	500	47972000
#	10	-1	10	1000	500	47640000
#	50	-1	9	5000	1000	45342000
#	100	-1	9	1000	20	44078000

minsplit_values <- c(500, 1000, 5000)
minbucket_values <- c(20, 250, 500, 1000)
maxdepth_values <- c(7, 8, 9, 10)
cp_values <- c(-1)

# Itera sobre las combinaciones de parámetros
for (minsplit in minsplit_values) {
  for (minbucket in minbucket_values) {
    if (minsplit >= 2 * minbucket) { # Asegura la relación deseada
      for (maxdepth in maxdepth_values) {
        for (cpvalue in cp_values) {  
        
          # Entrena el modelo con los parámetros actuales
          modelo <- rpart(
            formula = "clase_binaria ~ .",
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
          dapply[, prob_baja2 := prediccion[, "pos"]]
          
          # Solo envía estímulo a los registros con probabilidad de BAJA+2 mayor a 1/40
          dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]
          
          # Genera el archivo para Kaggle con el nombre ajustado a los parámetros
          file_name <- paste0("./exp/HT2020/K2020_142-cp_", cpvalue, "-md_", maxdepth, "-ms_", minsplit, "-mb_", minbucket, ".csv")
          fwrite(dapply[, .(numero_de_cliente, Predicted)], file = file_name, sep = ",")
          
          # Puedes incluir aquí código para evaluar el modelo y decidir cuál configuración es la mejor
        }
      }
    }
  }
}

# Asegúrate de que los directorios "./exp/" y "./exp/KA2001/" existen antes de correr este script
# o incluye la creación de estos directorios dentro del script.



