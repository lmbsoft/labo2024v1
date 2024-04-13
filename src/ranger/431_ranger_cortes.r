# Ranger  una libreria que implementa el algoritmo Random Forest

# limpio la memoria
rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("ranger")
require("randomForest") # solo se usa para imputar nulos

# notar como la suma de muchos arboles contrarresta el efecto de min.node.size=1
# "mtry" = 30, cantidad de variables que evalua para hacer un split
#  generalmente sqrt(ncol(dtrain))
param <- list(
  "num.trees" = 300, # cantidad de arboles
  "mtry" = 13,
  "min.node.size" = 50, # tamaño minimo de las hojas
  "max.depth" = 10 # 0 significa profundidad infinita
)
#probar con parámetros mejores
#fecha	num.trees	max.depth	min.node.size	mtry	xval_folds	ganancia	iteracion
#20240411 062137	499	19	270	9	5	56619000	39
param <- list(
  "num.trees" = 499, # cantidad de arboles
  "mtry" = 9,
  "min.node.size" = 270, # tamaño minimo de las hojas
  "max.depth" = 19 # 0 significa profundidad infinita
)
#escribir un mensaje para agregar en un commit en kaggle sobre los parámetros usados
#num.trees = 499, mtry = 9, min.node.size = 270, max.depth = 19


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

setwd("~/buckets/b1/") # Establezco el Working Directory

# cargo MI semilla, que esta en MI bucket
tabla_semillas <- fread( "./datasets//mis_semillas.txt" )
ksemilla_azar <- tabla_semillas[ 1, semilla ]  # 1 es mi primer semilla


# cargo los datos donde entreno
dataset <- fread("./datasets/dataset_pequeno.csv")


dtrain <- dataset[foto_mes == 202107]
dapply <- dataset[foto_mes == 202109]


# genero el modelo de Random Forest con la libreria ranger
set.seed(ksemilla_azar) # Establezco la semilla aleatoria

factorizado <- as.factor(dtrain$clase_ternaria)
dtrain[, clase_ternaria := factorizado]

# imputo los nulos, ya que ranger no acepta nulos
# Leo Breiman, ¿por que le temias a los nulos?
dtrain <- na.roughfix(dtrain)

setorder(dtrain, clase_ternaria) # primero quedan los BAJA+1, BAJA+2, CONTINUA

# genero el modelo de Random Forest llamando a ranger()
modelo <- ranger(
  formula = "clase_ternaria ~ .",
  data = dtrain,
  probability = TRUE, # para que devuelva las probabilidades
  num.trees = param$num.trees,
  mtry = param$mtry,
  min.node.size = param$min.node.size,
  max.depth = param$max.depth
)


# Carpinteria necesaria sobre  dapply
# como quiere la Estadistica Clasica, imputar nulos por separado
# ( aunque en este caso ya tengo los datos del futuro de anteman
#  pero bueno, sigamos el librito de estos fundamentalistas a rajatabla ...
dapply[, clase_ternaria := NULL]
dapply <- na.roughfix(dapply)


# aplico el modelo recien creado a los datos del futuro
prediccion <- predict(modelo, dapply)

# Genero la entrega para Kaggle
entrega <- as.data.table(list(
  "numero_de_cliente" = dapply[, numero_de_cliente],
  "prob" = prediccion$predictions[, "BAJA+2"],
  "Predicted" = as.numeric(prediccion$predictions[, "BAJA+2"] > 1 / 40)
)) # genero la salida

# creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/KA4310/", showWarnings = FALSE)

# ordeno por probabilidad descendente
setorder(entrega, -prob)
#quito la columna prob para que no de error al subir
entrega[, prob := NULL]


# genero archivos con los  "envios" mejores
# deben subirse "inteligentemente" a Kaggle para no malgastar submits
# si la palabra inteligentemente no le significa nada aun
# suba TODOS los archivos a Kaggle
# espera a la siguiente clase sincronica en donde el tema sera explicado

#mi estrategia para los cortes será la siguiente:
#voy a generar hasta el corte que me dió en la entrega anterior: 26000
# luego bajo haciendo una búsqueda binaria entregando y comparando el puntaje de cada entrega

cortes <- seq(4000, 26000, by = 500)
for (envios in cortes) {
  entrega[, Predicted := 0L]
  entrega[1:envios, Predicted := 1L]

  # Construyo el nombre del archivo con los parámetros
  nombre_archivo <- paste0(
    "KA4310_", # prefijo del nombre
    "nt", param$num.trees, "_",  # número de árboles
    "mtry", param$mtry, "_",  # mtry
    "ns", param$min.node.size, "_",  # min.node.size
    "md", param$max.depth, # max.depth
    "_", envios,  # cantidad de envios
    ".csv" 
  )

  archivo_salida <- paste0("./exp/KA4310/", nombre_archivo)

  # genero el archivo para Kaggle
  fwrite(entrega,
    file = archivo_salida,
    sep = ","
  )
}