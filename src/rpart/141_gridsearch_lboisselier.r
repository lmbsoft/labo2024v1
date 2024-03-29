# esqueleto de grid search ajustado

rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("parallel")

PARAM <- list()
PARAM$semillas <- c(200003, 200009, 200017, 200023, 200029)

particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)

  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))

  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
    by = agrupa
  ]
}

ArbolEstimarGanancia <- function(semilla, param_basicos) {
  particionar(dataset, division = c(7, 3), agrupa = "clase_ternaria", seed = semilla)

  modelo <- rpart("clase_ternaria ~ .",
    data = dataset[fold == 1], # fold==1 es training
    xval = 0,
    control = param_basicos
  )

  prediccion <- predict(modelo,
    dataset[fold == 2], # fold==2 es testing
    type = "prob"
  )

  ganancia_test <- dataset[
    fold == 2,
    sum(ifelse(prediccion[, "BAJA+2"] > 0.025,
      ifelse(clase_ternaria == "BAJA+2", 117000, -3000),
      0
    ))
  ]

  ganancia_test_normalizada <- ganancia_test / 0.3

  return(ganancia_test_normalizada)
}

ArbolesMontecarlo <- function(semillas, param_basicos) {
  ganancias <- mcmapply(ArbolEstimarGanancia,
    semillas,
    MoreArgs = list(param_basicos),
    SIMPLIFY = FALSE,
    mc.cores = 5
  )

  ganancia_promedio <- mean(unlist(ganancias))

  return(ganancia_promedio)
}

setwd("~/Documentos/2024/universidad austral/laboratorio1")
#setwd("~/buckets/b1/")
dataset <- fread("./datasets/dataset_pequeno.csv")
dataset <- dataset[clase_ternaria != ""]

dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/HT2020/", showWarnings = FALSE)
archivo_salida <- "./exp/HT2020/gridsearch.txt"

tb_grid_search <- data.table(
  cp = numeric(),
  max_depth = integer(),
  min_split = integer(),
  min_bucket = integer(),
  ganancia_promedio = numeric()
)

for (vcp in seq(-1, -0.01, by = 0.1)) {
  for (vmin_split in c(250, 500, 750, 1000)) {
    for (vmin_bucket in c(5, 10, 20, 40)) {
      if (vmin_split >= 2 * vmin_bucket) { 
        for (vmax_depth in c(3, 5, 7, 9)) {
          param_basicos <- list(
            cp = vcp,
            minsplit = vmin_split,
            minbucket = vmin_bucket,
            maxdepth = vmax_depth
          )

          ganancia_promedio <- ArbolesMontecarlo(PARAM$semillas, param_basicos)

          tb_grid_search <- rbindlist(
            list(tb_grid_search, 
                 list(cp = vcp, max_depth = vmax_depth, min_split = vmin_split, min_bucket = vmin_bucket, ganancia_promedio = ganancia_promedio))
          )
        }
      }
    }
  }

  Sys.sleep(2)
  fwrite(tb_grid_search,
         file = archivo_salida,
         sep = "\t")
}
