# Cargar la librería necesaria
library(data.table)
#setear el workingdir al directorio donde estoy parado
# Setear el workingdir al directorio donde estoy ejecutando el script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Leer el archivo BO_log.txt
dt <- fread("BO_log.txt")

# Funciones previas definidas

tree_description <- function(max_depth, num_leaves) {
  if (max_depth >= 10 & num_leaves >= 1000) {
    return("Grande y Complejo")
  } else if (max_depth <= 10 & num_leaves <= 100) {
    return("Pequeño y Simple")
  } else {
    return("Verificar")
  }
}

generalization_risk <- function(lambda_l1, lambda_l2, learning_rate) {
  regularization <- ifelse(lambda_l1 > 0.1 | lambda_l2 > 0.1, "Buena Regularización", "Poca Regularización")
  stability <- ifelse(learning_rate < 0.05, "Estable", "Inestable")
  return(paste(regularization, stability, sep=", "))
}

model_complexity <- function(learning_rate, feature_fraction, num_leaves, min_data_in_leaf) {
  lr_complexity <- ifelse(learning_rate < 0.05, "Baja", ifelse(learning_rate > 0.2, "Alta", "Moderada"))
  ff_complexity <- ifelse(feature_fraction <= 0.4, "Baja", ifelse(feature_fraction >= 0.8, "Alta", "Moderada"))
  nl_complexity <- ifelse(num_leaves > 1000, "Alta", ifelse(num_leaves < 200, "Baja", "Moderada"))
  mdil_complexity <- ifelse(min_data_in_leaf > 50, "Baja", ifelse(min_data_in_leaf < 20, "Alta", "Moderada"))
  return(paste(lr_complexity, ff_complexity, nl_complexity, mdil_complexity, sep=", "))
}

# Nueva función para calcular el riesgo de sobreajuste
overfitting_chance <- function(num_leaves, min_data_in_leaf, max_depth, feature_fraction, lambda_l1, lambda_l2) {
  score <- 0
  score <- score + ifelse(num_leaves > 1000, 20, ifelse(num_leaves < 200, -10, 5))
  score <- score + ifelse(min_data_in_leaf < 20, 20, ifelse(min_data_in_leaf > 50, -10, 5))
  score <- score + ifelse(max_depth > 15, 20, ifelse(max_depth < 5, -10, 5))
  score <- score + ifelse(feature_fraction > 0.8, 20, ifelse(feature_fraction < 0.4, -10, 5))
  score <- score + ifelse(lambda_l1 < 0.01 & lambda_l2 < 0.01, 20, -10)
  
  # Convertir score a una probabilidad porcentual de sobreajuste
  probability <- max(0, min(100, 50 + score))  # Limitar entre 0 y 100
  return(probability)
}

# Aplicar las funciones a las columnas correspondientes
dt[, Tree_Description := tree_description(max_depth, num_leaves)]
dt[, Generalization_Risk := generalization_risk(lambda_l1, lambda_l2, learning_rate)]
dt[, Model_Complexity := model_complexity(learning_rate, feature_fraction, num_leaves, min_data_in_leaf)]
dt[, Overfitting_Chance := overfitting_chance(num_leaves, min_data_in_leaf, max_depth, feature_fraction, lambda_l1, lambda_l2)]

# Guardar los resultados en un nuevo archivo CSV
fwrite(dt, "BO_log_analisis.csv")
