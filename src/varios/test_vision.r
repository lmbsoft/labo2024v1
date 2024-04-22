library(data.table)
library(ggplot2)
library(jsonlite)

#ejemplo de camino muy feliz para ver si funciona consultar a gpt vision sobre lo que se ve en una imagen generada con ggplot

#setear el workingdir en el directorio donde estoy parado
setwd("~/Documentos/2024/universidad austral/laboratorio de implementación 1/labo2024v1/src/varios")

# Generar datasets y guardar imágenes
x_values <- seq(-5, 5, by = 0.1)

# Dataset para x^2
data_quadratic <- data.table(x = x_values, y = x_values^2)
g <- ggplot(data_quadratic, aes(x, y)) + geom_line() + ggtitle("x^2")
ggsave("cuadratica_01.png", plot = g)

# Dataset para x^3
data_cubic <- data.table(x = x_values, y = x_values^3)
g <- ggplot(data_cubic, aes(x, y)) + geom_line() + ggtitle("x^3")
ggsave("cubica_01.png", plot = g)

# Bucle para enviar imágenes
contador <- 1
print(URLencode("devolver en una palabra la forma de la función: cuadrática o cúbica"))
while (contador <= 100) {
    Sys.sleep(5)  # Espera de 5 segundos entre intentos
    
    # Decidir qué imagen enviar
    prompt_text <- URLencode("devolver en una palabra la forma de la función: cuadrática o cúbica")
    if (runif(1) < 0.75) {
        imagen_a_enviar <- "cuadratica_01.png"
        print(paste("Intento", contador, ": Enviando cuadrática"))
    } else {
        imagen_a_enviar <- "cubica_01.png"
        print(paste("Intento", contador, ": Enviando cúbica"))
    }
    
    # Comando curl para enviar la imagen
    str_curl <- sprintf(
        "curl -X 'POST' \\
        'https://proyectoia.lmbsoft.com.ar/analizar_vision?prompt=%s' \\
        -H 'accept: application/json' \\
        -H 'Content-Type: multipart/form-data' \\
        -F 'imagen=@%s;type=image/png'",
        prompt_text, imagen_a_enviar
    )
    
    print(str_curl)

    # Ejecutar el comando y capturar la respuesta
    output <- system(str_curl, intern = TRUE)

    # Intentar parsear la respuesta JSON
    tryCatch({
    respuesta_json <- fromJSON(paste(output, collapse=""))
    respuesta <- respuesta_json$respuesta
    print(respuesta)

    # Condición de corte si la respuesta es 'cubica'
    if (tolower(respuesta) == "cúbica" || tolower(respuesta) == "cubica"){
        print("Respuesta cúbica recibida, deteniendo el script.")
        break
    }
    }, error = function(e) {
    # Imprimir error y continuar
    cat("Error parsing JSON:", e$message, "\n")
    cat("Output received from server:\n", paste(output, collapse="\n"), "\n")
    })
  
    contador <- contador + 1
}
