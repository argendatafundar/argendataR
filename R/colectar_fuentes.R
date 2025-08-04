#' colectar_fuentes
#'
#' @param pattern string regex para capturar los nombres de variables en el entorno global
#'
#' @export 
#'
#'
#'
colectar_fuentes <- function(pattern = "^fuente.*"){

  # Genero un vector de codigos posibles
  posibles_codigos <- c(fuentes_raw()$codigo,fuentes_clean()$codigo)

  # Usar ls() para buscar variables en el entorno global
  variable_names <- ls(pattern = pattern, envir = globalenv())

  # Obtener los valores de esas variables
  valores <- unlist(mget(variable_names, envir = globalenv()))

  # Filtrar aquellas variables que sean de tipo character (string)
  # Esto es para que la comparacion sea posible en la linea de abajo
  strings <- valores[sapply(valores, is.character)]

  # solo devuelvo las fuentes que existen
  return(valores[valores %in% posibles_codigos])
}
