# Función para comparar pares de columnas homónimas
#' Title
#'
#' @param dataframe
#'
#' @return
#' @export
#'
#' @examples
#'

comparar_cols <- function(dataframe) {
  # Identificar pares de columnas homónimas
  nombres <- names(dataframe)
  pares <- nombres[grep("\\.x$", nombres)]
  pares <- sub("\\.x$", "", pares)

  # Inicializar un dataframe para almacenar los resultados
  resultados <- data.frame(anio = dataframe$anio)

  # Recorrer cada par y calcular la diferencia
  for (par in pares) {
    col_x <- paste(par, ".x", sep = "")
    col_y <- paste(par, ".y", sep = "")

    # Verificar si ambas columnas existen en el dataframe
    if (col_x %in% nombres && col_y %in% nombres) {
      # Calcular la diferencia y añadir al dataframe de resultados
      resultados[[par]] <- dataframe[[col_x]] - dataframe[[col_y]]
    }
  }

  return(resultados)
}




