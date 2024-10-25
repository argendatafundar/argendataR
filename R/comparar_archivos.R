#' Comparar dos archivos
#'
#' @param x data.frame dataframe a comparar con el output cargado en el drive
#' @param y data.frame dataframe anterior que se usa como referencia de comparacion. Ver `argendataR::descargar_output()`.
#' @description
#' El resultado es una lista que contiene cuales son las columnas faltantes (cols_faltantes), cuales son las columnas nuevas (cols_nuevas),
#' un dataframe que compara las clases de columnas viejas y columnas nuevas (comparacion_clases),
#' la cantidad de filas de diferencia entre el nuevo dataset y el viejo (diferencia_nfilas), y una lista con metricas de comparacion por columna (comparacion_cols).
#' La lista de comparacion_cols presenta por cada columna la cantidad de nuevos NA (nuevos_na), la cantidad de mismatches para la columna en funcion de los PK (suma_mismatches) y
#' ademas un dataframe (metricas_filas) que indica para la columna analizada para cada tupla de valores PK las diferencias absolutas (abs(Xi/Xi-1)) y diferencias relativas (abs(Xi/Xi-1)/abs(Xi-1))
#'
#' @return Devuelve una lista con los resultados de los chequeos realizados
#' @export
#'
#'


comparar_archivos <- function(x, y) {
  
  path_x <- normalize_path(x)
  
  path_y <- normalize_path(y)
  
  
  msj <- glue::glue("Archivo {path_x} no encontrado")
  assertthat::assert_that(file.exists(path_x), msg = msj)
  
  msj <- glue::glue("Archivo {path_y} no encontrado")
  assertthat::assert_that(file.exists(path_y), msg = msj)
  
  
}
