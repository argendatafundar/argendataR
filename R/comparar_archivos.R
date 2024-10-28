#' Comparar dos archivos
#'
#' @param x string path a un archivo
#' @param y string path a un archivo
#' @description
#' Compara dos archivos y devuelve una lista de controles
#' @return Devuelve una lista con los resultados de los chequeos realizados
#' @export
#'
#'


comparar_archivos <- function(x, y) {
  
  path_x <- normalize_path(x)
  path_y <- normalize_path(y)
  
  
  
  msj <- glue::glue("Archivo {path_x} no encontrado")
  print(file.exists(path_x))
  assertthat::assert_that(file.exists(path_x), msg = msj)
  
  print(file.exists(path_y))
  msj <- glue::glue("Archivo {path_y} no encontrado")
  assertthat::assert_that(file.exists(path_y), msg = msj)
  
  size_x <- file.size(path_x)
  print(size_x)
  size_y <- file.size(path_y)
  print(size_y)
  
  time_x <- file.mtime(path_x)
  print(time_x)
  time_y <- file.mtime(path_y)
  print(time_y)
  hash_x <-  digest::digest(path_x, algo = "md5", serialize = F, file = T)
  print(hash_x)
  
  hash_y <-  digest::digest(path_y, algo = "md5", serialize = F, file = T)
  print(hash_y)
  
  if ( size_x == size_y ) {
    message("Los archivos tienen el mismo peso")
  } else {
    message("Los archivos no tienen el mismo peso")
  }
  
  if ( hash_x == hash_y ) {
    message("Los archivos tienen el mismo hash md5")
  } else {
    message("Los archivos no tienen el mismo hash md5")
  }
  
  if (size_x ==  size_y & hash_x == hash_y) {
    cambio <- FALSE
  } else {
    cambio <- TRUE
  }
  
  rpta <- list(
    "cambio" = cambio,
    "path_x" = path_x,
    "path_y" = path_y,
    "size_x" = size_x,
    "size_y" = size_y,
    "hash_x" = hash_x,
    "hash_y" = hash_y,
    "time_x" = time_x,
    "time_y" = time_y
    
  )
  
  rpta
  
}
