#' Armar path a un archivo usando normalizePath(glue())
#'
#' @param x string Cadena de texto con funcionalidades de glue
#'
#' @return string Devuelve un string con la ruta
#'


armar_ruta <-
  function(x) {
    suppressWarnings(normalizePath(glue::glue(x)))
  }
