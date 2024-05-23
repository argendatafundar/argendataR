#' Deprecated
#'
#' @param x string Cadena de texto con funcionalidades de glue
#'
#' @return string Devuelve un string con la ruta
#'


armar_ruta <-
  function(x) {
    suppressWarnings(normalize_path(glue::glue(x)))
  }
