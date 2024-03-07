#' Expandir una serie usando variaciones de otra serie 
#'
#' @description
#' Toma un vector x con valores definidos de x1 a xn y con valores faltantes de xn+1 a xm y calcula los valores faltantes usando variaciones de otro vector var1 a varm como: xn+1 = xn*varn+1.
#'
#'
#' @param x variable con serie a expandir
#' @param var  variable con variaciones a usar para expandir la  serie
#'
#' @return vector con la serie expandida
#' @export
#'
#' 


expansor_xvar <- function(x,var) {
  for (i in 1:length(x)) {
    if (is.na(x[i])) {
      new_value <- x[i-1]*var[i]

      if (length(new_value) == 0) {
        new_value <- NA
      }

      x[i] <- new_value
    }
  }

  if (any(is.na(x))) {
    warning("La expansion del vector dejo valores faltantes")
  }

  x
}

