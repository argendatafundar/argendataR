#' Title
#'
#' @description
#' para empalmar una serie usando variaciones de otra serie
#' dada una columna x con los valores de la serie a expandir: Xi a Xn de 1 a N
#' y dada una columna var donde VARi = Xi/Xi-1 como las proporciones entre un valor de x y su antecedente
#' si Xi es NA lo calcula y reemplaza como Xi-1*VARi, si no es NA lo deja tal cual
#'
#'
#' @param x
#' @param var
#'
#' @return
#' @export
#'
#' @examples


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

