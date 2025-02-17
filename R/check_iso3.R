#' Verifica códigos ISO3
#'
#' @description
#' Esta función verifica si todos los elementos de un vector están presentes en un conjunto de códigos ISO3 validados.
#'
#' @param x Un vector de caracteres que contiene los códigos ISO3 a verificar.
#' 
#' @return Devuelve un valor lógico TRUE si todos los códigos están validados. Si no, devuelve un vector lógico indicando cuáles códigos no son válidos.
#' 
#' @examples
#' # Ejemplo de uso
#' check_iso3(c("ARG", "BRA", "USA"))
#' 
#' @export
check_iso3 <- function(x) {
  # Validación de entrada
  if (!is.character(x)) {
    stop("El argumento 'x' debe ser un vector de caracteres.")
  }
  

    geonomenclador <- tryCatch({
      jsonlite::fromJSON(URL_GEONOMENCLADOR(),
       flatten = TRUE)
    }, error = function(e) {
      stop("Error al cargar el geonomenclador: ", e$message)
    })

  # Verificar códigos ISO3
  check <- all(x %in% geonomenclador$iso3)
  
  if (isFALSE(check)) {
    # Identificar valores no válidos
    invalid_values <- unique(x[!x %in% geonomenclador$iso3])
    
    warning("Algunos valores de 'x' no son codigos ISO3 validados en geonomenclador: ", paste(invalid_values, collapse = ", "))
    flush.console()
    
    y <- x %in% geonomenclador$iso3
    names(y) <- x
    return(y)

  } else {

    return(check)
    
  }
}
