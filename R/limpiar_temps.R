#' Borra archivos temporales usados por el paquete argendataR
#'
#' @export
#'
#'

limpiar_temps <- function() {

  archivos <- list.files(tempdir(), "_argdt", full.names = T)

  purrr::walk(archivos, function(x) {file.remove(x)})

  warning(sprintf("Se borraron %d archivos temporales", length(archivos)))

}
