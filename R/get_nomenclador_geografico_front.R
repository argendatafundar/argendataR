#' Devuelve el nomenclador geografico para argendata
#'
#' @return tibble con la sheet de consolidado_fundar del drive de Argendata
#' @export
#'

get_nomenclador_geografico_front <- function() {
  # Definir el nombre del archivo de caché
  cache_file <- file.path(tempdir(), "nomenclador_geografico_cache_argdt.rds")
  
  # Verificar si el archivo de caché ya existe
  if (file.exists(cache_file)) {
    # Leer el nomenclador desde el caché
    nomenclador <- readRDS(cache_file)
  } else {
    # Obtener la URL del nomenclador desde utils.R
    url_nomenclador <- URL_GEONOMENCLADOR_FRONT()
    
    # Descargar y leer el archivo Excel directamente desde la URL
    nomenclador <- tryCatch(
      {
        jsonlite::fromJSON(url_nomenclador, flatten = TRUE)
      },
      error = function(e) {
        stop("Error al descargar o leer el nomenclador desde la URL: ", e$message)
      }
    )
    
    # Guardar el nomenclador en el archivo de caché
    saveRDS(nomenclador, cache_file)
  }
  
  return(nomenclador)
}
