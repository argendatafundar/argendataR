#' Descargar output desde el drive
#'
#' @param nombre nombre del archivo a descargar. La función usa grepl para identificar el archivo, si el nombre coincide con más de un archivo devuelve error.
#' @param subtopico codigo de 6 letras del subtopico en mayusculas.
#' @param entrega_subtopico nombre exacto de la carpeta de entrega donde buscar el output
#' @param ... parametros adicionales pasados a read_delim
#'
#' @description
#' La funcion descarga el archivo en la carpeta temporal de la sesión y hace read_delim desde allí. 
#' 
#' @export
#'
#' 

descargar_output <- function(nombre, subtopico, entrega_subtopico, ...) {
    
    limpiar_temps()
    
    # dowload or read output
    subtopico_outputs_df <- subtopico_outputs(subtopico_nombre = subtopico,
                                              entrega_subtopico = entrega_subtopico)
    
    id_output <- subtopico_outputs_df$id[grepl(nombre, subtopico_outputs_df$name)]
    
    filetemp <- tempfile(pattern = sprintf("%s_%s_%s_argdt",
                                          nombre,
                                          entrega_subtopico,
                                          subtopico),
                         fileext = ".csv")
    
    googledrive::drive_download(file = googledrive::as_id(id_output),
                                path = filetemp)
    
    output_drive <-  readr::read_delim(filetemp, ...)
    
    
    output_drive
    
  }

