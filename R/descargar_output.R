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
  
    path <- gsub("\\.csv$","",nombre)
    
    path <- paste0(path, ".csv")
  
    output_url <- glue::glue("{GH_DATA_RAWURL()}/{subtopico}/{path}") 
    
    df <- tryCatch(
      httr2::req_perform(httr2::request(output_url)),
      httr2_http_404 = function(cnd) NULL
    )
    
    if (is.null(df)) {
      warning("Output no encontrado en repo 'data'.")
      # dowload or read output
      subtopico_outputs_df <- subtopico_outputs(subtopico_nombre = subtopico,
                                                entrega_subtopico = entrega_subtopico)
      
      id_output <- subtopico_outputs_df$id[grepl("cosasoa", subtopico_outputs_df$name)]
      
      stopifnot("Output no encontrada en el drive de Argendata" = length(id_output) != 0)
      
      stopifnot("Se encontro mas de una coincidencia en el drive de Argendata. Corregir filesystem del drive" = length(id_output) != 1)
      
      filetemp <- tempfile(pattern = sprintf("%s_%s_%s_argdt",
                                             nombre,
                                             entrega_subtopico,
                                             subtopico),
                           fileext = ".csv")
      
      googledrive::drive_download(file = googledrive::as_id(id_output),
                                  path = filetemp)
      
      output_drive <-  readr::read_delim(filetemp, ...)
    
    } else {
      
      output_drive <- rvest::html_text(rvest::read_html(df$body)) %>%
        readr::read_csv()
      
    }
    
    
    
    
    output_drive
    
  }

