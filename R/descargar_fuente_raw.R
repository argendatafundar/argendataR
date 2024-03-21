#' Descarga fuente version raw
#'
#' @param id_fuente id numerico de la fuente tal cual aparece en la sheet de fuentes. Ver `fuentes_raw()` 
#' @param path_raw nombre de la fuente tal cual aparece en la sheet de fuentes. Ver `fuentes_raw()`
#' @param dir directorio donde se descarga la fuente
#' 
#' @return file la fuente seleccionada de Fuentes/raw descargada en el directorio especificado
#' @export
#'

descargar_fuente_raw <- function(id_fuente, path_raw, dir) {
  
  stopifnot("Se requiere id_fuente o path_raw Definir 1 de los dos parametros" = xor(!missing(id_fuente), !missing(path_raw)))
  
  
  dir <- gsub("/$", "", dir)
  
  stopifnot("dir debe ser string de un directorio existente" = dir %in% gsub("^./","",list.dirs()))
  
  df_fuentes <- fuentes_raw()
  
  if (!missing(id_fuente)) {
    
    stopifnot("id fuente no es numeric o no fue encontrado en sheet fuentes" = is.numeric(id_fuente) & id_fuente %in% df_fuentes$id_fuente)
    
    path_raw <- df_fuentes[df_fuentes$id_fuente == id_fuente,][[ "path_raw"]]
    

    
  } 
  
  stopifnot("'path_raw' no es string o no fue encontrado en sheet `fuentes_raw()`" = is.character(path_raw) & path_raw %in% df_fuentes$path_raw)
  
  
  fuentes_raw_dir <- fuentes_raw_dir()$tree
  
  fuente_gd_id <- fuentes_raw_dir[fuentes_raw_dir$name == path_raw,][["id"]]
  
  googledrive::drive_download(file = googledrive::as_id(fuente_gd_id),
                              path = glue::glue("{dir}/{path_raw}"),
                              overwrite = T)
  
  }
