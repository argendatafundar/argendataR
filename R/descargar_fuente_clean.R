#' Descarga fuente version clean
#'
#' @param id_fuente id numerico de la fuente tal cual aparece en la sheet de fuentes. Ver `fuentes_clean()` 
#' @param path_clean path_clean de la fuente tal cual aparece en la sheet de fuentes. Ver `fuentes_clean()`
#' @param dir directorio donde se descarga la fuente
#' 
#' @return file la fuente seleccionada de Fuentes/clean descargada en el directorio especificado
#' @export
#'

descargar_fuente_clean <- function(id_fuente, path_clean, dir) {
  
  stopifnot("Se requiere id_fuente o path_clean. Definir 1 de los dos parametros" = xor(!missing(id_fuente), !missing(path_clean)))
  
  dir <- gsub("/$", "", dir)
  
  stopifnot("dir debe ser string de un directorio existente" = dir.exists(dir))
  
  
  df_fuentes <- fuentes_clean()
  
  
  if (!missing(id_fuente)) {
    
    stopifnot("id fuente no es numeric o no fue encontrado en sheet fuentes" = is.numeric(id_fuente) & id_fuente %in% df_fuentes$id_fuente_clean)
    
    path_clean <- df_fuentes[df_fuentes$id_fuente_clean == id_fuente,][["path_clean"]]
    
    
  } 
  
  stopifnot("'path_clean' no es string o no fue encontrado en sheet `fuentes_clean()`" = is.character(path_clean) & path_clean %in% df_fuentes$path_clean)
    
  
  fuentes_clean_dir <- fuentes_clean_dir()$tree
  
  fuente_gd_id <- fuentes_clean_dir[fuentes_clean_dir$name == path_clean,][["id"]]

  
  
  googledrive::drive_download(file = googledrive::as_id(fuente_gd_id),
                              path = glue::glue("{dir}/{path_clean}"),
                              overwrite = T)
  
}
