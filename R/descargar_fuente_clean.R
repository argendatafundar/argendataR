#' Descarga fuente version clean
#'
#' @param id_fuente id numerico de la fuente tal cual aparece en la sheet de fuentes. Ver `fuentes_clean()` 
#' @param nombre nombre de la fuente tal cual aparece en la sheet de fuentes. Ver `fuentes_clean()`
#' @param dir directorio donde se descarga la fuente
#' 
#' @return file la fuente seleccionada de Fuentes/clean descargada en el directorio especificado
#' @export
#'

descargar_fuente_clean <- function(id_fuente, nombre, dir) {
  
  dir <- gsub("/$", "", dir)
  
  stopifnot("dir debe ser string de un directorio existente" = dir %in% gsub("^./","",list.dirs()))
  
  
  df_fuentes <- fuentes_clean()
  
  
  if (!missing(id_fuente)) {
    stopifnot("id fuente no es numeric o no fue encontrado en sheet fuentes" = is.numeric(id_fuente) & id_fuente %in% df_fuentes$id_fuente)
    
    fuente_path_clean <- df_fuentes[df_fuentes$id_fuente_clean == id_fuente,][["path_clean"]]
    
    fuentes_clean_dir <- fuentes_clean_dir()$tree
    
    fuente_gd_id <- fuentes_clean_dir[fuentes_clean_dir$name == fuentes_clean_dir,][["id"]]
    
  } else if (!missing(nombre)) {
    
    stopifnot("'nombre' no es string o no fue encontrado en sheet `fuentes_clean()`" = is.character(nombre) & nombre %in% df_fuentes$nombre)
    
    fuente_path_raw <- df_fuentes[df_fuentes$nombre == nombre,][[ "path_raw"]]
    
    fuentes_raw_dir <- fuentes_raw_dir()$tree
    
    fuente_gd_id <- fuentes_raw_dir[fuentes_raw_dir$name == fuente_path_raw,][["id"]]
    
  } else {
    stop("debe ingresar id_fuente o nombre")
  }
  
  
  googledrive::drive_download(file = fuente_gd_id,
                              path = glue::glue("{dir}/{fuente_path_raw}"),
                              overwrite = T)
  
}
