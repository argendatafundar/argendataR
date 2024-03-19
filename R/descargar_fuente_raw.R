#' Descarga fuente version raw
#'
#' @param id_fuente id numerico de la fuente tal cual aparece en la sheet de fuentes. Ver `fuentes_raw()` 
#' @param nombre nombre de la fuente tal cual aparece en la sheet de fuentes. Ver `fuentes_raw()`
#' @param dir directorio donde se descarga la fuente
#' 
#' @return file la fuente seleccionada de Fuentes/raw descargada en el directorio especificado
#' @export
#'

descargar_fuente_raw <- function(id_fuente, nombre, dir) {
  
  dir <- gsub("/$", "", dir)
  
  stopifnot("dir debe ser string de un directorio existente" = dir %in% gsub("^./","",list.dirs()))
  
  df_fuentes <- fuentes_raw()
  
  if (!missing(id_fuente)) {
    stopifnot("id fuente no es numeric o no fue encontrado en sheet fuentes" = is.numeric(id_fuente) & id_fuente %in% df_fuentes$id_fuente)
    
    fuente_path_raw <- df_fuentes[df_fuentes$id_fuente == id_fuente,][[ "path_raw"]]
    
    fuentes_raw_dir <- fuentes_raw_dir()$tree
    
    fuente_gd_id <- fuentes_raw_dir[fuentes_raw_dir$name == fuente_path_raw,][["id"]]
    
  } else if (!missing(nombre)) {
    
    stopifnot("nombre fuente no fue encontrado en sheet fuentes" = nombre %in% df_fuentes$nombre)
    
    fuente_path_raw <- df_fuentes[df_fuentes$nombre == nombre,][[ "path_raw"]]
    
    fuentes_raw_dir <- fuentes_raw_dir()$tree
    
    fuente_gd_id <- fuentes_raw_dir[fuentes_raw_dir$name == fuente_path_raw,][["id"]]
    
  } else {
    stop("debe ingresar id_fuente o nombre")
  }
  

  
  googledrive::drive_download(file = googledrive::as_id(fuente_gd_id),
                              path = glue::glue("{dir}/{fuente_path_raw}"),
                              overwrite = T)
  
  }
