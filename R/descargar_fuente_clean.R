#' Descarga fuente version clean
#'
#' @param id_fuente id numerico de la fuente tal cual aparece en la sheet de fuentes. Ver `fuentes()` 
#'
#' @return descarga la fuente seleccionada de Fuentes/raw
#' @export
#'

descargar_fuente_clean <- function(id_fuente) {
  
  df_fuentes <- fuentes_clean()
  
  stopifnot("id fuente no es numeric o no fue encontrado en sheet `fuentes_clean()`" = is.numeric(id_fuente) & id_fuente %in% df_fuentes$id_fuente_clean)
  
  fuente_path_clean <- df_fuentes[df_fuentes$id_fuente_clean == id_fuente, "path_clean"]
  
  fuentes_clean_dir <- fuentes_clean_dir()$tree
  
  fuente_gd_id <- fuentes_clean_dir[fuentes_clean_dir$name == fuentes_clean_dir,"id"]
  
  googledrive::drive_download(googledrive::as_id(fuente_gd_id),
                              path = fuente_path_clean, overwrite = T)
  
}
