#' Descarga fuente version raw
#'
#' @param id_fuente id numerico de la fuente tal cual aparece en la sheet de fuentes. Ver `fuentes()` 
#'
#' @return descarga la fuente seleccionada de Fuentes/raw
#' @export
#'

descargar_fuente_raw <- function(id_fuente) {
  
  df_fuentes <- fuentes_raw()
  
  stopifnot("id fuente no es numeric o no fue encontrado en sheet fuentes" = is.numeric(id_fuente) & id_fuente %in% df_fuentes$id_fuente)

  fuente_path_raw <- df_fuentes[df_fuentes$id_fuente == id_fuente, "path_raw"]
  
  fuentes_raw_dir <- fuentes_raw_dir()$tree
  
  fuente_gd_id <- fuentes_raw_dir[fuentes_raw_dir$name == fuente_path_raw,"id"]
  
  googledrive::drive_download(googledrive::as_id(fuente_gd_id),
                              path = fuente_path_raw, overwrite = T)
  
}
