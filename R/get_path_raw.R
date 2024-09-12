#' Devuelve el full path de un codigo de fuente raw
#'
#' @param codigo string Codigo de la fuente raw para la cual se busca el path de referencia. Ej: `"R\\d{1,2}C0"`
#' 
#' @export
#'
#'

get_raw_path <- function(codigo){
  
  df_fuentes_raw <- fuentes_raw() 
  
  stopifnot("Codigo no registrado en fuentes_raw()" = codigo %in% df_fuentes_raw$codigo)
  
  path_raw <- df_fuentes_raw[df_fuentes_raw$codigo == codigo, "path_raw"]
  
  if (SERVER_USER_CALL()) {
    
    glue::glue("{RUTA_FUENTES()}/raw/{path_raw}")
    
  } else {
    
    glue::glue("{IP_FUENTES()}/raw/{path_raw}")
    
  }
  
  
}