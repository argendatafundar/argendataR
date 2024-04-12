#' Consulta los full path de archivos en el directorio temporal
#'
#' @param pattern string Patron de texto del nombre de archivo a recuperar. Ej: `"R37C1"` o `"R\\d{1,2}C0"`
#' 
#' @export
#'

get_temp_path <- function(pattern = NULL) {
  
  dir <- tempdir()
  list.files(dir, full.names = T)[grepl(pattern, list.files(dir, full.names = T))]
  
}