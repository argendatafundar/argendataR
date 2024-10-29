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

#' Consulta el path de fuentes
#'
#' @param codigo string Codigo de fuente cuyo path se busca. Ej: `"R37C1"` o `"R37C0"`
#' 
#' @returns string Path de la fuente
#' @export
#'

get_fuente_path <- function(codigo = NULL) {
  
  df_clean <- fuentes_clean()
  df_raw <- fuentes_raw()
  
  
  codigo %in% df_clean$codigo | codigo %in% df_raw$codigo
  
  stopifnot("codigo debe ser character" = is.character(codigo))
  
  row_clean <- which(codigo == fuentes_clean()$codigo)
  row_raw <- which(codigo == fuentes_raw()$codigo)
  
  stopifnot("Error: codigo figura en tabla clean y en tabla raw" = !(length(row_clean) == 1 & length(row_raw) == 1))
  stopifnot("Error: codigo no figura en tabla clean ni en tabla raw" = !(length(row_clean) == 0 & length(row_raw) == 0))
  stopifnot("Error: codigo con multiples coincidencias" = length(row_clean) == 1 | length(row_raw) == 1)
  
  if (length(row_clean) == 1) {
    
    get_clean_path(codigo)
    
  } else if (length(row_raw)) {
    
    get_raw_path(codigo)
    
  } else {
    
    stop("Error no previsto")
  }
  
}