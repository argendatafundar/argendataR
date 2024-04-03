#' Descarga de fuentes de Argendata desde el drive
#'
#' @param codigo string Codigo de la fuente tal cual aparece en la consulta a 
#' @param dir string directorio donde se descarga la fuente. Si es NULL toma `getwd()`
#'
#' @return Descarga el archivo correspondiente al codigo de fuente desde el drive de Argendata
#' @export
#'

descargar_fuente <- function(codigo, dir = NULL) {
  
  df <- fuentes()
  
  if (is.null(dir)) {
    dir <- getwd()
  }
  
  
  stopifnot("'dir' debe ser string" = is.character(dir))
  
  stopifnot("'dir' debe ser path a ubicacion existente" = dir.exists(dir))
  
  stopifnot("'codigo' debe ser string" = is.character(codigo))
  stopifnot("'codigo' no corresponde a una fuente cargadas. Ver `fuentes()`" = codigo %in% df[["codigo"]])
  
  if (grepl("R\\d+C0", codigo)) { 
    
    # codigo <- regmatches(codigo, m = regexpr("(?<=R)(\\d+)", text = codigo, perl = T))
    
    # id <- as.numeric(codigo)
    
    descargar_fuente_raw(id_fuente = codigo, dir = dir)
    
  } else if (grepl("C[1-9]+", codigo)) {
      
    # codigo <- regmatches(codigo, m = regexpr("(?<=C)(\\d+)", text = codigo, perl = T))
    
    # id <- as.numeric(codigo)
    
    descargar_fuente_clean(id_fuente = codigo, dir = dir)
    
    
  }
  
  
}