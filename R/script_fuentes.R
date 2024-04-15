#' Crea un .R con el esquema basico de script para outputs
#'
#'
#' @param path string Ruta y nombre del archivo a crear
#' @param .navigate logical Si es TRUE abre el editor del archivo creado. FALSE (default).
#' @returns Crea un archivo .R con el esquema basico para un script de subtopico
#' @export
#'


script_fuentes <- function(path = NULL, .navigate = F) {


  stopifnot("path debe ser charaacter de largo 1" = is.character(path) & length(path) == 1)


  if (!grepl("\\.R$", path)) {
    path <- paste0(path,".R")
  }


  stopifnot("Ya existe un archivo con el path especificado" = !file.exists(path))

  file.create(path)

  lineas_base <- readLines(fs::path_package("argendataR", "fuentes_SUBTOP.txt") )

  stringi::stri_write_lines(lineas_base, con = path,encoding = "UTF-8", sep = "\n")


  if (!rstudioapi::isAvailable()) {
    sprintf("Se creo el archivo %s con el esquema de descarga de fuentes", path)
    return(NULL)
  }

  if (isTRUE(.navigate)) {
    rstudioapi::navigateToFile(file = path)
  }



}
