#' Consulta metadata de subtopicos
#'
#' @param subtopico string Texto con el codigo de 6 letras del subtopico o patron de regex
#'
#' @return dataframe con la metadata
#' @export
#'


metadata <- function(subtopico = NULL) {
  
  stopifnot("'subtopico' debe ser string con codigo de 6 letras de subtopico" = is.character(subtopico))
  
  subtopico <- toupper(subtopico)
  
  # Lista los archivos o carpetas dentro de la carpeta de subtemas utilizando su ID
  paths_subtopicos <- subtopicos_dir()$tree
  
  stopifnot("'subtopico' no hallado en el drive" = any(grepl(subtopico, paths_subtopicos$name)))
  
  paths_subtopicos <- paths_subtopicos[grepl(subtopico, paths_subtopicos$name),]
  
  # Para cada ID en paths_subtopicos, lista los archivos dentro y los recopila en una lista
  files_subtopicos <- purrr::map(paths_subtopicos$id, function(x) {
    googledrive::drive_ls(googledrive::as_id(x))
  })
  
  # Combina los dataframes de archivos de subtemas en uno solo
  files_subtopicos <- dplyr::bind_rows(files_subtopicos)
  
  # Filtra los archivos de metadatos que contienen "argendata -" en su nombre, excluyendo aquellos que contienen "ejemplo"
  metadata_files <- files_subtopicos %>% 
    dplyr::filter(grepl("argendata -", tolower(name))) %>% 
    dplyr::filter(! grepl("ejemplo", tolower(name)))
  
  # Para cada archivo de metadatos, lee su contenido saltando las primeras 6 filas y asumiendo que las columnas son tipo texto
  metadata <- purrr::map2(metadata_files$id, metadata_files$name, function(x, y) {
    googlesheets4::read_sheet(x, skip = 6, col_types = "c") %>% 
      dplyr::mutate(subtopico_nombre = gsub("ArgenData - ", "", y))
  })
  
  # Combina los datos leídos de todos los archivos de metadatos en un único dataframe
  metadata <- dplyr::bind_rows(metadata)
  
  # Filtra el dataframe resultante para eliminar las filas donde todos los valores son NA (datos faltantes)
  metadata <- metadata %>% 
    dplyr::filter(!dplyr::if_all(dplyr::everything(), is.na))
}
