#' get_mapping
#' @description
#' Devuelve un dataframe con el mapping del subtopico publicado en main del repo transformers
#' 
#' @param subtopico string Nombre de 6 letras del subtopico
#'
#' @return dataframe Devuelve un dataframe con el mapping del subtopico
#' @export
#'
#' 

get_mapping <- function(subtopico) {
  
  subtopico <- toupper(subtopico)
  
  repo <- gh::gh(endpoint = "/repos/argendatafundar/transformers/git/trees/main?recursive=false")
  
  tree <- repo$tree
  
  paths <- sapply(tree, function(x) {x$path})
  
  lista_subtopicos <- grep("^[A-Z]{6}$", paths, value = T)
  
  assertthat::assert_that(length(subtopico) == 1)
  assertthat::assert_that(is.character(subtopico))
  assertthat::assert_that(nchar(subtopico) == 6)
  assertthat::assert_that(subtopico %in% lista_subtopicos)
  
  
  url_base <- glue::glue("https://raw.githubusercontent.com/argendatafundar/transformers/refs/heads/main/{subtopico}/mappings.json")
  
  # Manejar el error al intentar cargar el JSON
  mapping <- tryCatch(
    {
      # Intentar cargar el JSON
      jsonlite::fromJSON(url_base, flatten = TRUE)
    },
    error = function(e) {
      # Manejar el error
      message("Error al cargar el JSON: ", e$message)
      # Retornar un valor alternativo o NULL
      NULL
    }
  )
  
  # Verificar el resultado
  if (is.null(mapping)) {
    message("La variable 'mapping' no se pudo cargar debido a un error.")
  } else {
    message("El JSON se cargo correctamente.")
  }
  
  mapping <- dplyr::bind_rows(mapping, .id = "dataset")
  
  mapping
  
}
