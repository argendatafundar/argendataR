#' Title
#'
#' @param subtopico_nombre
#' @param entrega_subtopico
#'
#' @return
#' @export
#'
#' @examples
outputs_subtopico <- function(subtopico_nombre, entrega_subtopico) {
  # eleccion del subtopico
  # subtopico <- subtopicos$name[subtopicos$name == x]

  # levanta los outputs del subtopico

  files_subtopico <- googledrive::drive_ls(subtopicos()$id[subtopicos()$name == subtopico_nombre])

  files_subtopico <- files_subtopico[files_subtopico["name"] == "datasets", ]

  outputs_id <- googledrive::drive_ls(files_subtopico[["id"]])

  outputs <- googledrive::drive_ls(outputs_id[grepl(entrega_subtopico, outputs_id[["name"]])][["id"]])


  outputs
}
