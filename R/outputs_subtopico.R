#' outputs_subtopico
#'
#' @param subtopico_nombre codigo 6 letras del subtopico tal cual su nombre en el drive de Argendata
#' @param entrega_subtopico nombre de la entrega a buscar tal cual su nombre en el drive de Argendata
#'
#' @return dataframe con los outputs del subtopico para la entrega elegida
#' @export
#'

outputs_subtopico <- function(subtopico_nombre, entrega_subtopico) {

  files_subtopico <- googledrive::drive_ls(subtopicos()$id[subtopicos()$name == subtopico_nombre])

  files_subtopico <- files_subtopico[files_subtopico["name"] == "datasets", ]

  outputs_id <- googledrive::drive_ls(files_subtopico[["id"]])

  outputs <- googledrive::drive_ls(outputs_id[grepl(entrega_subtopico, outputs_id[["name"]])][["id"]])


  outputs
}
