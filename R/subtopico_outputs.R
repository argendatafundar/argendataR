#' Consulta lista de outputs del subtopico/entrega en drive
#'
#' @param subtopico_nombre codigo 6 letras del subtopico tal cual su nombre en el drive de Argendata
#' @param entrega_subtopico NULL deprecated
#'
#' @return dataframe con los outputs del subtopico para la entrega elegida
#' @export
#'

subtopico_outputs <- function(subtopico_nombre, entrega_subtopico = NULL ) {

  stopifnot("'subtopico_nombre' debe ser string" = is.character(subtopico_nombre))

  # stopifnot("'entrega_subtopico' debe ser string" = is.character(entrega_subtopico))

  subtopicos_dir_df <- subtopicos_dir()

  filetemp <- list.files(tempdir(), full.names = T)[grepl("subtopico_outputs", list.files(tempdir()))]

  if (length(filetemp) == 1) {

    readr::read_rds(filetemp)

  } else {

  files_subtopico <- googledrive::drive_ls(googledrive::as_id(subtopicos_dir_df$tree$id[subtopicos_dir_df$tree$name == subtopico_nombre]))

  datasets_id <- files_subtopico[files_subtopico["name"] == "datasets",][["id"]]

  datasets <- googledrive::drive_ls(googledrive::as_id(datasets_id))

  outputs_id <- datasets$id[datasets$name == "outputs"]

  outputs <- googledrive::drive_ls(googledrive::as_id(outputs_id))

  # entregas_id <- outputs[grepl(entrega_subtopico, outputs[["name"]]),][["id"]]
  #
  # entregas_dir <- googledrive::drive_ls(googledrive::as_id(entregas_id))


  readr::write_rds(outputs,
                   file = tempfile(pattern = "subtopico_outputs_argdt", fileext = ".rds"))

  outputs

  }
}
