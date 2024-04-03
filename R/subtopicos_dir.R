#' subtopicos_dir
#'
#' @keywords internal
#' @export

subtopicos_dir <- function() {

  argendata_root_dir <- argendata_root_dir()

  filetemp <- list.files(tempdir(), full.names = T)[grepl("subtopicos_dir", list.files(tempdir()))]

  if (length(filetemp) == 1) {

    readr::read_rds(filetemp)

  } else {

    subtopicos_dir <- list()
    subtopicos_dir$id <- argendata_root_dir$id[argendata_root_dir$name == "SUBTOPICOS"]
    subtopicos_dir$tree <- googledrive::drive_ls(googledrive::as_id(subtopicos_dir$id))

    readr::write_rds(subtopicos_dir, tempfile(pattern = "subtopicos_dir_argdt", fileext = ".rds"))

    subtopicos_dir

  }

}
