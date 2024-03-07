#' subtopicos
#'
#' @keywords internal
#' @export

subtopicos <- function() {

  argendata_root_dir <- argendata_root_dir()
  subtopicos_dir <- list()
  subtopicos_dir$id <- argendata_root_dir$id[argendata_root_dir$name == "SUBTOPICOS"] 
  subtopicos_dir$tree <- googledrive::drive_ls(googledrive::as_id(subtopicos_dir$id))
  subtopicos_dir

  }
