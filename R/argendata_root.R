
#' Title
#'
#' @export
#' @keywords internal
#'

argendata_root <- function() {
  drive_root <- googledrive::drive_ls(path = googledrive::as_id(Sys.getenv('ARGENDATA_DRIVE')))

}
