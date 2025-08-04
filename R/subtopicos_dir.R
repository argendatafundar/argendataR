#' subtopicos_dir
#'
#' @param version (deprecated) Solo motivos de compatibilidad
#' @keywords internal
#' @export

subtopicos_dir <- function(version=NULL) {
  

  version <- "v2"

  search_string = 'subtopicos_dir'
  temp_out <- tempfile(pattern = "subtopicos_dir_argdt", fileext = ".rds")
  folder_name <- "SUBTOPICOS"
  
  if (version == 'v2'){
    
    search_string = 'subtopicos_dir.*v2'
    temp_out <- tempfile(pattern = "subtopicos_dir_argdt_v2", fileext = ".rds")
    folder_name <- "TOPICOS"
  
    }
  
  argendata_root_dir <- argendata_root_dir(version)

  filetemp <- list.files(tempdir(), full.names = T)[grepl(search_string, list.files(tempdir()))]

  if (length(filetemp) == 1) {

    readr::read_rds(filetemp)

  } else {

    subtopicos_dir <- list()
    subtopicos_dir$id <- argendata_root_dir$id[argendata_root_dir$name == folder_name]
    subtopicos_dir$tree <- googledrive::drive_ls(googledrive::as_id(subtopicos_dir$id))

    readr::write_rds(subtopicos_dir, temp_out)

    subtopicos_dir

  }

}
