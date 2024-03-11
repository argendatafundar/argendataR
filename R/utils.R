
#' Directorio raiz de argendata
#'
#' @keywords internal
#'

argendata_root_dir <- function() {
  
  stopifnot("ARGENDATA_DRIVE no esta definido en .Renviron o esta mal escrito" = nchar(Sys.getenv("ARGENDATA_DRIVE")) == 33)
  
  argendata_root_dir <- googledrive::drive_ls(googledrive::as_id(Sys.getenv("ARGENDATA_DRIVE")))
  argendata_root_dir
}

#' Directorio bbdd de argendata
#'
#' @keywords internal
#'
bbdd_dir <- function() {

  argendata_root_dir <- argendata_root_dir()
  bbdd_dir <- list()
  bbdd_dir$id <- argendata_root_dir$id[argendata_root_dir$name == "BASES DE DATOS"] 
  bbdd_dir$tree <- googledrive::drive_ls(googledrive::as_id(bbdd_dir$id))
  bbdd_dir
}

#' Directorio fuentes de argendata
#'
#' @keywords internal
#'
fuentes_dir <- function(){
  bbdd_dir <- bbdd_dir()$tree
  fuentes_dir <- list()
  
  fuentes_dir$id <- bbdd_dir$id[bbdd_dir$name == "Fuentes"]
  fuentes_dir$tree <- googledrive::drive_ls(googledrive::as_id(fuentes_dir$id))
  fuentes_dir
}

#' fuentes_raw_sheet_id
#'
#' @keywords internal
#' @return id de sheet de fuentes

fuentes_raw_sheet_id <- function() {
  
  fuentes_dir <- fuentes_dir()$tree
  
  fuentes_raw_sheet_id <- fuentes_dir$id[fuentes_dir$name == "fuentes_raw"]
  
  fuentes_raw_sheet_id
}

#' fuentes_raw_sheet_id
#'
#' @keywords internal
#' @return id de sheet de fuentes

fuentes_clean_sheet_id <- function() {
  
  fuentes_dir <- fuentes_dir()$tree
  
  fuentes_clean_sheet_id <- fuentes_dir$id[fuentes_dir$name == "fuentes_clean"]
  
  fuentes_clean_sheet_id
}

#' lista de entradas dentro de Fuentes/raw
#'
#' @return tibble con el directorio de fuentes raw
#' @keywords internal
#' 

fuentes_raw_dir <- function() {
  fuentes_dir <- fuentes_dir()$tree
  fuentes_raw_dir <- list()
  fuentes_raw_dir$id <- fuentes_dir$id[fuentes_dir$name == "raw"]
  fuentes_raw_dir$tree <- googledrive::drive_ls(googledrive::as_id(fuentes_raw_dir$id))
  fuentes_raw_dir
}

#' lista de entradas dentro de Fuentes/clean
#'
#' @return tibble con el directorio de fuentes clean
#' @keywords internal
#' 

fuentes_clean_dir <- function() {
  fuentes_dir <- fuentes_dir()$tree
  fuentes_clean_dir <- list()
  fuentes_clean_dir$id <- fuentes_dir$id[fuentes_dir$name == "clean"]
  fuentes_clean_dir$tree <- googledrive::drive_ls(googledrive::as_id(fuentes_clean_dir$id))
  fuentes_clean_dir
}
