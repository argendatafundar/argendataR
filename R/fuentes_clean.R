#' Fuentes clean
#' 
#' @param limpiar_cache  Logical Si es TRUE borra el cache de consultas al drive de argendata. Si es FALSE reutiliza el cache existente.
#' @return tibble con la sheet de fuentes clean de googledrive Argendata
#' @export
#'

fuentes_clean <- function(limpiar_cache = F) {

  if(isTRUE(limpiar_cache)){
    limpiar_temps()
  }
  
  filetemp <- list.files(tempdir(), full.names = T)[grepl("fuentes_clean_argdt", list.files(tempdir()))]

  if (length(filetemp) == 1) {

    readr::read_csv(filetemp) %>%
      suppressMessages()

  } else {
    df <- googlesheets4::read_sheet(ss = fuentes_clean_sheet_id())

    df %>%
      readr::write_csv(tempfile("fuentes_clean_argdt", fileext = ".csv")) %>%
      suppressMessages()

    df
  }



}
