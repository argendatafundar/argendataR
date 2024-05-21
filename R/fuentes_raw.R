#' Fuentes raw
#'
#' @param limpiar_cache  Logical Si es TRUE borra el cache de consultas al drive de argendata. Si es FALSE reutiliza el cache existente.
#' @return tibble con la sheet de fuentes raw de googledrive Argendata
#' @export
#'

fuentes_raw <- function(limpiar_cache = T) {

  if(isTRUE(limpiar_cache)){
    limpiar_temps()
  }

  filetemp <- list.files(tempdir(), full.names = T)[grepl("fuentes_raw_argdt", list.files(tempdir()))]

  if (length(filetemp) == 1) {

    readr::read_csv(filetemp) %>%
      suppressMessages()

  } else {
    df <- googlesheets4::read_sheet(ss = fuentes_raw_sheet_id())

    df <- df %>%
      dplyr::filter(df$borrada != T | is.na(df$borrada)) %>%
      readr::write_csv(file = tempfile("fuentes_raw_argdt",
                                fileext = ".csv")) %>%
      suppressMessages()

    df
  }


}
