#' Fuentes clean
#'
#' @return tibble con la sheet de fuentes clean de googledrive Argendata
#' @export
#'

fuentes_clean <- function() {

    df <- readr::read_csv(glue::glue("{IP_FUENTES()}/fuentes_clean.csv"), show_col_types = F, progress = F)

    df <- df %>%
      dplyr::filter(df$borrada != T | is.na(df$borrada)) 

    df

}


