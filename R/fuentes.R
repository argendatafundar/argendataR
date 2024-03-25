#' Fuentes 
#'
#' @return tibble con la sheet de fuentes raw+clean de googledrive Argendata
#' @export
#'

fuentes <- function() {
  
  df <- fuentes_raw() %>% 
    dplyr::left_join(fuentes_clean(),
                     "id_fuente"  = "id_fuente_raw") 
  
  
  df$id_fuente_clean <- replace(df$id_fuente_clean, which(is.na(df$id_fuente_clean)), 0)
  
  df$codigo <- sprintf("R%dC%d",df$id_fuente, df$id_fuente_clean)
  
  df
  
}
