#' Fuentes 
#'
#' @return tibble con la sheet de fuentes raw+clean de googledrive Argendata
#' @export
#'

fuentes <- function() {
  
  df <- tidyr::as_tibble(dplyr::left_join(fuentes_raw(), fuentes_clean(),
                     by = c("id_fuente"  = "id_fuente_raw")))
  
  df$id_fuente_clean <- replace(df$id_fuente_clean, which(is.na(df$id_fuente_clean)), 0)
  
  df$codigo <- sprintf("R%dC%d",df$id_fuente, df$id_fuente_clean)
  
  df <- dplyr::bind_cols(df[,which(colnames(df) == "codigo")], df[,-which(colnames(df) == "codigo")])
  
  df
}
