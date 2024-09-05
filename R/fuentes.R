#' Fuentes
#'
#' @param limpiar_cache  Logical Si es TRUE borra el cache de consultas al drive de argendata. Si es FALSE reutiliza el cache existente.
#' @return tibble con la sheet de fuentes raw+clean de googledrive Argendata
#' @export
#'

fuentes <- function() {

  df_r <- fuentes_raw()
  
  df_r <- df_r %>%
    dplyr::rename_with(.fn = function(x) {paste0(x,"_raw")})

  df_c <- fuentes_clean()

  df_c <- df_c %>%
    dplyr::rename_with(.cols = -c("id_fuente_raw", "id_fuente_clean", "path_clean"),
                .fn = function(x) paste0(x, "_clean"))

  df_c <- dplyr::left_join(df_c, df_r, by = c("id_fuente_raw" = "id_fuente_raw"))


  df <- dplyr::bind_rows(df_r, df_c)

  df
}
