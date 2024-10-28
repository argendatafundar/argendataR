#' Devuelve el full path de un codigo de fuente raw
#'
#' @param codigo string Codigo de la fuente raw para la cual se busca el path de referencia. Ej: `"R\\d{1,2}C0"`
#'
#' @export
#'
#'

get_clean_path <- function(codigo){

  df_fuentes_clean <- fuentes_clean()

  stopifnot("Codigo no registrado en fuentes_clean()" = codigo %in% df_fuentes_clean$codigo)

  path_clean <- df_fuentes_clean[df_fuentes_clean$codigo == codigo, "path_clean"]

  if (SERVER_USER_CALL()) {

    glue::glue("{RUTA_FUENTES()}/clean/{path_clean}")

  } else {

    glue::glue("{IP_FUENTES()}/clean/{path_clean}")

  }


}
