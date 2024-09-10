#' Lee una fuente de fuentes de Argendata desde el drive
#'
#' @param id_fuente string con el codigo o id numerico con la fuente tal cual aparece en `fuentes_clean()`
#'
#' @return Descarga el archivo correspondiente al codigo de fuente desde el drive de Argendata
#' @export
#'


read_parquet_srv <- function(id_fuente) {

  df_fuentes <- fuentes_clean()

  if (is.numeric(id_fuente)) {

    stopifnot("'id_fuente' no coincide con ningun 'id_fuente' en sheet fuentes_clean" = id_fuente %in% df_fuentes$id_fuente_clean)

    codigo <- df_fuentes[["codigo"]][df_fuentes["id_fuente_clean"] == id_fuente]

  } else if (is.character(id_fuente)) {

    stopifnot("'id_fuente' no coincide con ningun 'codigo' en sheet de fuentes. Ver `fuentes_clean()`." = id_fuente %in% df_fuentes$codigo )

    codigo <- id_fuente

  }

  path_clean <- df_fuentes[df_fuentes$codigo == codigo,][[ "path_clean"]]

  if (SERVER_USER_CALL()) {
    arrow::read_parquet(glue::glue("{RUTA_FUENTES()}/clean/{path_clean}"))
  } else {
    arrow::read_parquet(glue::glue("{IP_FUENTES()}/clean/{path_clean}"))
  }
}
