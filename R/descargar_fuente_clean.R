#' Descarga fuente version clean
#'
#' @param id_fuente id numerico de la fuente tal cual aparece en la sheet de fuentes. Ver `fuentes_clean()`
#' @param dir directorio donde se descarga la fuente
#' @param limpiar_cache  Logical Si es TRUE borra el cache de consultas al drive de argendata. Si es FALSE reutiliza el cache existente.
#'
#' @return file la fuente seleccionada de Fuentes/clean descargada en el directorio especificado
#' @export
#'

descargar_fuente_clean <- function(id_fuente, dir = NULL, limpiar_cache = F) {


  if (is.null(dir)) {
    dir <- tempdir()
  }

  dir <- gsub("/$", "", dir)

  stopifnot("dir debe ser string de un directorio existente" = dir.exists(dir))


  df_fuentes <- fuentes_clean()

  stopifnot("'id_fuente' debe ser numeric con id_fuente o character con codigo de fuente" = is.character(id_fuente) | is.numeric(id_fuente))

  if (is.numeric(id_fuente)) {

    stopifnot("'id_fuente' no coincide con ningun 'id_fuente' en sheet fuentes_clean" = id_fuente %in% df_fuentes$id_fuente_clean)

    codigo <- df_fuentes[["codigo"]][df_fuentes["id_fuente_clean"] == id_fuente]

  } else if (is.character(id_fuente)) {

    stopifnot("'id_fuente' no coincide con ningun 'codigo' en sheet de fuentes. Ver `fuentes_clean()`." = id_fuente %in% df_fuentes$codigo )

    codigo <- id_fuente

  }

  path_clean <- df_fuentes[df_fuentes$codigo == codigo,][[ "path_clean"]]

  path_clean_body <- gsub("\\.[^\\.]*$", "", path_clean)

  ext <- regmatches(path_clean, m = regexpr("\\.[^\\.]*$", text = path_clean, perl = T))


  utils::download.file(url = glue::glue("{IP_FUENTES()}/clean/{path_clean}"),
                destfile = glue::glue("{dir}/{path_clean_body}_{codigo}{ext}"),
                mode = "wb")



}
