#' Actualizar informacion de una fuente clean
#'
#' @description
#' Hace `drive_upload()` con overwrite = T pisando la version anterior de la fuente en el drive y actualiza 'timestamp' de una fuente en la sheet de fuentes clean en el drive de Argendata.
#' @details
#' La funcion toma el dato de path_clean declarado en `fuentes_clean()` para el id seleccionado y espera que exista el archivo correpondiente al path_clean. Ese archivo ser subirá al drive pisando el archivo preexistente.
#' La fecha será actualizada usando `Sys.time()` al momento de su ejecución.
#'
#' @param id_fuente_clean integer id numerico que permite seleccionar la fuente segun aparece en el sheet. Para consultar ids usar  `fuentes_clean()`
#'
#' @export
#'
#'

actualizar_fuente_clean <- function(id_fuente_clean) {

  stopifnot("'id_fuente_clean' debe ser id numerico de fuente o character con codigo de fuente" = is.numeric(id_fuente_clean) | is.character(id_fuente_clean))

  df_fuentes <- fuentes_clean()

  if (is.numeric(id_fuente_clean)) {

    stopifnot("'id_fuente_clean' no encontrado en sheet de fuentes. Ver `fuentes_clean()`." = id_fuente_clean %in% df_fuentes$id_fuente_clean )

  } else if (is.character(id_fuente_clean)) {

    stopifnot("'id_fuente_clean' no coincide con ningun codigo en sheet de fuentes. Ver `fuentes_clean()`." = id_fuente_clean %in% df_fuentes$codigo )
    id_fuente_clean <- regmatches(id_fuente_clean, m = regexpr("(?<=R)(\\d+)", text = id_fuente_clean, perl = T))

    id_fuente_clean <- as.numeric(id_fuente_clean)
  }



   fecha <- Sys.time()

  inputs <- list(
    "id_fuente_clean" = id_fuente_clean,
    "fecha" = fecha
    )

  df_fuentes <- df_fuentes[df_fuentes$id_fuente_clean == inputs$id_fuente_clean,]

  df_fuentes$fecha <-  inputs$fecha


  if (!file.exists(paste0("data/_FUENTES/clean/", df_fuentes$path_clean))) {
    stop("No se encontro el archivo clean en data/_FUENTES/clean/\nGuardarlo en la ubicacion antes de continuar")
  }


  print( df_fuentes[df_fuentes$id_fuente_clean == inputs$id_fuente_clean ,])

  googledrive::drive_upload(media = paste0("data/_FUENTES/clean/", df_fuentes$path_clean),
                            path = googledrive::as_id(fuentes_clean_dir()$id),
                            name = df_fuentes$path_clean, overwrite = T)


  df_fuentes %>%
    googlesheets4::range_write(col_names = F,
                               ss = fuentes_clean_sheet_id(),
                               range = sprintf("A%d:G%d", id_fuente_clean+1, id_fuente_clean+1))




}
