#' Actualizar informacion de una fuente
#'
#' @description
#' Actualiza 'fecha_descarga' y 'fecha_actualizar' de una fuente en la sheet de fuentes en el drive de Argendata. Hace `drive_upload()` con overwrite = T pisando la version anterior de la fuente en el drive.
#'
#'
#' @param id_fuente integer id numerico que permite seleccionar la fuente segun aparece en el sheet. Para consultar ids usar  `fuentes()`
#' @param fecha_descarga string or date Valor de la fecha de descarga. Puede ser clase 'date' como producto de `Sys.Date()` o un string parseable por `as.Date()`
#' @param fecha_actualizar string or date Valor de la fecha en que se debe actualizar la descarga. Puede ser clase 'date' como producto de `Sys.Date()` o un string parseable por `as.Date()`
#'
#' @export
#'
#' 

actualizar_fuente <- function(id_fuente,
                              fecha_descarga= NULL,
                              fecha_actualizar = NULL) {


  stopifnot("'id_fuente' debe ser numerico" = is.numeric(id_fuente))

  df_fuentes <- fuentes()


  stopifnot("'id_fuente' no encontrado en sheet de fuentes. Ver `fuentes()`." = id_fuente %in% df_fuentes$id_fuente )

  if (is.null(fecha_descarga)) {
    fecha_descarga <- Sys.Date()
  } else {
    fecha_descarga <- as.Date(fecha_descarga)
    stopifnot("'fecha_descarga' debe ser null o fecha o string parseable a fecha" = !is.na(inputs$fecha_descarga))
  }

  inputs <- list(
    "id_fuente" = id_fuente,
    "fecha_descarga" = fecha_descarga ,
    "fecha_actualizar" =  fecha_actualizar
  )

  inputs <- inputs[sapply(inputs, function(x) !is.null(x))]


  df_fuentes <- df_fuentes[df_fuentes$id_fuente == id_fuente,]

  for (i in names(inputs)) {

    df_fuentes[df_fuentes$id_fuente == inputs$id_fuente , i] <-  inputs[i]

  }


  print( df_fuentes[df_fuentes$id_fuente == inputs$id_fuente ,])
  

  df_fuentes |>
    googlesheets4::range_write(ss = fuentes_sheet_id(),
                               range = sprintf("A%d:I%d", id_fuente))
  

  googledrive::drive_upload(media = paste0("data/_FUENTES/raw/", df_fuentes$path_raw),
                            path = googledrive::as_id(fuentes_raw_dir()$id),
                            name = df_fuentes$path_raw, overwrite = T)

  }
