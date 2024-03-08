#' Actualizar informacion de una fuente clean
#'
#' @description
#' Actualiza 'fecha' de una fuente en la sheet de fuentes clean en el drive de Argendata y hace `drive_upload()` con overwrite = T pisando la version anterior de la fuente en el drive.
#'
#'
#' @param id_fuente_clean integer id numerico que permite seleccionar la fuente segun aparece en el sheet. Para consultar ids usar  `fuentes_raw()`
#' @param fecha string or date Valor de la fecha de descarga. Puede ser clase 'date' como producto de `Sys.Date()` o un string parseable por `as.Date()`
#'
#' @export
#'
#' 

actualizar_fuente_clean <- function(id_fuente_clean,
                                  fecha = NULL) {
  
  
  stopifnot("'id_fuente_clean' debe ser numerico" = is.numeric(id_fuente_clean))
  
  df_fuentes <- fuentes_clean()
  
  
  stopifnot("'id_fuente_clean' no encontrado en sheet de fuentes. Ver `fuentes_clean()`." = id_fuente_clean %in% df_fuentes$id_fuente_clean )
  
 if (is.null(fecha)) {
   fecha <- Sys.Date()
 } else {
   fecha <- as.Date(as.character(fecha))
   stopifnot("'fecha' debe ser null o fecha o string parseable a fecha" = !is.na(fecha))
 }
  
  inputs <- list(
    "id_fuente_clean" = id_fuente_clean,
    "fecha" = fecha 
    )
  
  df_fuentes <- df_fuentes[df_fuentes$id_fuente_clean == id_fuente_clean,]

  df_fuentes$fecha <-  inputs$fecha
  
  
  if (!file.exists(paste0("data/_FUENTES/clean/", df_fuentes$path_clean))) {
    stop("No se encontro el archivo clean en data/_FUENTES/clean/\nGuardarlo en la ubicacion antes de continuar")
  }
  
  
  print( df_fuentes[df_fuentes$id_fuente == inputs$id_fuente ,])
  
  
  df_fuentes |>
    googlesheets4::range_write(data = _,
                               ss = fuentes_raw_sheet_id(),
                               range = sprintf("A%d:F%d", id_fuente_clean))
  
  
  googledrive::drive_upload(media = paste0("data/_FUENTES/clean/", df_fuentes$path_clean),
                            path = googledrive::as_id(fuentes_clean_dir()$id),
                            name = df_fuentes$path_clean, overwrite = T)
  
}