#' Agregar o actualizar una fuente raw
#'
#' @description
#' Si `actualizar == FALSE` (default) Agrega una fuente no registrada previamente: genera una nueva entrada en la sheet de fuentes y hace `googledrive::drive_upload()` con overwrite = F de la fuente.
#' Si `actualizar == TRUE` Actualiza 'fecha_descarga' y 'fecha_actualizar' de una fuente en la sheet de fuentes en el drive de Argendata. Hace `googledrive::drive_upload()` con overwrite = T pisando la version anterior de la fuente en el drive.
#'
#'
#' @param url string Link directo a la fuente si existiera o link a la página web más inmediata a la  fuente.
#' @param nombre string Nombre único que identifica a la fuente
#' @param institucion string Nombre oficial de la institucion
#' @param actualizable logical TRUE o FALSE  sobre si la fuente será actualizada y debe volver a ser descargada en nueva versión en el futuro.
#' @param fecha_descarga date o string o null Fecha de descarga como valor de clase 'date', o 'string' parseable por `as.Date()`. Si es null toma la fecha de `Sys.Date()`
#' @param fecha_actualizar date o string o null Fecha de descarga como valor de clase 'date', o 'string' parseable por `as.Date()`. Si es null toma fecha actual más 6 meses
#' @param path_raw string Nombre del archivo de la fuente tal cual fue descargado en el directorio data/_FUENTES/raw/ de argendata-etl
#' @param script string  Nombre del archivo del script de descarga de la fuente tal cual se guardó en scripts/descarga_fuentes/ de argendata-etl
#' @param api logical TRUE o FALSE indicando si la fuente es una api o no.
#' @param actualizar logical Si es TRUE registra la fuente como una nueva mediante [agregar_fuente_raw()]. Si es FALSE actuliza la  fuente  ya cargada con [actualizar_fuente_raw()]
#'
#' @export
#'

cargar_fuente_raw <- function(url = NULL,
                              nombre = NULL,
                              institucion = NULL,
                              actualizable = NULL,
                              fecha_descarga = NULL,
                              fecha_actualizar = NULL,
                              path_raw = NULL,
                              script = NULL,
                              api = FALSE,
                              actualizar = FALSE) {
  
  
  
  if (actualizar == F) {
    
    agregar_fuente_raw(
      url = url,
      nombre = nombre,
      institucion = institucion,
      actualizable = actualizable,
      fecha_descarga = fecha_descarga,
      fecha_actualizar = fecha_actualizar,
      path_raw = path_raw,
      script = script,
      api = api
    )
    
  } else if(actualizar == T) {
    df_fuentes <- fuentes_raw()
    
    id_fuente <- df_fuentes[df_fuentes$url == url & 
                              df_fuentes$nombre == nombre &
                              df_fuentes$path_raw == path_raw &
                              df_fuentes$script == script,][["id_fuente"]]
    
    
    actualizar_fuente_raw(id_fuente = id_fuente, fecha_actualizar = fecha_actualizar)
    
  } else {
    stop("param 'actualizar' debe ser TRUE o FALSE.")
  }
  
}

