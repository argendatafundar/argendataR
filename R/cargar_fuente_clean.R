#' Agregar o actualizar una fuente clean
#'
#' @description
#' Si `actualizar == FALSE` (default) Agrega una fuente no registrada previamente: genera una nueva entrada en la sheet de fuentes y hace `googledrive::drive_upload()` con overwrite = F de la fuente.
#' Si `actualizar == TRUE` Actualiza 'fecha_descarga' y 'fecha_actualizar' de una fuente en la sheet de fuentes en el drive de Argendata. Hace `googledrive::drive_upload()` con overwrite = T pisando la version anterior de la fuente en el drive.
#'
#'
#' @param id_fuente_raw integer id numerico que permite seleccionar la fuente raw segun aparece en el sheet. Para consultar ids usar  [fuentes_raw()]
#' @param nombre string Nombre único que identifica a la fuente en su versión 'clean'.
#' @param script string  Nombre del archivo del script de descarga de la fuente tal cual se guardó en scripts/limpieza_fuentes/ de argendata-etl
#' @param path_clean string Nombre del archivo de la fuente tal cual fue guardado en el directorio data/_FUENTES/clean/ de argendata-etl
#' @param actualizar logical Si es TRUE registra la fuente como una nueva mediante [agregar_fuente_clean()]. Si es FALSE actuliza la  fuente  ya cargada con [actualizar_fuente_raw()].
#'
#' @export
#'

cargar_fuente_clean <- function(id_fuente_raw = id_fuente_raw,
                              path_clean = path_clean,
                              nombre = nombre,
                              script = script) {
  
  
  
  if (actualizar == F) {
    
    agregar_fuente_clean(id_fuente_raw = id_fuente_raw, path_clean = path_clean,
                         nombre = nombre, script = script)
    
  } else if(actualizar == T) {
    df_fuentes <- fuentes_raw()
    
    id_fuente <- df_fuentes[df_fuentes$url == url & 
                              df_fuentes$nombre == nombre &
                              df_fuentes$path_raw == path_raw &
                              df_fuentes$script == script,][["id_fuente"]]
    
    
    actualizar_fuente_clean(id_fuente = id_fuente, fecha_actualizar = fecha_actualizar)
    
  } else {
    stop("param 'actualizar' debe ser TRUE o FALSE.")
  }
  
}
