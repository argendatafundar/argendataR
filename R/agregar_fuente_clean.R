#' Agregar nueva fuente clean
#'
#' @description
#' Agrega una fuente no registrada previamente: genera una nueva entrada en la sheet de fuentes y hace `drive_upload()` con overwrite = F de la fuente.
#'
#'
#' @param id_fuente_raw integer id numerico que permite seleccionar la fuente raw segun aparece en el sheet. Para consultar ids usar  `fuentes_raw()`
#' @param nombre string Nombre único que identifica a la fuente en su versión 'clean'.
#' @param script string  Nombre del archivo del script de descarga de la fuente tal cual se guardó en scripts/limpieza_fuentes/ de argendata-etl
#' @param path_clean string Nombre del archivo de la fuente tal cual fue guardado en el directorio data/_FUENTES/clean/ de argendata-etl
#'
#' @details
#' Más de una fuente clean puede referir a una misma fuente raw. Por ejemplo, si la fuente raw consiste en un excel de multiples hojas, cada hoja debería pasar a ser un csv independiente.
#' 
#'
#' @export
#'


agregar_fuente_clean <- function(id_fuente_raw = NULL,
                               path_clean = NULL,
                               nombre = NULL,
                               script = NULL) {
  
  
  
  inputs <- list(
    id_fuente_raw = id_fuente_raw,
    path_clean = path_clean,
    nombre = nombre,
    script = script,
    fecha = Sys.Date()
  )
  
  
  stopifnot("No se admiten parametros nulos" = !any(sapply(inputs, is.null)))
  
  stopifnot("No se admiten parametros con NAs" = !any(sapply(inputs, is.na)))
  
  stopifnot("No se admiten parametros con string vacios. Eg: ''" = !any(sapply(inputs, function(x) {as.character(x) == ''})))
  
  df_fuentes_raw <- fuentes_raw()
  
  stopifnot("El id_fuente_raw no existe en la sheet de fuentes raw. Verificar si es un id valido en  `fuentes_raw()`" = id_fuente_raw %in% df_fuentes_raw$id_fuente)
  
  df_fuentes <- fuentes_clean()
  
  if (nrow(df_fuentes[df_fuentes$nombre == inputs$nombre & df_fuentes$id_fuente_raw == inputs$id_fuente_raw,]) != 0) {
    
    print(df_fuentes[df_fuentes$nombre == inputs$nombre & df_fuentes$id_fuente_raw == inputs$id_fuente_raw,])
    stop("Ya existe esa combinacion nombre y id_fuente_raw. Verificar si es una posible duplicacion o cambiar de nombre")
  }
  
  if (!file.exists(paste0("data/_FUENTES/clean/", inputs$path_clean))) {
    stop("No se encontro el archivo clean en data/_FUENTES/clean Guardarlo en la ubicacion antes de continuar")
  }
  
  if (!file.exists(paste0("scripts/limpieza_fuentes/", inputs$script))) {
    stop("No se encontro el archivo script en scripts/limpieza_fuentes/. Guardarlo en la ubicacion antes de continuar")
  }
  
  last_id <- dplyr::last(df_fuentes$id_fuente_clean)
  
  if (is.na(last_id)) {
    next_id <- 1
  } else {
    next_id <- last_id+1
    
  }
  
  inputs$id_fuente_clean <- next_id
  
  print(paste("La fuente quedara registrada con el id:", inputs$id_fuente_clean))
  
  print(
    
    tibble::as_tibble(inputs)  %>% 
      dplyr::select(  "id_fuente_clean" ,
                      "id_fuente_raw",
                      "path_clean",
                      "nombre",
                      "script",
                      "fecha")
  )
  
  
  
  fuentes_clean_dir <- fuentes_clean_dir()
  
  
  if (path_clean %in% fuentes_clean_dir$tree$name) {
    print(df_fuentes[df_fuentes$path_clean == path_clean, ])
    stop("El archivo ya existe en el drive. Cambiar el nombre del archivo o borrar el archivo existente")
    
  }
  
  googledrive::drive_upload(media = paste0("data/_FUENTES/clean/", path_clean),
                            path = googledrive::as_id(fuentes_clean_dir$id),
                            name = path_clean)
  
  
  
  tibble::as_tibble(inputs)  %>% 
    dplyr::select(  "id_fuente_clean" ,
                    "id_fuente_raw",
                    "path_clean",
                    "nombre",
                    "script",
                    "fecha")  %>% 
    googlesheets4::sheet_append(ss = fuentes_clean_sheet_id())
  
}
