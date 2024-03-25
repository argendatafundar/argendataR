#' Agregar nueva fuente raw
#'
#' @description
#' Agrega una fuente no registrada previamente: genera una nueva entrada en la sheet de fuentes y hace `drive_upload()` con overwrite = F de la fuente.
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
#' 
#' @export
#'


agregar_fuente_raw <- function(
                           url = NULL,
                           nombre = NULL,
                           institucion = NULL,
                           actualizable = NULL,
                           fecha_descarga = NULL,
                           fecha_actualizar = NULL,
                           path_raw = NULL,
                           script = NULL,
                           api = FALSE) {



  inputs <- list(
    "url" = url ,
    "nombre" = nombre ,
    "institucion" = institucion,
    "actualizable" = actualizable ,
    "fecha_descarga" = fecha_descarga ,
    "fecha_actualizar" =  fecha_actualizar ,
    "path_raw" = path_raw,
    "script" = script,
    "api" = api
  )

  if (is.null(fecha_actualizar)) {
    fecha_actualizar <- Sys.Date()+months(6)
  }

  inputs$fecha_descarga <- as.Date(inputs$fecha_descarga)

  inputs$fecha_actualizar <- as.Date(inputs$fecha_actualizar)


  stopifnot("No se admiten parametros nulos" = !any(sapply(inputs, is.null)))

  stopifnot("No se admiten parametros con NAs" = !any(sapply(inputs, is.na)))

  stopifnot("No se admiten parametros con string vacios. Eg: ''" = !any(sapply(inputs, function(x) {as.character(x) == ''})))

  stopifnot("param 'actualizable' debe ser logico" = is.logical(inputs$actualizable))

  stopifnot("param 'fecha_descarga' debe ser fecha" = !is.na(inputs$fecha_descarga))

  stopifnot("param 'fecha_actualizar' debe ser fecha" = !is.na(inputs$fecha_actualizar))

  stopifnot("param 'url' debe ser una url valida" =  grepl("^(https|http)://",inputs$url))

 stopifnot("param 'api' debe ser T o F" = is.logical(api) & !is.na(api))

  df_fuentes <- fuentes_raw()

  if (nrow(df_fuentes[df_fuentes$nombre == inputs$nombre & df_fuentes$url == inputs$url & df_fuentes$institucion == inputs$institucion,]) != 0) {
    stop("Ya existe esa combinacion nombre, institucion y url. Verificar si es una posible duplicacion o cambiar de nombre, institucion o url")
  }
  
  if (!file.exists(paste0("data/_FUENTES/raw/", inputs$path_raw))) {
    stop("No se encontro el archivo raw en data/_FUENTES/raw. Guardarlo en la ubicacion antes de continuar")
  }
  
  if (!file.exists(paste0("scripts/descarga_fuentes/", inputs$script))) {
    stop("No se encontro el archivo script en scripts/descarga_fuentes/. Guardarlo en la ubicacion antes de continuar")
  }

  last_id <- dplyr::last(df_fuentes$id_fuente)

  if (is.na(last_id)) {
    next_id <- 1
  } else {
    next_id <- last_id+1
    
  }

  inputs$id_fuente <- next_id

  print(paste("La fuente quedara registrada con el id:", inputs$id_fuente))

  print(

    tibble::as_tibble(inputs)  %>% 
      dplyr::select(  "id_fuente" ,
                      "nombre",
                      "url",
                      "institucion",
                      "actualizable",
                      "fecha_descarga",
                      "fecha_actualizar",
                      "path_raw",
                      "script",
                      "api")
  )
  
  
  
  fuentes_raw_dir <- fuentes_raw_dir()
  
  
  if (path_raw %in% fuentes_raw_dir$tree$name) {
    print(df_fuentes[df_fuentes$path_raw == path_raw, ])
    stop("El archivo ya existe en el drive. Cambiar el nombre del archivo o borrar el archivo existente")
    
  }
  
  googledrive::drive_upload(media = paste0("data/_FUENTES/raw/", path_raw),
                            path = googledrive::as_id(fuentes_raw_dir$id),
                            name = path_raw)
  
  
  
  tibble::as_tibble(inputs)  %>% 
    dplyr::select(  "id_fuente" ,
                    "nombre",
                    "url",
                    "institucion",
                    "actualizable",
                    "fecha_descarga",
                    "fecha_actualizar",
                    "path_raw",
                    "script",
                    "api")  %>% 
    googlesheets4::sheet_append(
      ss = fuentes_raw_sheet_id())
  


 

}
