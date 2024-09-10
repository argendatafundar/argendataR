#' Agregar nueva fuente raw
#'
#' @description
#' Agrega una fuente no registrada previamente: genera una nueva entrada en la sheet de fuentes y hace `drive_upload()` con overwrite = F de la fuente.
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
#' @param directorio string Ruta al directorio desde el cual cargar el archivp
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
                           api = FALSE,
                           directorio = NULL) {




  if (is.character(fecha_actualizar)) {

    fecha_actualizar <- as.Date(fecha_actualizar)
    stopifnot("param 'fecha_actualizar' debe ser fecha valida o string parseable como fecha o null" = !is.na(fecha_actualizar) & length(fecha_actualizar) != 0)

  } else if (class(fecha_actualizar) %in% c("Date", "POSIXct", "POSIXt")) {

    stopifnot("param 'fecha_actualizar' debe ser fecha valida o string parseable como fecha o null" = !is.na(fecha_actualizar) & length(fecha_actualizar) != 0)

  } else if (is.null(fecha_actualizar)) {

    fecha_actualizar <- Sys.Date() + months(1, abbreviate = F)


  } else {

    stop("param 'fecha_actualizar' debe ser fecha valida o string parseable como fecha o null")
  }



  if (is.character(fecha_descarga)  | class(fecha_descarga) %in% c("Date", "POSIXct", "POSIXt")) {

    fecha_descarga <- as.Date(fecha_descarga)
    stopifnot("param 'fecha_descarga' debe ser date o string parseable como fecha o null" = !is.na(fecha_descarga) & length(fecha_descarga) != 0)

  } else if (!is.null(fecha_descarga)) {

    stop("param 'fecha_descarga' debe ser fecha o null")

    }




  inputs <- list(
    "url" = url ,
    "nombre" = nombre ,
    "institucion" = institucion,
    "actualizable" = actualizable ,
    "fecha_descarga" = as.Date(fecha_descarga),
    "fecha_actualizar" =  fecha_actualizar ,
    "path_raw" = path_raw,
    "script" = script,
    "api" = api
  )



  stopifnot("No se admiten parametros nulos" = !any(sapply(inputs[!names(inputs) %in% c("fecha_actualizar")], is.null)))

  stopifnot("No se admiten parametros con NAs" = !any(sapply(inputs[!names(inputs) %in% c("fecha_actualizar")], is.na)))

  stopifnot("No se admiten parametros con string vacios. Eg: ''" = !any(sapply(inputs[!names(inputs) %in% c("fecha_actualizar")], function(x) {as.character(x) == ''})))

  stopifnot("param 'actualizable' debe ser logico" = is.logical(inputs$actualizable))

  stopifnot("param 'fecha_descarga' debe ser fecha" = !is.na(inputs$fecha_descarga))

  stopifnot("param 'fecha_actualizar' debe ser fecha" = !is.na(inputs$fecha_actualizar))

  # stopifnot("param 'url' debe ser una url valida" =  grepl("^(https|http)://",inputs$url))

  stopifnot("param 'api' debe ser T o F" = is.logical(api) & !is.na(api))

  df_fuentes_raw <- fuentes_raw()

  df_fuentes_raw_md5 <- tools::md5sum(glue::glue("{RUTA_FUENTES()}/fuentes_raw.csv"))


  if (nrow(df_fuentes_raw[df_fuentes_raw$nombre == inputs$nombre & df_fuentes_raw$url == inputs$url & df_fuentes_raw$institucion == inputs$institucion,]) != 0) {
    stop("Ya existe esa combinacion nombre, institucion y url. Verificar si es una posible duplicacion o cambiar de nombre, institucion o url")
  }

  if (!file.exists(paste0("scripts/descarga_fuentes/", inputs$script)) &
      !file.exists(inputs$script)) {
    stop("No se encontro el archivo script en scripts/descarga_fuentes/. Guardarlo en la ubicacion antes de continuar")
  }

 if (is.null(directorio)) {
      directorio <- tempdir()
    } else {
      stopifnot("'directorio' debe ser string a una ruta valida" = dir.exists(directorio))
  } 


  last_id <- dplyr::last(df_fuentes_raw$id_fuente)

  if (is.na(last_id)) {
    next_id <- 1
  } else {

    next_id <- last_id + 1

  }

  inputs$id_fuente <- next_id

  inputs$codigo <- sprintf("R%dC0", inputs$id_fuente)

  print(paste("La fuente quedara registrada con el codigo:", inputs$codigo))

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
                      "api",
                      "codigo")
  )





  if (path_raw  %in% list.files(glue::glue("{RUTA_FUENTES()}/raw"))) {

    print(df_fuentes_raw[df_fuentes_raw$path_raw == path_raw, ])
    stop("El archivo ya existe en el drive. Cambiar el nombre del archivo o borrar el archivo existente")

  }

  stopifnot("El registro de fuentes cambio antes de finalizar la actualizacion. Vuelva a intentarlo" = df_fuentes_raw_md5 == tools::md5sum(glue::glue("{RUTA_FUENTES()}/fuentes_raw.csv")))



  if (file.exists(normalize_path(paste(directorio, inputs$path_raw, sep = "/")))) {



    file.copy(from = glue::glue("{directorio}/{inputs$path_raw}"),
              to = glue::glue("{RUTA_FUENTES()}/raw/{inputs$path_raw}"), overwrite = T, copy.mode = T)

    message("Fuente copiada a carpeta raw")


  } else {
    stop("Error inesperado al guardar el archivo")
  }



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
                    "api",
                    "codigo")  %>%
    readr::write_csv(file = glue::glue("{RUTA_FUENTES()}/fuentes_raw.csv"), eol = "\n", append = T)

  message("Registro agregado en fuentes raw")





}
