#' Actualizar informacion de una fuente raw
#'
#' @description
#' Actualiza 'fecha_descarga' y 'fecha_actualizar' de una fuente en la sheet de fuentes en el drive de Argendata. Hace `drive_upload()` con overwrite = T pisando la version anterior de la fuente en el drive.
#'
#' @details
#' La fecha será actualizada usando `Sys.time()` al momento de su ejecución.
#'
#' @param id_fuente integer id numerico que permite seleccionar la fuente segun aparece en el sheet. Para consultar ids usar  `fuentes_raw()`
#' @param url string Link directo a la fuente si existiera o link a la página web más inmediata a la  fuente.
#' @param nombre string Nombre único que identifica a la fuente
#' @param institucion string Nombre oficial de la institucion
#' @param actualizable logical TRUE o FALSE  sobre si la fuente será actualizada y debe volver a ser descargada en nueva versión en el futuro.
#' @param fecha_descarga date o string o null Fecha de descarga como valor de clase 'date', o 'string' parseable por `as.Date()`. Si es null toma la fecha de `Sys.Date()`
#' @param fecha_actualizar date o string o null Fecha de descarga como valor de clase 'date', o 'string' parseable por `as.Date()`. Si es null toma fecha actual más 6 meses
#' @param path_raw string Nombre del archivo de la fuente tal cual fue guardado.
#' @param script string  Nombre del archivo del script de descarga de la fuente tal cual se guardó en scripts/descarga_fuentes/ de argendata-etl
#' @param api logical TRUE o FALSE indicando si la fuente es una api o no.
#' @param directorio string Ruta al directorio desde el cual cargar el archivo
#' @param prompt logical Si es TRUE (default) pide confirmacion de los cambios.
#'
#' @export
#'
#'

actualizar_fuente_raw <- function(id_fuente,
                                  url = NULL,
                                  nombre = NULL,
                                  institucion = NULL,
                                  actualizable = NULL,
                                  fecha_descarga = NULL,
                                  fecha_actualizar = NULL,
                                  path_raw = NULL,
                                  script = NULL,
                                  api = NULL,
                                  directorio = NULL,
                                  prompt = TRUE) {

  limpiar_temps()

  if (is.null(directorio)) {
    directorio <- tempdir()
  } else {
    stopifnot("'directorio' debe ser string a una ruta valida" = dir.exists(directorio))
  }


  stopifnot("'id_fuente' debe ser id numerico de fuente o character con codigo de fuente" = is.numeric(id_fuente) | is.character(id_fuente))

  df_fuentes <- fuentes_raw(limpiar_cache  = T)

  if (is.numeric(id_fuente)) {

    stopifnot("'id_fuente' no encontrado en sheet de fuentes. Ver `fuentes_raw()`." = id_fuente %in% df_fuentes$id_fuente )

  } else if (is.character(id_fuente)) {

    stopifnot("'id_fuente' no coincide con ningun codigo en sheet de fuentes. Ver `fuentes_raw()`." = id_fuente %in% df_fuentes$codigo )
    id_fuente <- regmatches(id_fuente, m = regexpr("(?<=R)(\\d+)", text = id_fuente, perl = T))

    id_fuente <- as.numeric(id_fuente)
  }



  fecha_descarga <- Sys.time()

  if (is.character(fecha_actualizar) | class(fecha_actualizar) %in% c("Date", "POSIXct", "POSIXt")) {

    fecha_actualizar <- as.Date(fecha_actualizar)
    stopifnot("param 'fecha_actualizar' debe ser date o string parseable como fecha o null" = !is.na(fecha_actualizar))

  } else if (is.null(fecha_actualizar)) {

    fecha_actualizar <- "s/d"

  } else {

    stop("param 'fecha_actualizar' debe ser fecha o null")

    }




  inputs <- list(
    # "id_fuente" = id_fuente,
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

  inputs <- inputs[sapply(inputs, function(x) !is.null(x))]

  if (!isFALSE(prompt) & length(inputs) > 3) {

    message("Va a sobreescribir datos de registro de la fuente.")
    ok <- readline(prompt = "Continuar con la actualizacion de la fuente raw? Y/N")

    stopifnot("Actualizacion cancelado." = tolower(ok) == "y")

  }


  df_fuentes <- df_fuentes[df_fuentes$id_fuente == id_fuente,]
  


  for (i in names(inputs)) {
    print(i)
    print( df_fuentes[[which(df_fuentes$id_fuente == id_fuente), i]])
    
    inputs[[i]] <- coerce_to(inputs[[i]],
                             df_fuentes[[which(df_fuentes$id_fuente == id_fuente), i]])
    
    df_fuentes[[which(df_fuentes$id_fuente == id_fuente), i]] <- inputs[[i]]

  }
  
  # control path raw
  
  if (!file.exists(normalize_path(glue::glue("{directorio}/{df_fuentes$path_raw}")))) {
    warning("No existe el archivo fuente en la ruta especificada")
    warning(normalize_path(glue::glue("{directorio}/{df_fuentes$path_raw}")))
    stop()
    
  }


  print(df_fuentes)

  googledrive::drive_upload(media = normalize_path(glue::glue("{directorio}/{df_fuentes$path_raw}")),
                            path = googledrive::as_id(fuentes_raw_dir()$id),
                            name = df_fuentes$path_raw, overwrite = T)


  df_fuentes %>%
    googlesheets4::range_write(col_names = F,
                               ss = fuentes_raw_sheet_id(),
                               range = sprintf("A%d:L%d", id_fuente + 1, id_fuente + 1))


}



