#' Actualizar informacion de una fuente clean
#'
#' @description
#' Hace `drive_upload()` con overwrite = T pisando la version anterior de la fuente en el drive y actualiza 'timestamp' de una fuente en la sheet de fuentes clean en el drive de Argendata.
#' @details
#' La funcion toma el dato de path_clean declarado en `fuentes_clean()` para el id seleccionado y espera que exista el archivo correpondiente al path_clean. Ese archivo ser subirá al drive pisando el archivo preexistente.
#' La fecha será actualizada usando `Sys.time()` al momento de su ejecución.
#'
#' @param id_fuente_clean integer id numerico que permite seleccionar la fuente segun aparece en el sheet. Para consultar ids usar  `fuentes_clean()`
#' @param nombre string Nombre único que identifica a la fuente en su versión 'clean'.
#' @param script string  Nombre del archivo del script de descarga de la fuente tal cual se guardó en scripts/limpieza_fuentes/ de argendata-etl
#' @param path_clean string Nombre del archivo de la fuente tal cual fue guardado.
#' @param descripcion string Descripcion del dataset
#' @param directorio string Ruta al directorio desde el cual cargar el archivo. Si es NULL toma tempdir()
#' @param prompt logical Si es TRUE (default) pide confirmacion de los cambios.
#' @export
#'
#'

actualizar_fuente_clean <- function(id_fuente_clean,
                                    nombre = NULL,
                                    script = NULL,
                                    path_clean = NULL,
                                    descripcion = NULL,
                                    directorio = NULL,
                                    prompt = T) {

  limpiar_temps()

  if (is.null(directorio)) {
    directorio <- tempdir()
  } else {
    stopifnot("'directorio' debe ser string a una ruta valida" = dir.exists(directorio))
  }

  stopifnot("'id_fuente_clean' debe ser id numerico de fuente o character con codigo de fuente" = is.numeric(id_fuente_clean) | is.character(id_fuente_clean))

  df_fuentes <- fuentes_clean(limpiar_cache  = T)

  if (is.numeric(id_fuente_clean)) {

    stopifnot("'id_fuente_clean' no encontrado en sheet de fuentes. Ver `fuentes_clean()`." = id_fuente_clean %in% df_fuentes$id_fuente_clean )

    df_fuentes <- df_fuentes[df_fuentes$id_fuente_clean == id_fuente_clean,]


  } else if (is.character(id_fuente_clean)) {

    stopifnot("'id_fuente_clean' no coincide con ningun codigo en sheet de fuentes. Ver `fuentes_clean()`." = id_fuente_clean %in% df_fuentes$codigo )

    df_fuentes <- df_fuentes[df_fuentes$codigo == id_fuente_clean,]

  }




  inputs <- list(
    id_fuente_clean = id_fuente_clean,
    path_clean = path_clean,
    nombre = nombre,
    script = script,
    fecha = Sys.time()
  )


  inputs <- inputs[sapply(inputs, function(x) !is.null(x))]

  if (!isFALSE(prompt) & length(inputs) > 3) {

    message("Va a sobreescribir datos de registro de la fuente.")
    ok <- readline(prompt = "Continuar con la actualizacion de la fuente raw? Y/N")

    stopifnot("Actualizacion cancelado." = tolower(ok) == "y")

  }


  df_fuentes <- df_fuentes[df_fuentes$id_fuente_clean  == id_fuente_clean,]

  for (i in names(inputs)) {

    df_fuentes[df_fuentes$id_fuente_clean  == inputs$id_fuente_clean, i] <-  inputs[[i]]

  }




  if (!file.exists(normalize_path(paste(directorio, df_fuentes$path_clean, sep = "/")))) {
    stop("No se encontro el archivo clean, guardarlo en la ubicacion antes de continuar")
  }


  print(df_fuentes)

  googledrive::drive_upload(media = normalize_path(glue::glue("{directorio}/{df_fuentes$path_clean}")),
                            path = googledrive::as_id(fuentes_clean_dir()$id),
                            name = df_fuentes$path_clean,
                            overwrite = T)


  df_fuentes %>%
    googlesheets4::range_write(col_names = F,
                               ss = fuentes_clean_sheet_id(),
                               range = sprintf("A%d:I%d", df_fuentes$id_fuente_clean+1, df_fuentes$id_fuente_clean+1))




}
