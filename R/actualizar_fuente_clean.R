#' Actualizar informacion de una fuente clean
#'
#' @description
#' Actualiza el registro en la tabla de fuentes clean y guarda una nueva copia de la fuente clean el directorio del servidor.
#' @details
#' La funcion toma el dato de path_clean declarado en `fuentes_clean()` para el id seleccionado y espera que exista el archivo correpondiente al path_clean. Ese archivo ser subirá al drive pisando el archivo preexistente.
#' La fecha será actualizada usando `Sys.time()` al momento de su ejecución.
#'
#' @param df data.frame Datafrane de la fuente clean a registrar.
#' @param id_fuente_clean integer id numerico  o codigo string que permite seleccionar la fuente segun aparece en el sheet. Para consultar ids usar  `fuentes_clean()`
#' @param nombre string Nombre único que identifica a la fuente en su versión 'clean'.
#' @param script string  Nombre del archivo del script de descarga de la fuente tal cual se guardó en scripts/limpieza_fuentes/ de argendata-etl
#' @param path_clean string Nombre del archivo de la fuente. Debe ser extension .parquet.
#' @param descripcion string Descripcion del dataset
#' @param directorio string Ruta al directorio desde el cual cargar el archivo. Si es NULL toma tempdir()
#' @param prompt logical Si es TRUE (default) pide confirmacion de los cambios.
#' @param comparacion list Lista con la salida de la funcion `comparar_fuente_clean`
#' @export
#'
#'

actualizar_fuente_clean <- function(id_fuente_clean,
                                    df = NULL,
                                    nombre = NULL,
                                    script = NULL,
                                    path_clean = NULL,
                                    descripcion = NULL,
                                    directorio = NULL,
                                    prompt = T,
                                    comparacion = NULL) {


  stopifnot("`comparacion` debe ser una lista resultado de `comparar_fuente_clean`" = !is.list(comparacion) & length(comparacion) > 1)

  stopifnot("'id_fuente_clean' debe ser id numerico de fuente o character con codigo de fuente" = is.numeric(id_fuente_clean) | is.character(id_fuente_clean))

  df_fuentes_clean <- fuentes_clean()

  df_fuentes_clean_md5 <- tools::md5sum(glue::glue("{RUTA_FUENTES()}/fuentes_clean.csv"))

  if (is.numeric(id_fuente_clean)) {

    stopifnot("'id_fuente_clean' no encontrado en sheet de fuentes. Ver `fuentes_clean()`." = id_fuente_clean %in% df_fuentes_clean$id_fuente_clean )

    irow <- which(df_fuentes_clean$id_fuente_clean == id_fuente_clean)

    stopifnot("Mas de una coincidencia de id_fuente hallada" = length(irow) == 1)


  } else if (is.character(id_fuente_clean)) {

    stopifnot("'id_fuente_clean' no coincide con ningun codigo en sheet de fuentes. Ver `fuentes_clean()`." = id_fuente_clean %in% df_fuentes_clean$codigo )

    irow <- which(df_fuentes_clean$codigo == id_fuente_clean)

    stopifnot("Mas de una coincidencia de id_fuente hallada" = length(irow) == 1)

  }

  if (!is.null(script)) {

    if (!file.exists(paste0("scripts/limpieza_fuentes/", script)) &
        !file.exists(script)) {
      stop("No se encontro el archivo script en scripts/limpieza_fuentes/. Guardarlo en la ubicacion antes de continuar")
    }

  }

  if (is.data.frame(df)) {

    message("El df sera guardado como parquet")


  } else if (!is.data.frame(df)) {

    if (is.null(directorio)) {
      directorio <- tempdir()
    } else {
      stopifnot("'directorio' debe ser string a una ruta valida" = dir.exists(directorio))
    }

    stopifnot("La extension de la fuente clean debe ser parquet" = grepl("\\.parquet$", path_clean))

    stopifnot("Directorio y path_clean no son ruta valida" = file.exists(normalize_path(paste(directorio, path_clean, sep = "/"))))


    if (file.size(glue::glue("{directorio}/{path_clean}")) > 1E8) {
      warning("El peso del archivo supera el limite de github ")
    }

  } else {
    stop("Debe ingresar un dataframe valido o un path_clean valido")
  }


  inputs <- list(
    # id_fuente_clean = id_fuente_clean,
    path_clean = path_clean,
    nombre = nombre,
    script = script,
    descripcion = descripcion,
    fecha = Sys.time()
  )


  inputs <- inputs[sapply(inputs, function(x) !is.null(x))]


  if (!isFALSE(prompt) & length(inputs) > 1) {

    message("Va a sobreescribir datos de registro de la fuente.")
    ok <- readline(prompt = "Continuar con la actualizacion de la fuente raw? Y/N")

    stopifnot("Actualizacion cancelada." = tolower(ok) == "y")

  }



  for (i in names(inputs)) {

    inputs[[i]] <- coerce_to(inputs[[i]], df_fuentes_clean[[irow, i]])

    df_fuentes_clean[[irow, i]] <-  inputs[[i]]

  }

  print(df_fuentes_clean[irow,])

  stopifnot("El registro de fuentes cambio antes de finalizar la actualizacion. Vuelva a intentarlo" = df_fuentes_clean_md5 == tools::md5sum(glue::glue("{RUTA_FUENTES()}/fuentes_clean.csv")))


  df_fuentes_clean %>%
    readr::write_csv(file = glue::glue("{RUTA_FUENTES()}/fuentes_clean.csv"), eol = "\n", progress = F)

  message("Registro actualizado en fuentes clean")

  if (is.data.frame(df)) {

    df %>%
      arrow::write_parquet(sink = glue::glue("{RUTA_FUENTES()}/clean/{df_fuentes_clean[irow,'path_clean']}"), compression = "snappy")

    message("Parquet creado")
    
    comparacion %>% 
      jsonlite::write_json(path = glue::glue("{RUTA_FUENTES()}/clean/log/log_{df_fuentes_clean$codigo[irow]}_{format(Sys.time(), '%Y%m%d%z%S')}.json"))

  } else if (!is.data.frame(df)) {
    
    msj <- glue::glue("Leyendo desde {directorio}/{df_fuentes_clean[irow,'path_clean']}")
    message(msj)


    if (file.size(glue::glue("{directorio}/{df_fuentes_clean[irow,'path_clean']}")) > 1E8) {
      warning("El peso del archivo supera el limite de github ")
    }

    file.copy(from = glue::glue("{directorio}/{df_fuentes_clean[irow,'path_clean']}"),
              to = glue::glue("{RUTA_FUENTES()}/clean/{df_fuentes_clean[irow,'path_clean']}"), overwrite = T)

    message("Parquet creado")

    comparacion %>% 
      jsonlite::write_json(path = glue::glue("{RUTA_FUENTES()}/clean/log/log_{df_fuentes_clean$codigo[irow]}_{format(Sys.time(), '%Y%m%d%z%S')}.json"))
    
  } else {
    stop("Error inesperado al guardar el parquet")
  }





}
