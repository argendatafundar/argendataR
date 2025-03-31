#' Descargar output
#'
#' @param nombre nombre del archivo a descargar. La función usa grepl para identificar el archivo, si el nombre coincide con más de un archivo devuelve error.
#' @param subtopico codigo de 6 letras del subtopico en mayusculas.
#' @param entrega_subtopico nombre exacto de la carpeta de entrega donde buscar el output
#' @param branch branch de github a donde apuntar. Default es "main"
#' @param ... parametros adicionales pasados a read_delim
#'
#' @description
#' La funcion descarga el archivo si está disponible desde GH y sino desde la carpeta del drive de argendata
#' en la carpeta temporal de la sesión y hace read_delim desde allí.
#'
#' @export
#'
#'

descargar_output <- function(nombre, subtopico, entrega_subtopico = NULL, branch = "main", ...) {

    limpiar_temps()

    stopifnot("subtopico debe ser character de largo 6" = is.character(subtopico) & nchar(subtopico) == 6)

    path <- gsub("\\.csv$|\\.json$","",nombre)

    path <- paste0(path, ".csv")

    df <- tryCatch(
      get_output_repo(path = path, subtopico = subtopico, branch = branch),
      error = function(cnd) NULL
    )

    if (is.null(df)) {
      warning("Output no encontrado en repo 'data'.")
      flush.console()
      stopifnot("'entrega_subtopico' debe ser character" = is.character(entrega_subtopico))
      # dowload or read output
      subtopico_outputs_df <- subtopico_outputs(subtopico_nombre = subtopico,
                                                entrega_subtopico = entrega_subtopico)

      id_output <- subtopico_outputs_df$id[grepl("cosasoa", subtopico_outputs_df$name)]

      stopifnot("Output no encontrada en el drive de Argendata" = length(id_output) != 0)

      stopifnot("Se encontro mas de una coincidencia en el drive de Argendata. Corregir filesystem del drive" = length(id_output) != 1)

      filetemp <- tempfile(pattern = sprintf("%s_%s_%s_argdt",
                                             nombre,
                                             entrega_subtopico,
                                             subtopico),
                           fileext = ".csv")

      googledrive::drive_download(file = googledrive::as_id(id_output),
                                  path = filetemp)

      df <-  readr::read_delim(filetemp, ...)


    }

    df

}


#' Descargar output desde el repo de data de argendata
#'
#' @param path path del archivo a descargar. Ej.: '01_evolucion_CO2_historico.json' o '01_evolucion_CO2_historico.csv'
#' @param subtopico codigo de 6 letras del subtopico en mayusculas. Ej.: "CAMCLI"
#' @param branch branch de github a donde apuntar. Default es "main"
#'
#' @description
#' La funcion descarga el archivo si está disponible desde GH y sino desde la carpeta del drive de argendata
#' en la carpeta temporal de la sesión y hace read_delim desde allí.
#'
#' @export
#'
#'

get_output_repo <- function(path, subtopico, branch = "main") {

  assertthat::is.string(path)
  assertthat::is.string(branch)

  stopifnot("subtopico debe ser character de largo 6" = is.character(subtopico) & nchar(subtopico) == 6)

  subtopico <- toupper(subtopico)

  output_url <- glue::glue("{GH_DATA_RAWURL()}/{branch}/{subtopico}/{path}")

  message("Descargando output desde: ", output_url)
  flush.console()

  rpta <- tryCatch(
    httr2::req_perform(httr2::request(output_url)),
    httr2_http_404 = function(cnd) NULL
  )

  assertthat::assert_that(!is.null(rpta))


  if (grepl("\\.json$", path)) {

    df <- jsonlite::fromJSON(rawToChar(rpta$body))

  } else if (grepl("\\.csv$", path)) {


    df <- readr::read_csv(rawToChar(rpta$body))

  } else {

    stop("formato de archivo no soportado")

  }


  df

}

