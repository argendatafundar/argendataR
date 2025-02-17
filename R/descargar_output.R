#' Descargar output desde el drive
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

    path <- gsub("\\.csv$","",nombre)

    path <- paste0(path, ".csv")

    output_url <- glue::glue("{GH_DATA_RAWURL()}/{branch}/{subtopico}/{path}")

    message("Descargando output desde: ", output_url)
    flush.console()
    df <- tryCatch(
      httr2::req_perform(httr2::request(output_url)),
      httr2_http_404 = function(cnd) NULL
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

      output_drive <-  readr::read_delim(filetemp, ...)

    } else {

      output_drive <- rvest::html_text(rvest::read_html(df$body)) %>%
        readr::read_csv()

    }




    output_drive

  }

