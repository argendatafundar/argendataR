#' armador_descripcion_alt
#'
#' @param metadatos data.frame sus columnas son variable_nombre y descripcion. Ver `argendataR::metadata()`
#' @param etiquetas_nuevas data.frame Dataframe con la columna variable_nombre y la descripcion
#' @param output_cols string tiene las columnas del dataset que se quiere escribir
#'
#' @export
#'
#'

armador_descripcion_alt <- function(metadatos, etiquetas_nuevas = data.frame(), output_cols){

  etiquetas <- metadatos %>%
    dplyr::filter(.data$variable_nombre %in% output_cols)


  etiquetas <- etiquetas %>%
    dplyr::bind_rows(etiquetas_nuevas)


  diff <- dplyr::setdiff(output_cols, etiquetas$variable_nombre)

  stopifnot(`Error: algunas columnas de tu output no fueron descriptas` = length(diff) == 0)

  # En caso de que haya alguna variable que le haya cambiado la descripcion pero que
  # ya existia se va a quedar con la descripcion nueva.

  etiquetas <- etiquetas %>%
    dplyr::group_by(.data$variable_nombre) %>%
    dplyr::filter(if(dplyr::n() == 1) dplyr::row_number() == 1 else dplyr::row_number() == dplyr::n()) %>%
    dplyr::ungroup()

  etiquetas <- stats::setNames(as.list(etiquetas$descripcion), etiquetas$variable_nombre)

  return(etiquetas)

}
