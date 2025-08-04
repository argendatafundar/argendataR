#' armador_descripcion
#'
#' @param metadatos data.frame sus columnas son variable_nombre y descripcion. Ver `argendataR::metadata()`
#' @param etiquetas_nuevas data.frame Dataframe con la columna variable_nombre y la descripcion
#' @param output_cols string tiene las columnas del dataset que se quiere escribir
#'
#' @export
#'
#'

armador_descripcion <- function(metadatos, etiquetas_nuevas = data.frame(), output_cols){

  etiquetas <- metadatos %>%
    dplyr::filter(variable_nombre %in% output_cols)


  etiquetas <- etiquetas %>%
    bind_rows(etiquetas_nuevas)


  diff <- setdiff(output_cols, etiquetas$variable_nombre)

  stopifnot(`Error: algunas columnas de tu output no fueron descriptas` = length(diff) == 0)

  # En caso de que haya alguna variable que le haya cambiado la descripcion pero que
  # ya existia se va a quedar con la descripcion nueva.

  etiquetas <- etiquetas %>%
    group_by(variable_nombre) %>%
    filter(if(n() == 1) row_number() == 1 else row_number() == n()) %>%
    ungroup()

  etiquetas <- stats::setNames(as.list(etiquetas$descripcion), etiquetas$variable_nombre)

  return(etiquetas)

}
