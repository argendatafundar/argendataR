#' Comparar un data.frame con un output cargado en el drive de Argendata como csv
#'
#' @param df data.frame dataframe a comparar con el output cargado en el drive
#' @param nombre  string nombre del output en el drive
#' @param subtopico string subtopico al que pertenece el output
#' @param entrega_subtopico string nombre de la carpeta de entrega donde buscar el output 
#' @param pk string Vector con los nombres de columnas que son primary key del dataframe
#' @param drop_output_drive logical Si es TRUE (defatult) el resultado incluye el output cargado en el drive usado para comparar.
#'
#' @return Devuelve una lista con los resultados de los chequeos realizados
#' @export
#'

comparar_outputs <- function(df, nombre, subtopico, entrega_subtopico = "primera_entrega", pk = NULL, drop_output_drive = T) {

  if(missing(subtopico)) {

    subtopico <- get("subtopico", envir = globalenv())
  }

  filetemp <- list.files(tempdir(), full.names = T)[grepl(sprintf("%s_%s_argdt", nombre, subtopico), list.files(tempdir()))]

  if (length(filetemp) == 1) {

    output_drive <- readr::read_csv(filetemp)

  } else {

  # dowload or read output
  subtopico_outputs_df <- subtopico_outputs(subtopico_nombre = subtopico, entrega_subtopico = entrega_subtopico)

  id_output <- subtopico_outputs_df$id[grepl(nombre,subtopico_outputs_df$name)]

  filetemp <- tempfile(pattern = sprintf("%s_%s_argdt", nombre, subtopico), fileext = ".csv")

  googledrive::drive_download(file = googledrive::as_id(id_output), path = filetemp)

  output_drive <-  readr::read_csv(filetemp)



  }


  # check columns names

  cols_previas <-  colnames(output_drive)

  cols_faltantes <- cols_previas[! cols_previas %in% colnames(df)]

  sprintf("Todas las columnas del output previo estan en el df?: %s", all(cols_previas %in% colnames(df)))



  if (length(cols_faltantes) != 0) {

    msg_cols_faltantes <- paste0("Columnas faltantes: \n",
                  paste(cols_faltantes, collapse = ", "))
    warning(msg_cols_faltantes)

  }

  cols_nuevas <- colnames(df)[! colnames(df) %in% cols_previas]

  if (length(cols_nuevas) != 0) {

    msg_cols_nuevas <- paste0("Columnas nuevas:\n",
                              paste(cols_nuevas, collapse = ", "))
    warning(msg_cols_nuevas)
      }

  stopifnot("las columnas 'pk' deben estar presenten en el output previo y el output nuevo" = pk %in% cols_previas & pk %in% colnames(df))

  # check datatypes

  clases_previas <- lapply(output_drive, class)

  clases_previas <- tidyr::as_tibble(clases_previas)

  clases_previas <- tidyr::pivot_longer(clases_previas, cols = dplyr::everything(), names_to = "columna", values_to = "clase_previa")

  clases_nuevas <- lapply(df, class)

  clases_nuevas <- tidyr::as_tibble(clases_nuevas)

  clases_nuevas <- tidyr::pivot_longer(clases_nuevas, cols = dplyr::everything(), names_to = "columna", values_to = "clase_nueva")

  df_clases <- dplyr::left_join(clases_previas, clases_nuevas, by = "columna")

  df_clases <- df_clases[!is.na(df_clases$clase_nueva),]

  df_clases$coinciden <-  df_clases$clase_previa == df_clases$clase_nueva

  sprintf("Hay mismatch de clases de datos?: %s", any(!df_clases$coinciden))

  if(any(!df_clases$coinciden)) {

    comparacion_clases <- paste0("Mismatch de clases:\n",
                                 paste(utils::capture.output(print(df_clases[! df_clases$coinciden,])),
                                       collapse = "\n")
                                 )

    warning(comparacion_clases)


  }

  # check nrows

  sprintf("Filas en output previo: %d\nFilas en output nuevo: %d", nrow(output_drive), nrow(df))


  # similarity

  cols_comparacion <- cols_previas[cols_previas %in% colnames(df) & !cols_previas %in% pk]

  print(cols_comparacion)

  joined_df <- dplyr::left_join(output_drive, df, by = pk)

  checkeo_cols_values <-  purrr::map(cols_comparacion,
                                     .f = function(x) {comparar_valores(x, pk = pk,
                                                                        df = joined_df)})

  names(checkeo_cols_values) <- cols_comparacion

  resultado <- list("cols_faltantes" = cols_faltantes,
                    "cols_nuevas" = cols_nuevas,
                    "comparacion_clases" = df_clases,
                    "diferencia_nfilas" =  nrow(df) -nrow(output_drive),
                    "comparacion_cols" = checkeo_cols_values)

  if (!drop_output_drive) {
    resultado$output_drive <- output_drive
  }

  resultado

}



comparar_valores <- function(x, pk, df) {

  col_x <- paste0(x,".x")
  col_y <- paste0(x,".y")

  nuevos_na <- sum(!is.na(df[[col_x]]) & is.na(df[[col_y]]))

  class_x <- class(df[[paste0(x, ".x")]])

  if (class_x %in% c("numeric", "complex")) {

    diferencias_abs <- abs(df[[col_y]] - df[[col_x]])

    variaciones_rel <- round(diferencias_abs/abs(df[[col_x]]), 6)


    df_col <- dplyr::bind_cols(df[,pk],
                               tidyr::tibble(diferencias_abs,
                                variaciones_rel))


    list("nuevos_na" = nuevos_na,
         "mean_variaciones_rel" = mean(variaciones_rel, na.rm = T),
         "metricas_filas" = df_col
                  )


  } else if (class_x %in% c("character", "logical")) {

    coinciden_valores <- tidyr::replace_na(df[[col_y]] == df[[col_x]], F) | (is.na(df[[col_y]]) & is.na(df[[col_x]]))

    df_col <- dplyr::bind_cols(df[,pk],
                               tidyr::tibble(
                               coinciden_valores))

    list("nuevos_na" = nuevos_na,
         "suma_mismatches" = sum(coinciden_valores),
         "metricas_filas" = df_col)

  } else {

    "Comparacion no definida para la clase"

  }



}
