#' Comparar un data.frame con un output cargado en el drive de Argendata como csv
#'
#' @param df data.frame dataframe a comparar con el output cargado en el drive
#' @param nombre  string nombre del output en el drive
#' @param subtopico string subtopico al que pertenece el output. Si no esta definido el parametro busca en el ambiente global de la sesion un objeto llamado "subtopico"
#' @param entrega_subtopico string nombre de la carpeta de entrega donde buscar el output
#' @param pk string Vector con los nombres de columnas que son primary key del dataframe
#' @param drop_output_drive logical Si es FALSE (default) el resultado incluye el left_join de el output cargado en el drive con el df. Si es TRUE no lo incluye.
#' @description
#' El resultado es una lista que contiene cuales son las columnas faltantes (cols_faltantes), cuales son las columnas nuevas (cols_nuevas),
#' un dataframe que compara las clases de columnas viejas y columnas nuevas (comparacion_clases),
#' la cantidad de filas de diferencia entre el nuevo dataset y el viejo (diferencia_nfilas), y una lista con metricas de comparacion por columna (comparacion_cols).
#' La lista de comparacion_cols presenta por cada columna la cantidad de nuevos NA (nuevos_na), la cantidad de mismatches para la columna en funcion de los PK (suma_mismatches) y
#' ademas un dataframe (metricas_filas) que indica para la columna analizada para cada tupla de valores PK las diferencias absolutas (abs(Xi/Xi-1)) y diferencias relativas (abs(Xi/Xi-1)/abs(Xi-1))
#'
#' @return Devuelve una lista con los resultados de los chequeos realizados
#' @export
#'

comparar_outputs <- function(df, nombre, subtopico,
                             entrega_subtopico = "primera_entrega", pk = NULL,
                             drop_output_drive = F) {

  if (missing(subtopico)) {

    subtopico <- get("subtopico", envir = globalenv())
  }

  output_drive <- descargar_output(nombre = nombre,
                                   subtopico = subtopico,
                                   entrega_subtopico = entrega_subtopico)


  columns_check <- check_cols(df = df, output_drive = output_drive)


  stopifnot("las columnas 'pk' deben estar presenten en el output previo y el output nuevo" = pk %in% columns_check$cols_previas & pk %in% colnames(df))


  df_clases <- check_datatypes(df = df, output_drive = output_drive)

  # check nrows

  sprintf("Filas en output previo: %d\nFilas en output nuevo: %d", nrow(output_drive), nrow(df))


  # similarity

  cols_comparacion <- columns_check$cols_previas[columns_check$cols_previas %in% colnames(df) & !columns_check$cols_previas %in% pk]

  print(cols_comparacion)

  joined_df <- dplyr::left_join(output_drive, df, by = pk)

  checkeo_cols_values <-  purrr::map(cols_comparacion,
                                     .f = function(x,
                                                   data = joined_df,
                                                   pk = pk) {


                                       class_x <- class(data[[paste0(x, ".x")]])

                                       if (class_x %in% c("numeric", "complex")) {

                                         control_valores_num(root_name = x, primarykeys = pk, df = data)

                                       } else if (class_x %in% c("character", "logical", "factor")) {

                                         control_valores_nonnum(root_name = x, primarykeys = pk, df = data)

                                       } else {

                                         print("Controles no implementados para la clase")
                                         "Controles no implementados para la clase"

                                       }

                                       })

  names(checkeo_cols_values) <- cols_comparacion

  resultado <- list(unlist(columns_check),
                    "comparacion_clases" = df_clases,
                    "diferencia_nfilas" =  nrow(df) - nrow(output_drive),
                    "comparacion_cols" = checkeo_cols_values)

  if (!drop_output_drive) {
    resultado$output_drive <- joined_df
  }

  resultado

}


#' Title
#'
#' @param x vector
#' @param y vector
#'
#' @return cantidad de nas nuevos
#' @export
#'
nuevos_na <- function(x,y) {

  n_nuevos_na <- sum(!is.na(x) & is.na(y))
  n_nuevos_na

}


#' Controles para variables numericas
#'
#' @param root_name raiz del nombre de la columna. Ej. para comparar "valores.x" y "valores.y" se usa "valores"
#' @param primarykeys primary keys de del dataframe
#' @param df dataframe
#'
#' @return lista con los resultados de los controles
#' @export
#'
#' @description
#' Compara dos columnas de un dataframe, en general que es producto de un join.
#' Las columnas a comparar deben estar nombradas al estilo "colA.x" y "colA.y"
#'
#' @details
#' Los valores devueltos son:
#' - nuevos_na: cantidad de nuevos na en la col.y respecto a la col.x
#' - mean_variaciones_rel: promedio de las variaciones relativas (col.y/col.x)-1
#' - ks_test: p valor del Kolmogorov-Smirnov Tests entre col.x y col.y
#' - mw_test: p valor del Mann-Whitney test entre col.x y col.y
#' qqplot_var  = qqplot(col.x, col.y)
#' - metricas_filas =  dataframe con pk, col.x, col.y, col_var
#'
#'

control_valores_num <- function(root_name, primarykeys, df) {


  col_x <- paste0(root_name,".x")
  col_y <- paste0(root_name,".y")


  class_x <- class(df[[col_x]])

  stopifnot("la variable no es numeric" = class_x %in% c("numeric", "complex"))

  variaciones_rel <- round((df[[col_y]] - df[[col_x]])/df[[col_x]], 6)


  df$variaciones_rel <- variaciones_rel

#
#   df_col <- dplyr::bind_cols(df[,c(pk, col_x, col_y)],
#                              tidyr::tibble(variaciones_rel))


  df_test <- df[!is.na(df[col_x]) & !is.na(df[col_x]),]

  ks_test <- ks.test(df_test[[col_x]], df_test[[col_y]],
                     simulate.p.value	= T, B = 1000)

  mw_test <- wilcox.test(df_test[[col_x]], df_test[[col_y]], paired = F)

  qqplot_var <- qqplot(df_test[[col_x]], df_test[[col_y]], plot.it = F)

  list("nuevos_na" = nuevos_na(df[[col_x]], df[[col_y]]),
       "mean_variaciones_rel" = mean(variaciones_rel, na.rm = T),
       "ks_test" = ks_test$p.value,
       "mw_test" = mw_test$p.value,
       "qqplot_var" = qqplot_var#,
       # "metricas_filas" = dplyr::select(df, dplyr::all_of(c(primarykeys, col_x, col_y)), "variaciones_rel")
  )


}


#' Controles para variables no numericas
#'
#' @param root_name raiz del nombre de la columna. Ej. para comparar "valores.x" y "valores.y" se usa "valores"
#' @param primarykeys primary keys de del dataframe
#' @param df dataframe
#'
#' @return lista con los resultados de los controles
#' @export
#'
#' @description
#' Compara dos columnas de un dataframe, en general que es producto de un join.
#' Las columnas a comparar deben estar nombradas al estilo "colA.x" y "colA.y"
#' @details
#' Los valores devueltos son:
#' - nuevos_na: cantidad de nuevos na en la col.y respecto a la col.x
#' - tasa_mismatches: proporcion de mismathces (filas que no coinciden entre col.x y col.y) sobre el total de filas
#' - metricas_filas =  dataframe con pk, col.x, col.y, coinciden_valores

control_valores_nonnum <- function(root_name, primarykeys, df) {

  col_x <- paste0(root_name,".x")
  col_y <- paste0(root_name,".y")

  nuevos_na <- sum(!is.na(df[[col_x]]) & is.na(df[[col_y]]))

  class_x <- class(df[[col_x]])

 stopifnot("la variable debe ser logical o character" = class_x %in% c("character", "logical", "factor"))

    coinciden_valores <- tidyr::replace_na(df[[col_y]] == df[[col_x]], F) | (is.na(df[[col_y]]) & is.na(df[[col_x]]))

    df$coinciden_valores <- coinciden_valores

    list("nuevos_na" = nuevos_na(df[[col_x]], df[[col_y]]),
         "tasa_mismatches" = sum(!coinciden_valores)/length(coinciden_valores)#,
         # "metricas_filas" = dplyr::select(df, dplyr::all_of(c(primarykeys, col_x, col_y)), "coinciden_valores")
         )


}


#' Control de nombres de outputs de argendata
#'
#' @param df dataframe nuevo de output
#' @param output_drive dataframe anterior para la comparacion
#'
#' @return lista de resultados
#' @export
#'

check_cols <- function(df, output_drive){
  # check columns names

  cols_previas <-  colnames(output_drive)

  cols_faltantes <- cols_previas[!cols_previas %in% colnames(df)]

  sprintf("Todas las columnas del output previo estan en el df?: %s", all(cols_previas %in% colnames(df)))



  if (length(cols_faltantes) != 0) {

    msg_cols_faltantes <- paste0("Columnas faltantes: \n",
                                 paste(cols_faltantes, collapse = ", "))
    warning(msg_cols_faltantes)

  }

  cols_nuevas <- colnames(df)[!colnames(df) %in% cols_previas]

  if (length(cols_nuevas) != 0) {

    msg_cols_nuevas <- paste0("Columnas nuevas:\n",
                              paste(cols_nuevas, collapse = ", "))
    warning(msg_cols_nuevas)
  }

  columns_check <- list("cols_previas" = cols_previas,
                        "cols_faltantes" = cols_faltantes)

  columns_check
}

#' Control de clases de outputs de argendata
#'
#' @param df dataframe output nuevo
#' @param output_drive dataframe output previo
#'
#' @return dataframe de control de clases
#' @export
#'

check_datatypes <- function(df, output_drive) {

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

  if (any(!df_clases$coinciden)) {

    comparacion_clases <- paste0("Mismatch de clases:\n",
                                 paste(utils::capture.output(print(df_clases[!df_clases$coinciden,])),
                                       collapse = "\n")
    )

    warning(comparacion_clases)


  }

  df_clases

}


