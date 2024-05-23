#' Comparar un data.frame con un output cargado en el drive de Argendata como csv
#'
#' @param df data.frame dataframe a comparar con el output cargado en el drive
#' @param nombre  string nombre del output en el drive
#' @param subtopico string subtopico al que pertenece el output. Si no esta definido el parametro busca en el ambiente global de la sesion un objeto llamado "subtopico"
#' @param entrega_subtopico string nombre de la carpeta de entrega donde buscar el output
#' @param pk string Vector con los nombres de columnas que son primary key del dataframe
#' @param drop_output_drive logical Si es FALSE (default) el resultado incluye el output cargado en el drive usado para comparar. Si es TRUE no lo incluye.
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

comparar_outputs <- function(df, nombre, subtopico, entrega_subtopico = "primera_entrega", pk = NULL, drop_output_drive = T) {

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
                                     .f = function(x) {comparar_valores(x, pk = pk,
                                                                        df = joined_df)})

  names(checkeo_cols_values) <- cols_comparacion

  resultado <- list(columns_check,
                    "comparacion_clases" = df_clases,
                    "diferencia_nfilas" =  nrow(df) - nrow(output_drive),
                    "comparacion_cols" = checkeo_cols_values)

  if (!drop_output_drive) {
    resultado$output_drive <- output_drive
  }

  resultado

}


control_valores_num <- function(x, pk, df) {
  
  
  col_x <- paste0(x,".x")
  col_y <- paste0(x,".y")
  
  nuevos_na <- sum(!is.na(df[[col_x]]) & is.na(df[[col_y]]))
  
  class_x <- class(df[[paste0(x, ".x")]])
  
  stopifnot("la variable no es numeric" = class_x %in% c("numeric", "complex"))
    
  diferencias_abs <- abs(df[[col_y]] - df[[col_x]])
  
  variaciones_rel <- round(diferencias_abs/abs(df[[col_x]]), 6)
  
  
  df_col <- dplyr::bind_cols(df[,pk],
                             tidyr::tibble(diferencias_abs,
                                           variaciones_rel))
  
  
  list("nuevos_na" = nuevos_na,
       "mean_variaciones_rel" = mean(variaciones_rel, na.rm = T),
       "metricas_filas" = df_col
  )
    

  
  
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
         "suma_mismatches" = sum(!coinciden_valores),
         "metricas_filas" = df_col)

  } else {

    "Comparacion no definida para la clase"

  }



}


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
