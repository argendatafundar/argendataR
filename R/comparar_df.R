#' Comparar dos dataframes
#'
#' @param df data.frame dataframe a comparar con el output cargado en el drive
#' @param df_anterior data.frame dataframe anterior que se usa como referencia de comparacion. Ver `argendataR::descargar_output()`.
#' @param pk string Vector con los nombres de columnas que son primary key del dataframe
#' @param k_control_num numeric cantidad de desvios estandar a partir del cual se seleccionan posibles outliers
#' @param drop_joined_df logical Si es FALSE (default) el resultado incluye joined_df: el left_join de el output cargado en el drive con el df. Si es TRUE no lo incluye.
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

comparar_df <- function(df, df_anterior,
                             pk = NULL, k_control_num = 3,
                             drop_joined_df = F) {
  
  
  columns_check <- check_cols(df = df, df_anterior = df_anterior)
  
  stopifnot("parametro 'pk' debe ser character" = !is.null(pk))
  
  stopifnot("las columnas 'pk' deben estar presenten en el output previo y el output nuevo" = all(pk %in% colnames(df_anterior)) & all(pk %in% colnames(df)))
  
  
  df_clases <- check_datatypes(df = df, df_anterior = df_anterior)
  
  # check nrows
  
  sprintf("Filas en output previo: %d\nFilas en output nuevo: %d", nrow(df_anterior), nrow(df))
  
  
  # checkeo por pares de cols
  
  cols_comparacion <- colnames(df_anterior)[!colnames(df_anterior) %in% pk & colnames(df_anterior) %in% colnames(df)]
  
  
  joined_df <- dplyr::left_join(df_anterior, df, by = pk)
  
  checkeo_cols_values <-  purrr::map(cols_comparacion,
                                     .f = function(x,
                                                   data = joined_df,
                                                   pks = pk,
                                                   k_valor = k_control_num) {
                                       
                                       
                                       class_x <- class(data[[paste0(x, ".x")]])
                                       
                                       if (class_x %in% c("numeric", "complex", "integer")) {
                                         
                                         control_valores_num(root_name = x, pk = pks, df = data, k = k_valor)
                                         
                                       } else if (class_x %in% c("character", "logical", "factor")) {
                                         
                                         control_valores_nonnum(root_name = x, pk = pks, df = data)
                                         
                                       } else {
                                         
                                         print("Controles no implementados para la clase")
                                         "Controles no implementados para la clase"
                                         
                                       }
                                       
                                     })
  
  names(checkeo_cols_values) <- cols_comparacion
  
  resultado <- list("check_columnas" = columns_check,
                    "comparacion_clases" = df_clases,
                    "diferencia_nfilas" =  nrow(df) - nrow(df_anterior),
                    "comparacion_cols" = checkeo_cols_values)
  
  if (!drop_joined_df) {
    resultado$joined_df <- joined_df
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
  
  stopifnot("los vectores deben tener igual largo" = length(x) == length(y) )
  
  n_nuevos_na <- sum(!is.na(x) & is.na(y))
  n_nuevos_na
  
}


#' Controles para variables numericas
#'
#' @param root_name raiz del nombre de la columna. Ej. para comparar "valores.x" y "valores.y" se usa "valores"
#' @param pk primary keys de del dataframe
#' @param k numeric cantidad de desvios estandar a partir del cual se seleccionan posibles outliers
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
#' - metricas_filas =  dataframe con pk, col.x, col.y, col_var, variaciones_rel (col.y/col.x -1 ) y varntile (percentil de variaciones_rel)
#'
#'

control_valores_num <- function(root_name, pk, k, df) {
  
  
  col_x <- paste0(root_name,".x")
  col_y <- paste0(root_name,".y")
  
  
  class_x <- class(df[[col_x]])
  class_y <- class(df[[col_y]])
  
  
  stopifnot("la variable no es numeric" = class_x %in% c("numeric", "complex", "integer"))
  stopifnot("la variable no es numeric" = class_y %in% c("numeric", "complex", "integer"))
  
  
  
  na_count <- nuevos_na(df[[col_x]], df[[col_y]])
  
  variaciones_rel <- round((df[[col_y]] - df[[col_x]])/df[[col_x]], 6)
  
  
  df$variaciones_rel <- variaciones_rel
  
  mean_variaciones_rel <- mean(abs(variaciones_rel), na.rm = T)
  
  df$zscaled_variaciones_rel <- as.vector(scale(df$variaciones_rel))
  
  
  df_test <- df[!is.na(df[col_x]) & !is.na(df[col_y]),]
  
  ks_test <- stats::ks.test(df_test[[col_x]], df_test[[col_y]])
  
  mw_test <- stats::wilcox.test(df_test[[col_x]], df_test[[col_y]], paired = F)
  
  
  df_test <- dplyr::select(df_test, dplyr::all_of(c(pk, col_x, col_y)),
                           "variaciones_rel", "zscaled_variaciones_rel")
  
  df_test <- df_test[abs(df_test[["zscaled_variaciones_rel"]]) > k & !is.na(df_test[["zscaled_variaciones_rel"]]),]
  
  df <- df %>% 
    dplyr::mutate(label = purrr::pmap_chr(dplyr::pick(pk),
                            function(...) paste(..., sep = " - ")))
  
  vars_plot <- ggplot2::ggplot(data = df,
                               ggplot2::aes(x = .data[[col_x]],
                                            y = .data[[col_y]],
                                            label  = .data[["label"]]
                                            )) +
    ggplot2::geom_point(shape = 1) +
    ggplot2::geom_smooth(method = stats::lm, se = T) +
    ggplot2::geom_abline(slope = 1, color = "red", alpha = .7) +
    ggplot2::xlab(col_x) + ggplot2::ylab(col_y) +
    ggplot2::theme_minimal()
  
  
  list("nuevos_na" = na_count,
       "mean_variaciones_rel" = mean_variaciones_rel,
       "ks_test" = ks_test$p.value,
       "mw_test" = mw_test$p.value,
       "tasa_posibles_outliers" = nrow(df_test)/nrow(df),
       "plot" = vars_plot,
       "filas_nuevos_na" = df[!is.na(df[[col_x]]) & is.na(df[[col_y]]),],
       "filas_posibles_outliers" = df_test
       
  )
  
  
}


#' Controles para variables no numericas
#'
#' @param root_name raiz del nombre de la columna. Ej. para comparar "valores.x" y "valores.y" se usa "valores"
#' @param pk primary keys de del dataframe
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

control_valores_nonnum <- function(root_name, pk, df) {
  
  col_x <- paste0(root_name, ".x")
  col_y <- paste0(root_name, ".y")
  
  
  class_x <- class(df[[col_x]])
  
  stopifnot("la variable debe ser logical o character" = class_x %in% c("character", "logical", "factor"))
  
  na_count <- nuevos_na(df[[col_x]], df[[col_y]])
  
  coinciden_valores <- tidyr::replace_na(df[[col_y]] == df[[col_x]], F) | (is.na(df[[col_y]]) & is.na(df[[col_x]]))
  
  df$coinciden_valores <- coinciden_valores
  
  df <- df[!df[["coinciden_valores"]] & !is.na(df[["coinciden_valores"]]),]
  
  df <-  dplyr::select(df, dplyr::all_of(c(pk, col_x, col_y)), "coinciden_valores")
  
  list(
    "nuevos_na" = na_count,
    "tasa_mismatches" = sum(!coinciden_valores) / length(coinciden_valores),
    "filas_mismatches" = df
  )
  
  
}


#' Control de nombres de outputs de argendata
#'
#' @param df dataframe nuevo de output
#' @param df_anterior dataframe anterior para la comparacion
#'
#' @return lista de resultados
#' @export
#'

check_cols <- function(df, df_anterior){
  # check columns names
  
  cols_previas <-  colnames(df_anterior)
  
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
  
  columns_check <- list("cols_nuevas" = cols_nuevas,
                        "cols_faltantes" = cols_faltantes)
  
  columns_check
}

#' Control de clases de outputs de argendata
#'
#' @param df dataframe output nuevo
#' @param df_anterior dataframe output previo
#'
#' @return dataframe de control de clases
#' @export
#'

check_datatypes <- function(df, df_anterior) {
  
  # check datatypes
  
  clases_previas <- lapply(df_anterior, class)
  
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

