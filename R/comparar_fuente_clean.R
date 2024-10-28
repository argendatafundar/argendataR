#' Comparar un data.frame con una fuente clean
#'
#' @param df data.frame dataframe a comparar con el output cargado en el drive
#' @param df_anterior data.frame dataframe anterior que se usa como referencia de comparacion. Ver `argendataR::descargar_output()`.
#' @param id  string con el codigo o id numerico con la fuente tal cual aparece en fuentes_clean()
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


comparar_fuente_clean <- function(df, df_anterior,
                                  id,
                                  pk = NULL, k_control_num = 3,
                                  drop_joined_df = F) {
  
  
  
  if (missing(df_anterior)) {
    
    message("No se recibio un dataframe de referencia. Se procede a descarga desde etl-fuentes/clean")
  
    
    stopifnot("'id' debe ser codigo de fuente clean valido" = id %in% fuentes_clean()$codigo | id %in% fuentes_clean()$id_fuente_clean )
    
    df_anterior <- read_fuente_clean(id)

    
  }
  
  comparar_df(df = df, df_anterior =  df_anterior,
              pk = pk, k_control_num = k_control_num,
              drop_joined_df = drop_joined_df)
  
}