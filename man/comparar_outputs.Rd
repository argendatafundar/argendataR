% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comparar_outputs.R
\name{comparar_outputs}
\alias{comparar_outputs}
\title{Comparar un data.frame con un output cargado en el drive de Argendata como csv}
\usage{
comparar_outputs(
  df,
  df_anterior,
  nombre,
  subtopico,
  entrega_subtopico = "primera_entrega",
  pk = NULL,
  k_control_num = 3,
  drop_joined_df = F
)
}
\arguments{
\item{df}{data.frame dataframe a comparar con el output cargado en el drive}

\item{df_anterior}{data.frame dataframe anterior que se usa como referencia de comparacion. Ver \code{argendataR::descargar_output()}.}

\item{nombre}{string nombre del output en el drive para realizar la descarga si no se proveyo \code{df_anterior}}

\item{subtopico}{string subtopico al que pertenece el output para realizar la descarga si no se proveyo \code{df_anterior}. Si no esta definido el parametro busca en el ambiente global de la sesion un objeto llamado "subtopico"}

\item{entrega_subtopico}{string nombre de la carpeta de entrega donde buscar el output para realizar la descarga si no se proveyo \code{df_anterior}}

\item{pk}{string Vector con los nombres de columnas que son primary key del dataframe}

\item{k_control_num}{numeric cantidad de desvios estandar a partir del cual se seleccionan posibles outliers}

\item{drop_joined_df}{logical Si es FALSE (default) el resultado incluye joined_df: el left_join de el output cargado en el drive con el df. Si es TRUE no lo incluye.}
}
\value{
Devuelve una lista con los resultados de los chequeos realizados
}
\description{
El resultado es una lista que contiene cuales son las columnas faltantes (cols_faltantes), cuales son las columnas nuevas (cols_nuevas),
un dataframe que compara las clases de columnas viejas y columnas nuevas (comparacion_clases),
la cantidad de filas de diferencia entre el nuevo dataset y el viejo (diferencia_nfilas), y una lista con metricas de comparacion por columna (comparacion_cols).
La lista de comparacion_cols presenta por cada columna la cantidad de nuevos NA (nuevos_na), la cantidad de mismatches para la columna en funcion de los PK (suma_mismatches) y
ademas un dataframe (metricas_filas) que indica para la columna analizada para cada tupla de valores PK las diferencias absolutas (abs(Xi/Xi-1)) y diferencias relativas (abs(Xi/Xi-1)/abs(Xi-1))
}
