#' Genera json y csv con metadata y data de output para argendata
#'
#' @description
#' Genera un json con la data y metadata del output para Argendata. Opcionalmente exporta tambien un archivo con el formato esperado en Argendata.
#'
#'
#' @param data dataframe Dataframe con datos de output. `class(data)` debe ser 'data.frame'. Req. Default = NULL
#' @param extension string Extension con que se debe escribir el output exportado. Default = 'csv'
#' @param output_name string Nombre del output tal cual se usa para escribir el archivo csv. Req. Default = NULL
#' @param subtopico string Codigo de 6 letras del subtopico al que pertenece. Req. Default = NULL
#' @param fuentes string o numeric Vector con referencia a las fuentes usadas para el output segun aparecen en `fuentes()`. Si es string, los valores deben coincidir con 'nombre' de fuentes registradas, si es numeric deben coincidir con el 'id'. Req. Default = NULL
#' @param analista string Vector con nombre/s de la/s persona/s que crearon originalmente el output. Req. Default = NULL
#' @param aclaraciones string o null Cadena de texto para asentar aclaraciones pertinentes sobre el output. Default = NULL
#' @param exportar logical Si es TRUE escribe un archivo con el path '{output_name}.{extension}' dentro de 'data/\{subtopico\}'. Default = TRUE
#' @param pk string o null Nombres de columnas que son primary key del output. Si es NULL toma todas las columnas como ok. Default NULL.
#' @param es_serie_tiempo logical Valor TRUE o FALSO indicando si el output es una serie de tiempo. Default  = TRUE
#' @param columna_indice_tiempo string o null Nombre de la columna indice de tiempo. Valores aceptados: 'anio', 'fecha'. Otras columnas de referencia temporal (semestre, trim, etc) deberian ser parse. Default = NULL
#' @param columna_geo_referencia string o null Nombre de la columna con el codigo del area geografica si corresponde. Debe ser uno de   'iso3', 'cod_fundar', 'cod_pcia', 'cod_depto', 'eph_codagl', 'cod_aglo', 'cod_agl'. Default = NULL
#' @param nivel_agregacion string o null Nivel de agregacion al que se presentan los datos. Default  = NULL
#' @param nullables string o logical Vector con los nombres de las columnas que admiten nulls o valor logico TRUE o FALSE. Si es FALSE se considera que ninguna columna admite nulls, si es TRUE se considera que todas las columnas adminten nulls. Default = FALSE
#' @param etiquetas_indicadores list o null Lista nombrada con la etiqueta que le corresponde a las columnas. Los nombres de la lista deben coincidir con nombres de columnas de 'data'. Ejemplo: list('gini' = 'Indice de Gini', 'pbipcppp' = 'PBI per capita en parity purchase power'). Si es NULL(default), la funcion busca la columna 'indicador' en 'data' y toma como etiquetas los valores unicos de alli.
#' @param unidades list o null Lista nombrada con las unidades en que estan expresadas las columnas. Los nombres de la lista deben coincidir con nombres de columnas de 'data'. Ejemplo: list('gini' = 'indice', 'pbipcppp' = 'parity purchase power', 'population' = 'millones de personas'). Si es NULL (default), la funcion busca la columna 'unidad' en 'data' y genera una lista tomando las combinaciones unicas de 'unidad' e 'indicador' en 'data'.
#' @param classes list o null Si es null (default) la funcion genera una lista con las clases y nombres de columnas en data. Si es una lista los nombres de la lista deben coincidir con valores en la columna 'indicador' en 'data' y los valores deben ser: 'logical', 'character', 'double', 'interger' o 'date'.
#'
#' @returns Escribe localmente un json con la data y metadata definida usando '{output_name}.json' como path. Opcionalmente tambien escribe un csv '{output_name}.csv'
#' @export
#'
#'

write_output <- function(
    data = NULL,
    extension = 'csv',
    output_name = NULL,
    subtopico = NULL,
    fuentes = NULL,
    analista = NULL,
    aclaraciones = NULL,
    exportar = TRUE,
    pk = NULL,
    es_serie_tiempo = TRUE,
    columna_indice_tiempo = NULL,
    columna_geo_referencia = NULL,
    nivel_agregacion = NULL,
    nullables = FALSE,
    etiquetas_indicadores = NULL,
    unidades = NULL,
    classes = NULL) {


  # chequeos
  ## data

  stopifnot("'data' debe ser un dataframe" = is.data.frame(data))

  ## columnas
  columnas <- colnames(data)

  stopifnot("nombres de columnas invalidos en data" = !all(grepl("[^a-z0-9_]+", columnas)))

  ## subtopico

  # considerar chequear contra listado de subtopicos
  stopifnot("subtopico invalido" =  is.character(subtopico) & length(subtopico) == 1)

  ## fecha

  fecha <- format(Sys.time(), format = "%Y %m %d %X %Z", tz = "GMT0")

  ## nombre output

  stopifnot("'output_name' debe ser character de largo 1" = is.character(output_name) & length(output_name) == 1)

  ## formato

  stopifnot("'extension' debe ser 'csv'" = extension %in% c("csv") & length(extension) == 1)

  ## exportar

  stopifnot("'exportar' debe ser logico TRUE o FALSE " =  is.logical(exportar) & length(exportar) == 1)


  ## fuentes

  stopifnot("'fuentes' debe ser un vector tipo character o numeric" = class(fuentes) == "character" | class(fuentes) == "numeric")

  fuentes_clean_df <- fuentes_clean()
  
  if(is.numeric(fuentes)) {
    stopifnot("Alguna/s de las fuentes no estan cargadas en sheet fuentes" = all(fuentes %in% fuentes_clean_df$id_fuente))
  } else if (is.character(fuentes)) {
    stopifnot("Alguna/s de las fuentes no estan cargadas en sheet fuentes" = all(fuentes %in% fuentes_clean_df$nombre))
  } else if (!is.numeric(fuentes) & !is.character(fuentes)) {
    stop("Input de fuentes invalido. Debe ser vector de strings con nombres de fuentes registradas o vector numerico con id de fuentes registradas")
  }


  ## analistas

  # considerar chequear contra tabla de analistas
  stopifnot("'analista' debe ser character" = is.character(analista))

  # ## nivel de agregacion
  # stopifnot("'nivel_agregacion' debe ser uno de los siguientes strings: " = formato %in% c("pais", "json", "geojson", "shp") & length(formato) == 1)


  ## es_serie_tiempo
  stopifnot("'es_serie_tiempo' debe ser T o F" = is.logical(es_serie_tiempo) & length(es_serie_tiempo) == 1)

  ## columna_indice_tiempo

  if (es_serie_tiempo) {
    stopifnot("'columna_indice_tiempo' no hallada en 'data'. Debe ser uno de: 'anio', 'fecha'." = columna_indice_tiempo %in% columnas & length(columna_indice_tiempo) == 1 & columna_indice_tiempo %in% c("anio", "fecha"))
    if (columna_indice_tiempo == 'anio') {
      stopifnot("columna 'anio' en data debe ser 'numeric'" = class(data['anio']) == "numeric")
    }
    if (columna_indice_tiempo == 'fecha') {
      stopifnot("columna 'fecha' en data debe ser 'Date'" = class(data['fecha']) == "Date")
    }
  }


  ## columna_geo_referencia
  if (!is.null(columna_geo_referencia)) {
    stopifnot("'columna_geo_referencia' no hallada en 'data'. Debe ser uno de   'iso3', 'cod_fundar', 'cod_pcia', 'cod_depto', 'eph_codagl', 'cod_aglo', 'cod_agl'" = columna_geo_referencia %in% columnas & length(columna_geo_referencia) == 1 & columna_geo_referencia %in% c("iso3", "cod_fundar", "cod_pcia", "cod_depto", "eph_codagl", "cod_aglo", "cod_agl"))
  }

  ## nullables
  stopifnot("'nullables' debe ser un vector logico de largo 1 o vector character con nombres de columnas en 'data'." = (is.logical(nullables) & length(nullables) == 1 ) | (is.character(nullables) & length(nullables) <= length(columnas) & all(nullables %in% columnas) ) )

  if (is.logical(nullables)) {
    if(nullables) {
      nullables <- colnames(data)
    } else {
      nullables <- ""
    }
  }

  if (is.character(nullables)) {
    stopifnot("valores en 'nullables' no hallados entre los nombres de columas de 'data'" == all(nullables %in% colnames(data)))
  }

  ## etiquetas_indicadores

  if(is.list(etiquetas_indicadores)) {
    stopifnot("uno o mas nombres de 'etiquetas_indicadores' no coinciden con columnas en 'data.'" = all(names(etiquetas_indicadores) %in% columnas))
    stopifnot("hay etiquetas invalidas. Deben ser character no vacios." = all(sapply(etiquetas_indicadores, function(x) {is.character(x) & x != ""})))
  }  else if (is.null(etiquetas_indicadores)) {

    stopifnot("no se encontro la columna 'indicador' en 'data'. No es posible leer las etiquetas de data[,'indicador']" = "indicador" %in% columnas)
    etiquetas_indicadores <- unique(data[['indicador']])

  } else if (!is.null(etiquetas_indicadores)) {
    stop("'etiquetas_indicadores' debe ser una lista o null.")
  }


  ## unidades
  if(is.list(unidades)) {
    stopifnot("uno o mas nombres de 'unidades' no coinciden con columnas en data." = all(names(unidades) %in% columnas))
    stopifnot("hay 'unidades' invalidas. Deben ser character no vacios." = all(sapply(unidades, function(x) {is.character(x) & x != ""})))
  } else if (is.null(unidades)) {
    stopifnot("No se encontro la columna 'unidad' en 'data'. No es posible leer las unidades en 'data'" = "unidad" %in% columnas)
    unidades <- as.list(dplyr::distinct(data, dplyr::pick("indicador", "unidad"))[["unidad"]])
    names(unidades) <- dplyr::distinct(data, dplyr::pick("indicador", "unidad"))[["indicador"]]
  } else if (!is.null(unidades)) {
    stop("'unidades' debe ser una lista o null.")
  }

  ## classes
  if(is.list(classes)) {

    stopifnot("uno o mas nombres de 'classes' no coinciden con valores en `data['indicador']`" = all(names(classes) %in% unique(data$indicador)))
    stopifnot("hay 'classes' invalidas. Deben ser uno de: 'logical', 'character', 'double', 'interger', 'date'" = all(sapply(classes, function(x) {is.character(x) & x %in% c("double","integer", "character", "logical", "date")})))
    classes <- append(classes, lapply(dplyr::select(data, -"indicador"), class))

  } else if (is.null(classes)) {

    classes <- lapply(data, class)
    names(classes) <- colnames(data)

  } else if (!is.null(unidades)) {

    stop("'classes' debe ser una lista o null.")
  }

  ## pk

  if(is.character(pk)) {
    stopifnot("Valores de 'pk' deben coincidir con nombres de columna en 'data'. Hay uno o mas valores que no coinciden" = all(pk %in% colnames(data)))
  } else if (is.null(pk)) {
    pk <- colnames(data)
  } else {
    stop("'pk' deber ser string con los nombres de columnas correspondientes o null")
  }



  ## inputs

  inputs <- list(
    data = data,
    extension = extension,
    output_name = output_name,
    subtopico = subtopico,
    fuentes = fuentes,
    analista = analista,
    aclaraciones = aclaraciones,
    exportar = exportar,
    pk = pk,
    es_serie_tiempo = es_serie_tiempo,
    columna_indice_tiempo = columna_indice_tiempo,
    columna_geo_referencia = columna_geo_referencia,
    nivel_agregacion = nivel_agregacion,
    nullables = nullables ,
    etiquetas_indicadores = etiquetas_indicadores,
    unidades = unidades,
    classes = classes
  )



  # exportar
  

  if (exportar) {
    data  %>% 
      dplyr::mutate(dplyr::everything(), as.character)  %>% 
      readr::write_csv(file = glue::glue("data/{subtopico}/{output_name}.{extension}"),
                       eol = "\n",
                       quote = "all",
                       escape = "none",
                       na = "")
  }

  inputs
  
}
