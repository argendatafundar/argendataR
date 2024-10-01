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
#' @param fuentes string Vector con los codigos de las fuentes usadas para el output segun aparecen en `fuentes()`.  Req. Default = NULL
#' @param analista string Vector con nombre/s de la/s persona/s que crearon originalmente el output. Req. Default = NULL
#' @param aclaraciones string o null Cadena de texto para asentar aclaraciones pertinentes sobre el output. Default = NULL
#' @param exportar logical Si es TRUE escribe un archivo con el path '{output_name}.{extension}' dentro de 'data/\{subtopico\}'. Default = TRUE
#' @param pk string o null Nombres de columnas que son primary key del output. Si es NULL toma todas las columnas como ok. Default NULL.
#' @param es_serie_tiempo logical Valor TRUE o FALSO indicando si el output es una serie de tiempo. Default  = TRUE
#' @param columna_indice_tiempo string o null Nombre de la columna indice de tiempo. Valores aceptados: 'anio', 'fecha'. Otras columnas de referencia temporal (semestre, trim, etc) deberian ser parse. Default = NULL
#' @param columna_geo_referencia string o null Nombre de la columna con el codigo del area geografica si corresponde. Debe ser uno de   'iso3', 'cod_fundar', 'cod_pcia', 'cod_depto', 'eph_codagl', 'cod_aglo', 'cod_agl'. Default = NULL
#' @param nivel_agregacion string o null Nivel de agregacion al que se presentan los datos. Default  = NULL
#' @param nullables string o logical Vector con los nombres de las columnas que admiten nulls o valor logico TRUE o FALSE. Si es FALSE se considera que ninguna columna admite nulls, si es TRUE se considera que todas las columnas adminten nulls. Default = FALSE
#' @param cambio_nombre_output list o null Lista nombrada con la definicion del nombre anterior y el nombre nuevo del dataset.Ej. `list('nombre_nuevo' = 'exportaciones_pc_por_pais', 'nombre_anterior' = 'export_pc_por_pais_2022')`
#' @param cambio_nombre_cols list o null Lista nombrada con los pares de nombre nuevo y nombre anterior de las columnas que cambiaron de nombre.Ej. `list('nombre_nuevo' = 'nombre_anterior', 'pobreza_jovenes' = 'poberz_18_30')`
#' @param unidades list o null Lista nombrada con las unidades en que estan expresadas las columnas. Los nombres de la lista deben coincidir con nombres de columnas de 'data'. Ejemplo: list('gini' = 'indice', 'pbipcppp' = 'parity purchase power', 'population' = 'millones de personas'). Si es NULL (default), la funcion busca la columna 'unidad' en 'data' y genera una lista tomando las combinaciones unicas de 'unidad' e 'indicador' en 'data'.
#' @param classes list o null Si es null (default) la funcion genera una lista con las clases y nombres de columnas en data. Si es una lista los nombres de la lista deben coincidir con valores en la columna 'indicador' en 'data' y los valores deben ser: 'logical', 'character', 'double', 'interger' o 'date'.
#' @param descripcion_columnas list o null Lista nombrada con la descripcion que le corresponde a cada columna. Los nombres de la lista deben coincidir con nombres de columnas de 'data'. Ejemplo: list('gini' = 'Indice de Gini', 'pbipcppp' = 'PBI per capita en parity purchase power'). Si es NULL(default), la funcion busca la columna 'indicador' en 'data' y toma como etiquetas los valores unicos de alli.
#' @param directorio string Ruta al directorio desde el cual cargar el archivo. Si es NULL toma tempdir()
#' @param control list Lista que resulta de la comparacion entre output anterior y output nuevo. Ver `comparar_outputs()`
#' @param ... parametros adicionales para captura de aliases anteriores.
#' @returns Escribe localmente un json con la data y metadata definida usando '{output_name}.json' como path. Opcionalmente tambien escribe un csv '{output_name}.csv'
#' @export
#'
#'

write_output <- function(
    data = NULL,
    extension = 'csv',
    output_name = NULL,
    directorio = NULL,
    subtopico = NULL,
    fuentes = NULL,
    analista = NULL,
    aclaraciones = NULL,
    exportar = TRUE,
    control = NULL,
    cambio_nombre_output = NULL,
    cambio_nombre_cols = NULL,
    pk = NULL,
    es_serie_tiempo = TRUE,
    columna_indice_tiempo = NULL,
    columna_geo_referencia = NULL,
    nivel_agregacion = NULL,
    nullables = FALSE,
    descripcion_columnas = NULL,
    unidades = NULL,
    classes = NULL,
    ...) {

  ## dots evaluation
  dots <- list(...)
  if ("etiquetas_indicadores" %in% names(dots)) {
    if (missing(descripcion_columnas)) {
      descripcion_columnas <- dots[["etiquetas_indicadores"]]
    } else {
      warning("'descripcion_columnas' y 'etiquetas_indicadores' recibidos, ignorando 'etiquetas_indicadores'")
    }
  }

  if (is.null(directorio)) {
    directorio <- tempdir()
  } else {
    stopifnot("'directorio' debe ser string a una ruta valida" = dir.exists(directorio))
  }



  # chequeos
  ## data

  stopifnot("'data' debe ser un dataframe" = is.data.frame(data))

  ## columnas
  columnas <- colnames(data)

  stopifnot("nombres de columnas invalidos en data" = !all(grepl("[^a-z_]+", columnas)))

  ## subtopico

  # considerar chequear contra listado de subtopicos
  stopifnot("subtopico invalido" =  is.character(subtopico) & length(subtopico) == 1)

  ## fecha

  fecha <- format(Sys.time(), format = "%Y %m %d %X %Z", tz = "GMT0")

  ## nombre output

  stopifnot("'output_name' debe ser characters '[a-z_]' de largo 1" = is.character(output_name) & length(output_name) == 1 & !grepl("[^a-z_]+", output_name))

  output_name <- gsub("\\.csv$","",output_name)

  ## formato

  stopifnot("'extension' debe ser 'csv'" = extension %in% c("csv") & length(extension) == 1)

  ## exportar

  stopifnot("'exportar' debe ser logico TRUE o FALSE " =  is.logical(exportar) & length(exportar) == 1)


  ## fuentes

  stopifnot("'fuentes' debe ser un vector tipo character" = class(fuentes) == "character")

  fuentes_df <- fuentes()

  if (is.character(fuentes)) {
    stopifnot("Alguna/s de las fuentes no estan cargadas. Ver codigos en `fuentes()`" = all(fuentes %in% fuentes_df[["codigo_raw"]] | fuentes %in% fuentes_df[["codigo_clean"]]))
  } else {
    stop("Input de fuentes invalido. Debe ser vector de strings con codigos de fuentes registradas")
  }


  ## analistas

  # considerar chequear contra tabla de analistas
  stopifnot("'analista' debe ser character" = is.character(analista))

  # ## nivel de agregacion
  # stopifnot("'nivel_agregacion' debe ser uno de los siguientes strings: " = formato %in% c("pais", "json", "geojson", "shp") & length(formato) == 1)


  ## es_serie_tiempo
  stopifnot("'es_serie_tiempo' debe ser T o F" = is.logical(es_serie_tiempo) & length(es_serie_tiempo) == 1)

  ## columna_indice_tiempo

  if (isTRUE(es_serie_tiempo)) {
    stopifnot("'columna_indice_tiempo' no hallada en 'data'" = all(columna_indice_tiempo %in% columnas))

  } else if (isFALSE(es_serie_tiempo)) {
    stopifnot("'columna_indice_tiempo' debe ser NULL si el dataset no es serie de tiempo" = is.null(columna_indice_tiempo))

  } else {
    stop("'es_serie_tiempo' debe ser TRUE o FALSE")
  }


  ## columna_geo_referencia
  if (!is.null(columna_geo_referencia)) {

    stopifnot("'columna_geo_referencia' no hallada en 'data'" = is.character(columna_geo_referencia) & all(columna_geo_referencia %in% columnas))

    # stopifnot("'columna_geo_referencia' no hallada en 'data'. Debe ser uno de   'iso3', 'cod_fundar', 'prov_cod', 'cod_pcia', 'cod_depto', 'eph_codagl', 'cod_aglo', 'cod_agl'" = columna_geo_referencia %in% c("iso3", "prov_cod", "cod_fundar", "cod_pcia", "cod_depto", "eph_codagl", "cod_aglo", "cod_agl"))
  }

  ## nullables
  stopifnot("'nullables' debe ser un vector logico de largo 1 o vector character con nombres de columnas en 'data'." = (is.logical(nullables) & length(nullables) == 1 ) | (is.character(nullables) & length(nullables) <= length(columnas) & all(nullables %in% columnas) ) )

  if (is.logical(nullables)) {
    if (nullables) {
      nullables <- colnames(data)
    } else {
      nullables <- ""
    }
  } else if (is.character(nullables)) {
    stopifnot("valores en 'nullables' no hallados entre los nombres de columas de 'data'" == all(nullables %in% colnames(data)))

  }

  ## descripcion_columnas

  if (is.list(descripcion_columnas)) {
    stopifnot("uno o mas nombres de 'descripcion_columnas' no coinciden con columnas en 'data.'" = all(names(descripcion_columnas) %in% columnas))
    stopifnot("una o mas columnas no descriptas en 'descripcion_columnas'" = all(columnas %in% names(descripcion_columnas)))
    stopifnot("hay etiquetas invalidas. Deben ser character no vacios." = all(sapply(descripcion_columnas, function(x) {is.character(x) & x != ""})))
    stopifnot("hay columnas repetidas, cada columna solo debe declararse 1 vez" = all(sapply(unique(names(descripcion_columnas)), function(i) sum(names(descripcion_columnas) == i) == 1 )) )

  }  else if (is.null(descripcion_columnas)) {

    stop("Es obligatorio pasar una lista de descripcion de columnas")
    # stopifnot("no se encontro la columna 'indicador' en 'data'. No es posible leer las etiquetas de data[,'indicador']" = "indicador" %in% columnas)
    # descripcion_columnas <- unique(data[['indicador']])

  } else if (!is.null(descripcion_columnas)) {
    stop("'descripcion_columnas' debe ser una lista con la descripcion de columnas")
  }


  ## unidades
  if (is.list(unidades)) {
    stopifnot("uno o mas nombres de 'unidades' no coinciden con columnas en data." = all(names(unidades) %in% columnas))
    stopifnot("hay 'unidades' invalidas. Deben ser character no vacios." = all(sapply(unidades, function(x) {is.character(x) & x != ""})))
  } else if (is.null(unidades)) {
    stopifnot("No se encontro la columna 'unidad' en 'data'. No es posible leer las unidades en 'data'" = "unidad" %in% columnas)
    stopifnot("No se encontro la columna 'indicador' en 'data'. No es posible leer las unidades en 'data'" = "indicador" %in% columnas)
    unidades <- as.list(dplyr::distinct(data, dplyr::pick("indicador", "unidad"))[["unidad"]])
    names(unidades) <- dplyr::distinct(data, dplyr::pick("indicador", "unidad"))[["indicador"]]
  } else if (!is.null(unidades)) {
    stop("'unidades' debe ser una lista o null.")
  }

  ## classes
  if (is.list(classes)) {

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

  if (is.character(pk)) {
    stopifnot("Valores de 'pk' deben coincidir con nombres de columna en 'data'. Hay uno o mas valores que no coinciden" = all(pk %in% colnames(data)))
  } else if (is.null(pk)) {
    pk <- colnames(data)
  } else {
    stop("'pk' deber ser string con los nombres de columnas correspondientes o null")
  }

  ## control

  if (is.list(control)) {

    control$comparacion_cols <- lapply(control$comparacion_cols,
                                           function(x) {x[names(x) != "plot"]})


    control <- control[names(control) != "joined_df"]


    colscontrol <- names(control[["comparacion_cols"]])

    for (i  in colscontrol) {

      if ( "ks_test" %in% names(control[["comparacion_cols"]][[i]]) ) {

        stopifnot("El dataset tiene una variable numerica que no cumple los test de control" = control[["comparacion_cols"]][[i]]$ks_test > .2 &  control[["comparacion_cols"]][[i]]$mw_test > .2)

      } else {

        stopifnot("El dataset tiene una variable no numerica que no cumple los test de control" = control[["comparacion_cols"]][[i]]$tasa_mismatches < .05 )


      }


    }


  } else {

    control <- "no se incluyeron controles"

  }

  # diccionario_cambios

  stopifnot("'cambio_nombre_output' debe ser NULL o una lista" = is.null(cambio_nombre_output) | is.list(cambio_nombre_output))

  if (is.list(cambio_nombre_output)) {

    nombres_lista_output <- names(cambio_nombre_output)

    stopifnot("'cambio_nombre_output' debe ser una lista con nombres" = is.list(cambio_nombre_output) & is.character(names(cambio_nombre_output)))

    stopifnot("los nombres en lista 'cambio_nombre_output' solo deben ser 'nombre_anterior' y/o 'nombre_nuevo' " = all(nombres_lista_output %in% c("nombre_anterior", "nombre_nuevo")))

    stopifnot("'nombre_anterior' esta repetido, solo debe haber un elemento 'nombre_anterior'" = sum(names(nombres_lista_output == 'nombre_anterior')) == 1 | sum(names(nombres_lista_output == 'nombre_anterior')) == 0)

    stopifnot("'nombre_nuevo' esta repetido, solo debe haber un elemento 'nombre_nuevo'" = sum(names(nombres_lista_output == 'nombre_nuevo')) == 1 | sum(names(nombres_lista_output == 'nombre_nuevo')) == 0)

  } else {

    stopifnot("'cambio_nombre_output' debe ser list o NULL" = is.null(cambio_nombre_output))

  }

  stopifnot("'cambio_nombre_cols' debe ser NULL o una lista" = is.null(cambio_nombre_cols) | is.list(cambio_nombre_cols))

  if (is.list(cambio_nombre_cols)) {

    nombres_lista_cols <- names(cambio_nombre_cols)

    stopifnot("'cambio_nombre_cols' debe ser una lista con nombres" = is.list(cambio_nombre_cols) & is.character(names(cambio_nombre_cols)))

    stopifnot("los nombres en lista 'cambio_nombre_cols' deben coincidir con las columnas del dataframe 'data'" = all(nombres_lista_cols %in% columnas))

    stopifnot("hay nombres de columna repetidos en 'cambio_nombre_cols'" = all(sapply(unique(nombres_lista_cols), function(i) sum(nombres_lista_cols == i) == 1 )))

    stopifnot("los elementos 'cambio_nombre_cols' deben ser tipo character de largo 1" = all(sapply(cambio_nombre_cols, function(x) {class(x) == "character" & length(x) == 1})))

    stopifnot("los elementos 'cambio_nombre_cols' no pueden ser string vacio ''" = all(sapply(cambio_nombre_cols, function(x) {x != ""})))

  } else {

    stopifnot("'cambio_nombre_cols' debe ser list o NULL" = is.null(cambio_nombre_cols))

  }




  ## inputs

  inputs <- list(
    subtopico = subtopico,
    output_name = output_name,
    extension = extension,
    analista = analista,
    fuentes = fuentes,
    cambio_nombre_output = cambio_nombre_output,
    cambio_nombre_cols = cambio_nombre_cols,
    aclaraciones = aclaraciones,
    control = control,
    exportar = exportar,
    pk = pk,
    es_serie_tiempo = es_serie_tiempo,
    columna_indice_tiempo = columna_indice_tiempo,
    columna_geo_referencia = columna_geo_referencia,
    nivel_agregacion = nivel_agregacion,
    nullables = nullables ,
    descripcion_columnas = descripcion_columnas,
    unidades = unidades,
    classes = classes,
    data = data
  )



  # exportar


  if (exportar) {
    data  %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))  %>%
      readr::write_csv(file = normalize_path(glue::glue("{directorio}/{output_name}.{extension}")),
                       eol = "\n",
                       quote = "all",
                       escape = "none",
                       na = "")

    message(glue::glue("Se escribio el archivo: {directorio}/{output_name}.{extension}"))
  }

  jsonlite::write_json(x = inputs, path = normalize_path(glue::glue("{directorio}/{output_name}.json")))

  message(glue::glue("Se escribio el archivo: {directorio}/{output_name}.json"))

}
