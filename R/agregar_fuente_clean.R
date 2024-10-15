#' Agregar nueva fuente clean
#'
#' @description
#' Agrega una fuente no registrada previamente: genera una nueva entrada en la sheet de fuentes y hace `drive_upload()` con overwrite = F de la fuente.
#'
#' @param df data.frame Dataframe de la fuente clean a registrar.
#' @param id_fuente_raw integer id numerico que permite seleccionar la fuente raw segun aparece en el sheet. Para consultar ids usar  `fuentes_raw()`
#' @param nombre string Nombre único que identifica a la fuente en su versión 'clean'.
#' @param script string  Nombre del archivo del script de descarga de la fuente tal cual se guardó en scripts/limpieza_fuentes/ de argendata-etl
#' @param path_clean string Nombre del archivo de la fuente tal cual fue guardado.
#' @param descripcion string Descripcion del dataset
#' @param directorio string Ruta al directorio desde el cual cargar el archivo. Si es NULL toma tempdir()
#' @param prompt logical Si es TRUE (default) evalua si ya fuentes clean referidas al id_fuente_raw y pide confirmacion antes de continuar.
#' @details
#' Más de una fuente clean puede referir a una misma fuente raw. Por ejemplo, si la fuente raw consiste en un excel de multiples hojas, cada hoja debería pasar a ser un csv independiente.
#'
#'
#' @export
#'


agregar_fuente_clean <- function(id_fuente_raw = NULL,
                                 df = NULL,
                               path_clean = NULL,
                               nombre = NULL,
                               script = NULL,
                               descripcion = NULL,
                               directorio = NULL,
                               prompt = TRUE) {

  inputs <- list(
    id_fuente_raw = id_fuente_raw,
    path_clean = path_clean,
    nombre = nombre,
    script = script,
    fecha = Sys.time()
  )


  stopifnot("No se admiten parametros nulos" = !any(sapply(inputs, is.null)))

  stopifnot("No se admiten parametros con NAs" = !any(sapply(inputs, is.na)))

  stopifnot("No se admiten parametros con string vacios. Eg: ''" = !any(sapply(inputs, function(x) {as.character(x) == ''})))

  df_fuentes_raw <- fuentes_raw()

  stopifnot("El id_fuente_raw no existe en la sheet de fuentes raw. Verificar si es un id valido en  `fuentes_raw()`" = id_fuente_raw %in% df_fuentes_raw$id_fuente)

  df_fuentes_clean <- fuentes_clean()

  df_fuentes_clean_md5 <- tools::md5sum(glue::glue("{RUTA_FUENTES()}/fuentes_clean.csv"))

  control <- df_fuentes_clean[df_fuentes_clean$id_fuente_raw == id_fuente_raw,]


  if (!isFALSE(prompt) & nrow(control) > 0) {
    print(sprintf("Hay %d fuentes clean cargadas con el id %d", nrow(control), id_fuente_raw))
    print(control)
    ok <- readline(prompt = "Continuar con el registro de fuente clean? Y/N")

    stopifnot("Registro cancelado." = ok == "Y")

  }

  if (nrow(df_fuentes_clean[df_fuentes_clean$nombre == inputs$nombre & df_fuentes_clean$id_fuente_raw == inputs$id_fuente_raw,]) != 0) {

    print(df_fuentes_clean[df_fuentes_clean$nombre == inputs$nombre & df_fuentes_clean$id_fuente_raw == inputs$id_fuente_raw,])
    stop("Ya existe esa combinacion nombre y id_fuente_raw. Verificar si es una posible duplicacion o cambiar de nombre")
  }



  if (!file.exists(paste0("scripts/limpieza_fuentes/", inputs$script)) &
      !file.exists(inputs$script)) {
    stop("No se encontro el archivo script en scripts/limpieza_fuentes/. Guardarlo en la ubicacion antes de continuar")
  }

  if (is.data.frame(df)) {

    message("El df sera guardado como parquet")


  } else if (!is.data.frame(df)) {

    if (is.null(directorio)) {
      directorio <- tempdir()
    } else {
      stopifnot("'directorio' debe ser string a una ruta valida" = dir.exists(directorio))
    }

    stopifnot("La extension de la fuente clean debe ser parquet" = grepl("\\.parquet$", inputs$path_clean))

    stopifnot("Directorio y path_clean no son ruta valida" = file.exists(normalize_path(glue::glue("{directorio}/{inputs$path_clean}"))))


    if (file.size(normalize_path(glue::glue("{directorio}/{inputs$path_clean}"))) > 1E8) {
      warning("El peso del archivo supera el limite de github ")
    }

  } else {
    stop("Debe ingresar un dataframe valido o un path_clean valido")
  }



  last_id <- dplyr::last(df_fuentes_clean$id_fuente_clean)

  if (is.na(last_id)) {
    next_id <- 1
  } else {
    next_id <- last_id + 1

  }

  inputs$id_fuente_clean <- next_id

  inputs$codigo <- sprintf("R%dC%d", inputs$id_fuente_raw, inputs$id_fuente_clean )

  stopifnot("'descripcion' debe ser null o character" = is.null(descripcion) | is.character(descripcion))

  inputs$descripcion <- ifelse(!is.character(descripcion), "", descripcion)

  print(paste("La fuente quedara registrada con el codigo:", inputs$codigo))

  print(

    tibble::as_tibble(inputs)  %>%
      dplyr::select(  "id_fuente_clean" ,
                      "id_fuente_raw",
                      "path_clean",
                      "nombre",
                      "script",
                      "fecha",
                      "codigo",
                      "descripcion")
  )




  if (path_clean %in% list.files(glue::glue("{RUTA_FUENTES()}/clean"))) {
    print(df_fuentes_clean[df_fuentes_clean$path_clean == path_clean, ])
    stop("Ya existe un archivo con ese nombre. Cambiar el nombre del archivo o borrar el archivo existente")

  }

  stopifnot("En la tabla clean ya existe el path indicado" = nrow(df_fuentes_clean[df_fuentes_clean$path_clean == path_clean, ]) == 0)

  stopifnot("El registro de fuentes cambio antes de finalizar la actualizacion. Vuelva a intentarlo" = df_fuentes_clean_md5 == tools::md5sum(glue::glue("{RUTA_FUENTES()}/fuentes_clean.csv")))


  if (is.data.frame(df)) {

    df %>%
      arrow::write_parquet(sink = glue::glue("{RUTA_FUENTES()}/clean/{path_clean}"), compression = "snappy")

    message("Parquet creado")

  } else if (!is.data.frame(df) & file.exists(normalize_path(paste(directorio, inputs$path_clean, sep = "/")))) {


    if (file.size(glue::glue("{directorio}/{path_clean}")) > 1E8) {
      warning("El peso del archivo supera el limite de github ")
    }

    file.copy(from = glue::glue("{directorio}/{inputs$path_clean}"),
              to = glue::glue("{RUTA_FUENTES()}/clean/{inputs$path_clean}"), overwrite = T)

    message("Parquet creado")


  } else {
    stop("Error inesperado al guardar el parquet")
  }



  tibble::as_tibble(inputs)  %>%
    dplyr::select(  "id_fuente_clean" ,
                    "id_fuente_raw",
                    "path_clean",
                    "nombre",
                    "script",
                    "fecha",
                    "codigo",
                    "descripcion")  %>%
    readr::write_csv(file = glue::glue("{RUTA_FUENTES()}/fuentes_clean.csv"), eol = "\n", append = T)

  message("Tabla de fuentes clean actualizada")



}
