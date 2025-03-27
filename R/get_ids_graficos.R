#' @title Obtener IDs de gráficos de ArgenData
#' @description Extrae los identificadores de gráficos disponibles en una o varias URLs de ArgenData
#' @param url Un vector o lista de caracteres con las URLs de donde se extraerán los IDs de los gráficos
#' @return Un vector de caracteres con los IDs de los gráficos, ordenados alfabéticamente y sin duplicados
#' @export
#' @examples
#' \dontrun{
#' ids <- get_ids_graficos("https://argendata.fund.ar/notas/que-es-la-intensidad-de-carbono/")
#' }


get_ids_graficos <- function(url) {
  
  ids <- lapply(url, inner_get_ids_graficos)
  
  ids <- unlist(ids)

  ids <- unique(ids)
  
  ids <- sort(ids)
  
  ids
}

#' @title Función interna para obtener IDs de gráficos
#' @description Función auxiliar que extrae los identificadores de gráficos de una única URL de ArgenData
#' @param url Una cadena de caracteres con la URL de donde se extraerán los IDs de los gráficos
#' @return Un vector de caracteres con los IDs de los gráficos encontrados
#' @keywords internal
#' @importFrom assertthat is.string assert_that
#' @importFrom httr2 request req_perform resp_status resp_content_type
#' @importFrom rvest read_html html_elements html_attr
#' @importFrom stringr str_remove_all
inner_get_ids_graficos <- function(url) {
  
  assertthat::is.string(url)
  
  resp <- httr2::request(url) %>% 
    httr2::req_perform()
  
  assertthat::assert_that(resp %>% 
                            httr2::resp_status() == "200")
  

  assertthat::assert_that(  resp %>% 
                              httr2::resp_content_type() == "text/html")  
  
  html <- rvest::read_html(resp$body)
  elementos <- rvest::html_elements(html, css = ".chart.grafico_argendata")
  atributos_id <- rvest::html_attr(elementos, "id")
  ids <- stringr::str_remove_all(atributos_id, "wrapper_grafico_")
  ids
}