#' Scraper de datos de OWID
#'
#' @param url string con url al grafico de OWID del que se quieren tomar los datos
#'
#' @return dataframe Devuelve un dataframe con los datos y algunos metadatos
#'
#' @export
#'

owid_scraper <- function(url) {

  pedido <- httr2::request(url)

  rpta <- httr2::req_perform(pedido)

  contenido_html <- rpta %>%
    httr2::resp_body_html()

  links <- contenido_html %>%
    rvest::html_elements("link")

  links <- links[grepl("api.ourworldindata.org/v1/indicators",
                       rvest::html_attr(x = links, name = "href"))]

  links <- links %>%
    rvest::html_attr("href")

  data_links <- links[grepl("\\.data", links)]

  metadata_links <- links[grepl("\\.metadata", links)]

  ids_data <- as.character(stringr::str_extract_all(data_links, "(?<=\\/)\\d+(?=\\.)", simplify = T))
  ids_metadata <- as.character(stringr::str_extract_all(data_links, "(?<=\\/)\\d+(?=\\.)", simplify = T))

  data_owid <- purrr::map(data_links, function(x) {
    jsonlite::fromJSON(x)
  })

  names(data_owid) <- ids_data

  data_owid <- dplyr::bind_rows(data_owid, .id = "id")

  metadata_owid <- purrr::map(metadata_links, function(x) {
    jsonlite::fromJSON(x)
  })

  names(metadata_owid) <- ids_metadata

  metadata_owid <- purrr::map(metadata_owid, function(x) {
    metadata_i <- x[c("id", "name", "unit", "shortUnit", "datasetId", "presentation","dimensions")]
    metadata_i$entities <- metadata_i$dimensions$entities$values
    metadata_i$title <- metadata_i$presentation$titlePublic
    metadata_i[c("id", "name", "unit", "shortUnit", "datasetId", "title","entities")]
  })

  metadata_owid <- lapply(metadata_owid, function(x) {purrr::compact(x)})

  metadata_owid <-  dplyr::bind_rows(metadata_owid) %>%
    tidyr::unnest_wider("entities", names_sep = "_", names_repair = "unique")

  metadata_owid <- metadata_owid %>%
    dplyr::mutate(id = as.character(.data$id),
           entities_id = as.character(.data$entities_id))

  data_owid_wide <- data_owid %>%
    dplyr::mutate(id = as.character(.data$id),
           entities = as.character(.data$entities)) %>%
    tidyr::pivot_wider(names_from = .data$years, values_from = .data$values,
                       id_cols = c(.data$id, .data$entities))

  dataset_owid <- metadata_owid %>%
    dplyr::left_join(data_owid_wide,  by = c("id" = "id",
                                             "entities_id" = "entities"))

  dataset_owid
}
