#' Devuelve el nomenclador geografico para argendata
#'
#' @return tibble con la sheet de consolidado_fundar del drive de Argendata
#' @export
#'

get_nomenclador_geografico <- function() {

  filetemp <- list.files(tempdir(), full.names = T)[grepl("nomenclador_geografico", list.files(tempdir()))]

  if (length(filetemp) == 1) {

    jsonlite::fromJSON(filetemp) %>%
      tidyr::as_tibble() %>%
      suppressMessages()


  } else {

    temp <- tempfile(pattern = "nomenclador_geografico_argdt",
                     fileext = ".json")

    bbdd_tree <- bbdd_dir()$tree

    clasificadores_nomecladores <- googledrive::drive_ls(googledrive::as_id(x = bbdd_tree[bbdd_tree$name == "Clasificadores y Nomencladores",][["id"]]))

    geograficos <- googledrive::drive_ls(googledrive::as_id(clasificadores_nomecladores[clasificadores_nomecladores$name == "GEOGRAFICOS",][["id"]]))

    id <-  geograficos[grepl("consolidado_fundar_paises_agregaciones3.json",
                             geograficos$name),]$id

    googledrive::drive_download(googledrive::as_id(id), path = temp)

    jsonlite::fromJSON(temp) %>% tidyr::as_tibble() %>%  suppressMessages()


    }

  }
