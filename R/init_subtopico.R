#' Crear sripts y md del subtopico
#'
#' @param subtopico string codigo de 6 letras del subtopico
#' @param scripts logical Si es TRUE (default) crea los esqueletos de scripts
#' @param md logical Si es TRUE (default) crea un archivo md con la guia del subtopico
#'
#' @export
#'

init_subtopico <- function(subtopico, scripts = T, md = T) {

  metadata_subtopico <- metadata(subtopico)

  meta <- metadata_subtopico %>%
    dplyr::distinct(.data$dataset_archivo, .data$script_archivo,
                    .data$fuente_nombre,
                    .data$url_path) %>%
    dplyr::filter(!dplyr::if_all(dplyr::everything(),
                                 .fns = is.na))

  meta$script_nuevo <- gsub("\\.([^.]*)$", "", meta$dataset_archivo)


  if (isTRUE(scripts)) {

      escritos_r <- purrr::map(unique(meta$script_nuevo), function(x) {

        tryCatch(
          {
            script_subtopico(
            path = sprintf("scripts/subtopicos/%s/%s", subtopico,  x),
            .navigate = F)

            TRUE
            },
          error  = function(e) {
            warning(sprintf("El archivo %s ya existe, no fue sobreescrito.", x))
            FALSE
            }
        )


     })
  }



  path_md <- sprintf("scripts/subtopicos/%s/%s.md", subtopico)


  if (isTRUE(md)) {


    if (file.exists(path_md)) {
      warning(sprintf("%s ya existe.No se sobreescribio el archivo", path_md))
      escrito_md <- FALSE
    } else {

      file.create(path_md)



      cat(
        sprintf("# %s\n\n## Lista de datasets\n\n", subtopico),
        file = path_md,
        sep = "\n",
        append = T
      )

      purrr::walk(unique(meta$script_nuevo),
                  function(x) {
                    cat(
                      sprintf("- [ ] %s", x),
                      file = path_md,
                      sep = "\n",
                      append = T
                    )
                  })

      cat(
        sprintf("\n\n## Referencias datasets\n\n"),
        file = path_md,
        sep = "\n",
        append = T
      )

      purrr::walk(unique(meta$script_nuevo),
                  function(x) {
                    df_x <- meta %>%
                      dplyr::filter(.data$script_nuevo == x)

                    cat(
                      sprintf("\n- `%s`:\n", x),
                      file = path_md,
                      sep = "\n",
                      append = T
                    )

                    cat(
                      "  **Scripts:**",
                      file = path_md,
                      sep = "\n",
                      append = T
                    )

                    purrr::walk(unique(df_x$script_archivo), function(x) {
                      cat(
                        sprintf("-- %s", x),
                        file = path_md,
                        sep = "\n",
                        append = T
                      )

                    })
                    cat(
                      "  **Fuentes:**",
                      file = path_md,
                      sep = "\n",
                      append = T
                    )

                    df_y <- df_x %>%
                      dplyr::distinct(.data$fuente_nombre, .data$url_path)

                    purrr::walk2(df_y$fuente_nombre,
                                 df_y$url_path, function(x, y) {
                                   cat(
                                     sprintf("-- %s: %s", x, y),
                                     file = path_md,
                                     sep = "\n",
                                     append = T
                                   )

                                 })

                  })


      escrito_md <-  TRUE

    }

  } else {
      escrito_md <-  FALSE

    }

  escritos_r <- sum(unlist(escritos_r))
  escrito_md <- sum(escrito_md)

  message(sprintf("Se crearon %d archivos .R y %d archivo .md", escritos_r, escrito_md))

}
