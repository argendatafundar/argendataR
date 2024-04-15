#' Crear sripts y md del subtopico
#'
#' @param subtopico string codigo de 6 letras del subtopico
#' @param scripts logical Si es TRUE (default) crea los esqueletos de scripts
#' @param md logical Si es TRUE (default) crea un archivo md con la guia del subtopico
#' @param fuentes logical  Si es TRUE (default) crea un archivo .R para la descarga de fuentes de argendata
#' @param source_zero logical  Si es TRUE (default) crea un archivo .R para la ejecucion unficada del subtopico
#'
#' @export
#'

init_subtopico <- function(subtopico, scripts = T, md = T, fuentes = T, source_zero = T) {

  metadata_subtopico <- metadata(subtopico)

  meta <- metadata_subtopico %>%
    dplyr::distinct(.data$dataset_archivo, .data$orden_grafico, .data$script_archivo,
                    .data$fuente_nombre,
                    .data$url_path) %>%
    dplyr::filter(!dplyr::if_all(dplyr::everything(),
                                 .fns = is.na))

  meta$script_nuevo <- paste0(meta$orden_grafico,"_",gsub("\\.([^.]*)$", "", meta$dataset_archivo))


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

  if (isTRUE(fuentes)) {

    escrito_fuentes <-  tryCatch(
        {
          script_fuentes(
            path = sprintf("scripts/subtopicos/%s/fuentes_%s.R", subtopico,  subtopico),
            .navigate = F)

          TRUE
        },
        error  = function(e) {
          warning(sprintf("El archivo %s ya existe, no fue sobreescrito.", sprintf("scripts/subtopicos/%s/%s", subtopico,  subtopico)))
          FALSE
        }
      )
  }

  if (isTRUE(source_zero)) {

    path_source_zero <- sprintf("scripts/subtopicos/%s/0_%s.R", subtopico,  subtopico)

    if (file.exists(path_source_zero)) {
      warning(sprintf("%s ya existe.No se sobreescribio el archivo", sprintf("scripts/subtopicos/%s/0_%s.R", subtopico,  subtopico)))
      escrito_source_zero <- FALSE
    } else {

      file.create(path_source_zero)


      cat(
        glue::glue('source("scripts/subtopicos/{subtopico}/fuentes_{subtopico}.R")'),
        file = path_source_zero,
        sep = "\n",
        append = T
      )

      cat(
        glue::glue('subtopico <-  "{subtopico}"'),
        file = path_source_zero,
        sep = "\n",
        append = T
      )

      cat(
        glue::glue('analista <-  c("")'),
        file = path_source_zero,
        sep = "\n",
        append = T
      )

      cat(
        glue::glue('\n\n#-- Sources -----\n\n'),
        file = path_source_zero,
        sep = "\n",
        append = T
      )

      print(sprintf("scripts/subtopicos/%s/", subtopico))

      purrr::walk(list.files(sprintf("scripts/subtopicos/%s/", subtopico))[!grepl("^fuentes_|^0_.*.R", list.files(sprintf("scripts/subtopicos/%s/", subtopico)))],
                  function(x) {

                    cat(
                      glue::glue('# source("scripts/subtopicos/{subtopico}/{x})"'),
                      file = path_source_zero,
                      sep = "\n",
                      append = T
                    )
                  }
      )

    }
  }



  path_md <- sprintf("scripts/subtopicos/%s/%s.md", subtopico, subtopico)


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
  escrito_fuentes <- sum(escrito_fuentes)

  message(sprintf("Se crearon %d scripts de outputs .R, %d script de fuentes y un %d archivo .md",
                  escritos_r, escrito_fuentes, escrito_md))

}
