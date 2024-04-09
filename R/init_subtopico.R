#' Crear sripts y md del subtopico
#'
#' @param subtopico string codigo de 6 letras del subtopico
#'
#' @export
#'

init_subtopico <- function(subtopico) {
  
  meta <- metadata(subtopico)
  
  meta <- meta %>%
    dplyr::distinct(dataset_archivo, script_archivo,
                    fuente_nombre,
                    url_path) %>%
    dplyr::filter(!dplyr::if_all(dplyr::everything(),
                                 .fns = is.na))
  
  meta$script_nuevo <- gsub("\\.([^.]*)$", "", meta$dataset_archivo)
  
  purrr::walk(unique(meta$script_nuevo), function(x) {
    script_subtopico(
      path = sprintf("scripts/subtopicos/%s/%s", subtopico,  x),
      .navigate = F
    )
    
  })
  
  path_md <- sprintf("scripts/subtopicos/CAMCLI/%s.md", subtopico)
  
  msg <- sprintf("%s ya existe", path_md)
  
  stopifnot(msg = !file.exists(path_md))
  
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
                  dplyr::filter(script_nuevo == x)
                
                cat(
                  sprintf("\n- `%s`:\n", x),
                  file = path_md,
                  sep = "\n",
                  append = T
                )
                
                cat(
                  sprintf("  **Scripts:**", x),
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
                  sprintf("  **Fuentes:**", x),
                  file = path_md,
                  sep = "\n",
                  append = T
                )
                
                df_y <- df_x %>%
                  dplyr::distinct(fuente_nombre, url_path)
                
                purrr::walk2(df_y$fuente_nombre, df_y$url_path, function(x, y) {
                  cat(
                    sprintf("-- %s: %s", x, y),
                    file = path_md,
                    sep = "\n",
                    append = T
                  )
                  
                })
                
                
              })
  
}
