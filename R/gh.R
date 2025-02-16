#' Subir un nuevo archivo a un repositorio GitHub
#'
#' @param path_remoto Ruta completa del archivo en el repositorio remoto (puede incluir carpetas).
#' @param path_local Ruta del archivo local.
#' @param repo_owner Usuario o organización dueña del repositorio.
#' @param repo_name Nombre del repositorio.
#' @param branch Rama del repositorio donde se subirá el archivo.
#' @export
subir_archivo <- function(path_remoto, path_local, repo_owner, repo_name, branch) {
  file_content <- base64enc::base64encode(path_local)
  mensaje_commit <- paste("Subida de", basename(path_local))
  gh("PUT /repos/:owner/:repo/contents/:path",
     owner = repo_owner, repo = repo_name, path = path_remoto,
     message = mensaje_commit, content = file_content, branch = branch)
  message("Archivo nuevo subido exitosamente a GitHub en la rama ", branch, ": ", path_remoto)
}

#' Actualizar un archivo existente en un repositorio GitHub
#'
#' @param path_remoto Ruta completa del archivo en el repositorio remoto.
#' @param path_local Ruta del archivo local.
#' @param sha SHA del archivo remoto a actualizar.
#' @param repo_owner Usuario o organización dueña del repositorio.
#' @param repo_name Nombre del repositorio.
#' @param branch Rama del repositorio donde se actualizará el archivo.
#' @export
actualizar_archivo <- function(path_remoto, path_local, sha, repo_owner, repo_name, branch) {
  if (is.null(sha)) stop("SHA no puede ser nulo. Verifique el archivo remoto antes de intentar actualizar.")
  file_content <- base64enc::base64encode(path_local)
  mensaje_commit <- paste("Actualizacion de", basename(path_local))
  gh("PUT /repos/:owner/:repo/contents/:path",
     owner = repo_owner, repo = repo_name, path = path_remoto,
     message = mensaje_commit, content = file_content, sha = sha, branch = branch)
  message("Archivo actualizado exitosamente en GitHub en la rama ", branch, ": ", path_remoto)
}

#' Subir o actualizar un archivo en un repositorio GitHub
#'
#' @param path_local Ruta completa del archivo local. (Por ejemplo '/tmp/data.json')
#' @param path_remoto Ruta en donde se subirá o actualizará el archivo en el repositorio remoto
#'        (Por ejemplo 'CRECIM/data.json')
#' @param repo_owner Usuario o organización dueña del repositorio.
#' @param repo_name Nombre del repositorio.
#' @param branch Rama del repositorio donde se realizará la operación Por defecto, 'main'.
#' @export
subir_o_actualizar <- function(path_local, path_remoto,
                               repo_owner, repo_name,
                               branch = "main") {
  tryCatch({
    # Obtenemos lista de archivos del repo
    tree <- gh("GET /repos/:owner/:repo/git/trees/:branch?recursive=1",
               owner = repo_owner, repo = repo_name, branch = branch)$tree
    paths <- vapply(tree, function(x) x$path, character(1))
    
    # Verificamos si el path remoto ya existe
    index <- grep(paste0("^", path_remoto, "$"), paths)
    path_remoto_in_repo <- if (length(index) > 0) paths[index] else NULL
    
    # Si existe, obtenemos el sha
    if (!is.null(path_remoto_in_repo)) {
      message("Path encontrado en el repo: ", path_remoto_in_repo)
      sha <- gh("GET /repos/:owner/:repo/contents/:path?ref=:branch",
                owner = repo_owner,
                repo = repo_name,
                path = path_remoto_in_repo,
                branch = branch)$sha
      if (is.null(sha)) {
        stop("No se pudo obtener el SHA remoto.")
      }
    }
    
    # Verificamos que el archivo local exista
    if (!file.exists(path_local)) {
      stop(sprintf("El archivo local no existe en la ruta '%s'", path_local))
    }
    
    # Subimos o actualizamos según corresponda
    if (is.null(path_remoto_in_repo)) {
      # No existe en el repo, se sube como nuevo
      subir_archivo(path_remoto, path_local, repo_owner, repo_name, branch)
    } else {
      # Ya existe, se actualiza
      actualizar_archivo(path_remoto_in_repo, path_local, sha, repo_owner, repo_name, branch)
    }
    
  }, error = function(e) {
    message("Error: ", conditionMessage(e))
  })
}

#' Subir o actualizar un archivo en repositorio data de argendatafundar
#'
#' @param filename Nombre del archivo a buscar en tmpdir.
#' @param subtopico Ruta en el repositorio remoto donde se guardará el archivo.
#' @param branch Rama del repositorio donde se realizará la operación Por defecto, 'dev'.
#' @export
mandar_data <- function(filename, subtopico, branch = "dev") {

 subtopico <- toupper(subtopico)
  
  repo <- gh::gh(endpoint = "/repos/argendatafundar/data/git/trees/main?recursive=false")
  
  tree <- repo$tree
  
  paths <- sapply(tree, function(x) {x$path})
  
  lista_subtopicos <- grep("^[A-Z]{6}$", paths, value = T)
  
  assertthat::assert_that(length(subtopico) == 1)
  assertthat::assert_that(is.character(subtopico))
  assertthat::assert_that(nchar(subtopico) == 6)
  assertthat::assert_that(subtopico %in% lista_subtopicos)

  path_local <- glue::glue("{tempdir()}/{filename}")
  path_remoto <- glue::glue("{subtopico}/{filename}")

  subir_o_actualizar(
    path_local  = path_local,
    path_remoto = path_remoto,
    repo_owner  = "argendatafundar",
    repo_name   = "data",
    branch      = branch
  )
}

