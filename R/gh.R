#' Subir un nuevo archivo a un repositorio GitHub
#'
#' @param path_remoto Ruta completa del archivo en el repositorio remoto.
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
  mensaje_commit <- paste("Actualizaci\\u00f3n de", basename(path_local))
  gh("PUT /repos/:owner/:repo/contents/:path",
     owner = repo_owner, repo = repo_name, path = path_remoto,
     message = mensaje_commit, content = file_content, sha = sha, branch = branch)
  message("Archivo actualizado exitosamente en GitHub en la rama ", branch, ": ", path_remoto)
}

#' Subir o actualizar un archivo en un repositorio GitHub
#'
#' @param nombre_archivo Nombre del archivo a subir o actualizar.
#' @param repo_owner Usuario o organización dueña del repositorio.
#' @param repo_name Nombre del repositorio.
#' @param branch Rama del repositorio donde se realizará la operación Por defecto, 'main'.
#' @export
subir_o_actualizar <- function(nombre_archivo, repo_owner, repo_name, branch = "main") {
  tryCatch({
    tree <- gh("GET /repos/:owner/:repo/git/trees/:branch?recursive=1", owner = repo_owner, repo = repo_name, branch = branch)$tree
    paths <- vapply(tree, function(x) x$path, character(1))
    index <- grep(paste0("/", nombre_archivo, "$"), paths)
    path_remoto <- if (length(index) > 0) paths[index] else NULL

    if (!is.null(path_remoto)) {
      message("Path encontrado en el repo: ", path_remoto)
      sha <- gh("GET /repos/:owner/:repo/contents/:path?ref=:branch",
                owner = repo_owner, repo = repo_name, path = path_remoto, branch = branch)$sha
      if (is.null(sha)) stop("No se pudo obtener el SHA remoto.")
    }

    path_local <- file.path("/tmp", nombre_archivo)
    if (!file.exists(path_local)) stop("El archivo local no existe en /tmp.")

    if (is.null(path_remoto)) {
      subir_archivo(nombre_archivo, path_local, repo_owner, repo_name, branch)
    } else {
      actualizar_archivo(path_remoto, path_local, sha, repo_owner, repo_name, branch)
    }

  }, error = function(e) {
    message("Error: ", conditionMessage(e))
  })
}



#' Subir o actualizar un archivo en repositorio data de argendatafundar
#'
#' @param nombre_archivo Nombre del archivo a subir o actualizar.
#' @param branch Rama del repositorio donde se realizará la operación Por defecto, 'main'.
#' @export
#'

mandar_data <- function(nombre_archivo, branch = "dev") {

  subir_o_actualizar(nombre_archivo,
                     repo_owner = "argendatafundar",
                     repo_name = "data", branch = branch)

}

