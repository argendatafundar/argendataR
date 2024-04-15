#' write_csv estilo Fundar
#'
#' @param x un dataframe a escribir como csv
#' @param file nombre del archivo a escribir
#'
#' @return un archivo
#' @export
#'


write_csv_fundar <- function(x, file) {

    x <- dplyr::mutate(x, dplyr::across(dplyr::everything(), as.character))

    file <- gsub("\\s","_",replace_non_ascii(file))

    readr::write_csv(x = x,
                     file = file,
                     quote = "all",
                     na = "", eol = "\n")
}
