#' Fuentes clean
#'
#' @return tibble con la sheet de fuentes clean de googledrive Argendata
#' @export
#'

fuentes_clean <- function() {
  
  filetemp <- list.files(tempdir(), full.names = T)[grepl("fuentes_clean", list.files(tempdir()))]
  
  if (length(filetemp) == 1) { 
    
    readr::read_csv(filetemp) %>% 
      suppressMessages()
    
  } else { 
    df <- googlesheets4::read_sheet(ss = fuentes_clean_sheet_id())
    
    df %>% 
      readr::write_csv(tempfile("fuentes_clean", fileext = ".csv")) %>% 
      suppressMessages()
    
    df
  }
  
  
  
}
