#' @importFrom readr read_csv locale
.onLoad <- function(libname, pkgname) {
  .BitStatEnv <- new.env()
  assign("language", "kr", envir = .BitStatEnv)
  
  trans_file <- "translation.csv"
  trans_csv <- file.path(system.file(package = "BitStat"), 
                         "translation", trans_file)
  
  translation <- readr::read_csv(
    trans_csv, 
    col_types = "cc",
    locale = readr::locale(encoding = "UTF-8")
  ) %>% 
    suppressWarnings()
  
  assign("translation", translation, envir = .BitStatEnv)
  
  assign(".BitStatEnv", .BitStatEnv, envir = .GlobalEnv)
}