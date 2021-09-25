.onLoad <- function(libname, pkgname) {
  .BitStatEnv <- new.env()
  assign("language", "kr", envir = .BitStatEnv)
  
  assign(".BitStatEnv", .BitStatEnv, envir = .GlobalEnv)
}