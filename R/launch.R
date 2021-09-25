#' Launch BitStat apps
#'
#' @details See \url{https://r2bit.github.io/BitStat/docs} for radiant documentation and tutorials
#'
#' @param language character. app language. support Korean and English.
#' @param ... additional arguments to pass to shiny::runApp (e.g, port = 8080)
#'
#' @importFrom shiny runApp
#'
#' @examples
#' \dontrun{
#' launch()
#' }
#'
#' @export
launch <- function(language = c("kr", "en")[1], ...) {
  assign("language", language, envir = .BitStatEnv)
  
  suppressMessages(
    shiny::runApp(system.file("app", package = "BitStat"), ...)
  )
}
