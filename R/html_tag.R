#' @importFrom htmltools br
#' @export
html_br <- function() {
  htmltools::br() %>% 
    as.character() %>% 
    cat()
}  