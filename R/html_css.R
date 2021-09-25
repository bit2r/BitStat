#' @importFrom glue glue
#' @importFrom stringr str_replace
#' @export
size_margin <- function(top = NULL, right = NULL, bottom = NULL, left = NULL) {
  if (!is.null(top))
    top <- glue::glue(" margin-top: {top}px;")
  
  if (!is.null(right))
    right <- glue::glue(" margin-right: {right}px;")
  
  if (!is.null(bottom))
    bottom <- glue::glue(" margin-bottom: {bottom}px;")
  
  if (!is.null(left))
    left <- glue::glue(" margin-left: {left}px;")  
  
  paste0(top, right, bottom, left) %>% 
    stringr::str_replace("^ ", "")
}  


#' @importFrom glue glue
#' @importFrom stringr str_replace
#' @export
size_padding <- function(top = NULL, right = NULL, bottom = NULL, left = NULL) {
  if (!is.null(top))
    top <- glue::glue(" padding-top: {top}px;")
  
  if (!is.null(right))
    right <- glue::glue(" padding-right: {right}px;")
  
  if (!is.null(bottom))
    bottom <- glue::glue(" padding-bottom: {bottom}px;")
  
  if (!is.null(left))
    left <- glue::glue(" padding-left: {left}px;")  
  
  paste0(top, right, bottom, left) %>% 
    stringr::str_replace("^ ", "")
}  

