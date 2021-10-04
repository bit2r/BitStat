#' Convert one dimension data to integer
#' @param x factor, character. data to be changed.
#' @return integer
#' 
#' @examples
#' as_integer(pi)
#' as_integer(letters)
#'
#' @import dplyr
#' @export
as_integer <- function(x) {
  if (is.factor(x)) {
    int <- levels(x) %>% 
      .[x] %>% 
      as.integer()
    
    if (length(na.omit(int)) == 0) 
      as.integer(x) 
    else 
      int
  } else if (is.character(x)) {
    int <- as.integer(x)
    
    if (length(na.omit(int)) == 0) 
      as_integer(as.factor(x)) 
    else 
      int
  } else {
    as.integer(x)
  }
}


#' Convert one dimension data to numeric
#' @param x integer, factor, character. data to be changed.
#' @return numeric
#' 
#' @examples
#' as_numeric(3L)
#' as_numeric("3.14")
#' 
#' @import dplyr
#' @export
as_numeric <- function(x) {
  if (is.factor(x)) {
    num <- levels(x) %>% 
      .[x] %>% 
      as.numeric()
    
    if (length(na.omit(num)) == 0) 
      as.numeric(x) 
    else 
      num
  } else if (is.character(x)) {
    num <- as.numeric(x)
    
    if (length(na.omit(num)) == 0) 
      as_numeric(as.factor(x)) 
    else 
      num
  } else {
    as.numeric(x)
  }
}


#' Convert one dimension data to factor
#' @param x date, numeric, integer. data to be changed.
#' @param ordered logical. Whether the factor to be generated is 
#' an ordered factor.
#' @return factor
#' 
#' @examples
#' as_factor(letters)
#' 
#' @export
as_factor <- function(x, ordered = FALSE)  {
  factor(x, ordered = ordered)
}


#' Convert one dimension data to character
#' @param x date, numeric, integer, factor. data to be changed.
#' @return character
#' 
#' @examples
#' as_character(pi)
#' as_character(date())
#' 
#' @export
as_character <- function(x)  {
  as.character(x)
}  


#' Convert year-month-day format factor/character to date
#' @param x character or factor. data to be changed.
#' @return Date class
#' 
#' @examples
#' as_date("2004-4-11")
#'
#' @import dplyr
#' @importFrom lubridate ymd
#' @export
as_date <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }
  
  lubridate::ymd(x) %>% 
    as.Date()
}


#' @export
refactor <- function (x, target_levels, replce_levels) {
  x <- x %>% 
    as.character() %>% 
    ifelse(!. %in% target_levels, replce_levels, .)
  
  factor(x, levels = unique(x))
}
