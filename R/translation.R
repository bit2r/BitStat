#' @export
translate <- function(msg) {
  translation <- get("translation", envir = .BitStatEnv)
  language <- get("language", envir = .BitStatEnv)
  
  translation %>% 
    filter(kr %in% msg) %>% 
    select_at(vars(language)) %>% 
    pull()
}