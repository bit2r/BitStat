#' @importFrom rlang sym
#' @export
translate <- function(msg, msg_language = "kr", 
                      msg_token = NULL, token = NULL) {
  translation <- get("translation", envir = .BitStatEnv)
  language <- get("language", envir = .BitStatEnv)
    
  if (!is.null(msg_token)) {
    msg_token <- msg_token %>% 
    strsplit(",") %>% 
      unlist()
    
    msg_token %>% 
      seq() %>% 
      purrr::walk(
        function(x) {
          translation <<- translation %>% 
            mutate(!!msg_language := stringr::str_replace(
              !!rlang::sym(msg_language), 
              paste0("@", x), 
              msg_token[x]))
        }
      )
    
    if (is.null(token)) {
      token <- msg_token
    } else {
      token <- token %>% 
        strsplit(",") %>% 
        unlist() 
    }
    
    if (length(msg_token) != length(token))
      stop("different length msg_token and alt_token")
    
    token %>% 
      seq() %>% 
      purrr::walk(
        function(x) {
          translation <<- translation %>% 
            mutate(!!language := stringr::str_replace(
              !!rlang::sym(language), 
              paste0("@", x), 
              token[x]))
        }
      )    
  } 
  
  translation %>% 
    filter(!!rlang::sym(msg_language) %in% msg) %>% 
    select_at(vars(all_of(language))) %>% 
    pull()  
}
