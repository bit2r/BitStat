
#' @export
asis_cor_test <- function (x, digits = getOption("digits"), ...) 
{
  # cat("<br>")
  htmltools::span(x$method, style = "margin-left: 40px; font-size:15px; color:#6495ED;") %>% 
    as.character() %>% 
    cat("<br>", sep = "")
  cat("<br>")
  
  cat("<ul>")
  
  vars <- stringr::str_replace(x$data.name, " and", ",")
  glue::glue("{translate('데이터')}: {vars}") %>% 
    htmltools::tags$li() %>% 
    as.character() %>% 
    cat()

  out <- character()
  if (!is.null(x$statistic)) 
    out <- c(out, paste(names(x$statistic), "=", format(x$statistic, 
                                                        digits = max(1L, digits - 2L))))
  if (!is.null(x$parameter)) 
    out <- c(out, paste(names(x$parameter), "=", format(x$parameter, 
                                                        digits = max(1L, digits - 2L))))
  if (!is.null(x$p.value)) {
    fp <- format.pval(x$p.value, digits = max(1L, digits - 
                                                3L))
    out <- c(out, paste("p-value", if (startsWith(fp, "<")) fp else paste("=", 
                                                                          fp)))
  }
  
  glue::glue("{translate('통계량')}: {strwrap(paste(out, collapse = ', '))}") %>% 
    htmltools::tags$li() %>% 
    as.character() %>% 
    cat()
  
  if (!is.null(x$alternative)) {
    if (!is.null(x$null.value)) {
      if (length(x$null.value) == 1L) {
        stat_name <- substring(names(x$null.value), 1, 3)
        alt.char <- switch(x$alternative, two.sided = "not equal to", 
                           less = "less than", greater = "greater than")
        hypothesis <- glue::glue("true {stat_name} is {alt.char} {x$null.value}")
      }
      else {
        hypothesis <- glue::glue("{x$alternative} null values:")
      }
    }
    else hypothesis <- glue::glue("{x$alternative}")
  }
  
  glue::glue("{translate('대립가설')}: {translate(hypothesis, 'en', stat_name)}") %>% 
    htmltools::tags$li() %>% 
    as.character() %>% 
    cat()
  
  if (!is.null(x$conf.int)) {
    pct <- format(100 * attr(x$conf.int, 'conf.level'))
    confidence_interval <- paste(format(x$conf.int[1:2], digits = digits), collapse = ",")
    
    glue::glue("{pct} {translate('퍼센트 신뢰구간')}: [{confidence_interval}]") %>% 
      htmltools::tags$li() %>% 
      as.character() %>% 
      cat()
  }

  if (!is.null(x$estimate)) {
    estimate <- format(x$estimate, digits = digits)
    glue::glue("{translate('표본 추정치')}({names(estimate)}): {estimate}") %>% 
      htmltools::tags$li() %>% 
      as.character() %>% 
      cat()
  }

  cat("</ul>")
}
