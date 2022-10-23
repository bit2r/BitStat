################################################################################
## 01. Prepare Resources
################################################################################

##==============================================================================
## 01.01. Load Packages
##==============================================================================

##------------------------------------------------------------------------------
## 01.01.01. Set the library paths
##------------------------------------------------------------------------------

##------------------------------------------------------------------------------
## 01.01.02. Load packages that are related shiny & html
##------------------------------------------------------------------------------
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinybusy)
library(colourpicker)
library(htmltools)
library(flextable)

##------------------------------------------------------------------------------
## 01.01.03. Load packages that are tidyverse families
##------------------------------------------------------------------------------
library(dplyr)
library(readr)
library(vroom)
library(reactable)
library(glue)
library(dlookr)
library(openxlsx)


##==============================================================================
## 01.02. Loading Sources
##==============================================================================
#source("html_css.R")



################################################################################
## 02. Prepare Data and Meta
################################################################################
##==============================================================================
## 02.01. Global Options
##==============================================================================
## for upload file
options(shiny.maxRequestSize = 30 * 1024 ^ 2)
## for trace, if want.
options(shiny.trace = FALSE)
## for progress
options(spinner.color="#0275D8", spinner.color.background="#ffffff",
        spinner.size=2)


##==============================================================================
## 02.02. Meta data
##==============================================================================
assign("import_rds", NULL, envir = .BitStatEnv)
assign("list_datasets", readRDS(paste("www", "meta", "list_datasets.rds",
                                      sep = "/")), envir = .BitStatEnv)
assign("choosed_dataset", NULL, envir = .BitStatEnv)
assign("trans", NULL, envir = .BitStatEnv)


##==============================================================================
## 02.03. Translation meta
##==============================================================================
## set language
# i18n <- Translator$new(translation_csvs_path = "www/meta/translation")
# i18n$set_translation_language(get("language", envir = .BitStatEnv))


##==============================================================================
## 02.04. Widget meta
##==============================================================================
element_sep <- c(",", ";", "\t")
names(element_sep) <- c(translate("컴마"), translate("세미콜론"), translate("탭"))

element_quote <- c("", '"', "'")
names(element_quote) <- c(translate("없음"), translate("큰 따옴표"), 
                          translate("작은 따옴표"))

element_diag <- list("1", "2", "3")
names(element_diag) <- c(translate("결측치"), translate("음수값"), translate("0값"))

element_manipulate_variables <- list("Rename", "Change type", "Remove",
                                     "Reorder levels", "Reorganize levels", 
                                     "Transform", "Bin")
names(element_manipulate_variables) <- c(translate("이름 변경"), 
                                         translate("형 변환"), 
                                         translate("변수 삭제"),
                                         translate("범주 레벨 순서변경"),
                                         translate("범주 레벨 변경/병합"),
                                         translate("변수변환"),
                                         translate("비닝"))

element_change_type <- list("as_factor", "as_numeric", "as_integer", 
                            "as_character", "as_date")
names(element_change_type) <- c(translate("범주형으로"), translate("연속형으로"), 
                                translate("정수형으로"), translate("문자형으로"), 
                                translate("날짜(Y-M-D)로"))

## 통계량 종류
element_statistics <- list(
  "n", "na", "mean", "sd", "se_mean", "IQR", "skewness", "kurtosis"
)
names(element_statistics) <- c(
  translate("관측치수"),
  translate("결측치수"), 
  translate("산술평균"),
  translate("표준편차"),
  translate("표준오차"),
  translate("사분위수범위"),
  translate("왜도"),
  translate("첨도")
)

## 분위수 종류
element_quantiles <- list(
  "p00", "p01", "p05", "p10", "p20", "p25", "p30", "p40", "p50", 
  "p60", "p70", "p75", "p80", "p90", "p95", "p99", "p100"
)
names(element_quantiles) <- c(
  translate("최솟값"), translate("1%분위"), translate("5%분위"), 
  translate("10%분위"), translate("20%분위"), translate("1/4분위"), 
  translate("30%분위"), translate("40%분위"), translate("중위수"), 
  translate("60%분위"), translate("70%분위"), translate("3/4분위"), 
  translate("80%분위"), translate("90%분위"), translate("95%분위"), 
  translate("99%분위"), translate("최댓값")
)

## 대상변수 선택 방법
element_method_choose_variables <- list("all", "user")
names(element_method_choose_variables) <- c(
  translate("전체"), 
  translate("사용자 선택")
)

## 주변 합 종류
element_marginal_type <- list("sum", "pct_row", "pct_col", "pct_tot")
names(element_marginal_type) <- c(
  translate("주변 합"), 
  translate("행 백분율"),
  translate("열 백분율"), 
  translate("전체 백분율")
)

## 상관계수 종류
element_corr_method <- list(
  "pearson", "kendall","spearman"
)
names(element_corr_method) <- c(
  translate("피어슨의 적률 상관계수"), 
  translate("켄달의 순위 상관계수"),
  translate("스피어만의 순위 상관계수")
)

## 상관검정의 대립가설
element_alternative_test <- list(
  "two.sided", "less", "greater"
)
names(element_alternative_test) <- c(
  translate("상관계수 ≠ 0"), 
  translate("상관계수 < 0"),
  translate("상관계수 > 0")
)

## load source for tools
for (file in list.files(c("tools"), pattern = "\\.(r|R)$", full.names = TRUE)) {
  source(file, local = TRUE)
}



################################################################################
## 06. Shiny Rendering for CentOS
################################################################################
##==============================================================================
## 06.01. Shiny visualization functions
##==============================================================================

##------------------------------------------------------------------------------
## 06.01.01. Plot vis to PNG file for shiny server
##------------------------------------------------------------------------------
plotPNG <- function (func, filename = tempfile(fileext = ".png"), width = 400,
                     height = 400, res = 72, ...)  {
  if (capabilities("aqua")) {
    pngfun <- grDevices::png
  }
  else if (FALSE && nchar(system.file(package = "Cairo"))) {
    pngfun <- Cairo::CairoPNG
  }
  else {
    pngfun <- grDevices::png
  }
  
  pngfun(filename = filename, width = width, height = height, res = res, ...)
  
  op <- graphics::par(mar = rep(0, 4))
  
  tryCatch(graphics::plot.new(), finally = graphics::par(op))
  
  dv <- grDevices::dev.cur()
  
  on.exit(grDevices::dev.off(dv), add = TRUE)
  
  func()
  
  filename
}

##------------------------------------------------------------------------------
## 06.01.02. Rendering for shiny server
##------------------------------------------------------------------------------
renderPlot <- function (expr, width = "auto", height = "auto", res = 72, ...,
                        env = parent.frame(), quoted = FALSE, func = NULL)  {
  installExprFunction(expr, "func", env, quoted, ..stacktraceon = TRUE)
  
  args <- list(...)
  
  if (is.function(width))
    widthWrapper <- reactive({
      width()
    })
  else widthWrapper <- NULL
  
  if (is.function(height))
    heightWrapper <- reactive({
      height()
    })
  else heightWrapper <- NULL
  
  outputFunc <- plotOutput
  
  if (!identical(height, "auto"))
    formals(outputFunc)["height"] <- list(NULL)
  
  return(markRenderFunction(outputFunc, function(shinysession,
                                                 name, ...) {
    if (!is.null(widthWrapper)) width <- widthWrapper()
    if (!is.null(heightWrapper)) height <- heightWrapper()
    
    prefix <- "output_"
    
    if (width == "auto")
      width <- shinysession$clientData[[paste(prefix, name,
                                              "_width", sep = "")]]
    if (height == "auto")
      height <- shinysession$clientData[[paste(prefix, name,
                                               "_height", sep = "")]]
    if (is.null(width) || is.null(height) || width <= 0 || height <= 0)
      return(NULL)
    pixelratio <- shinysession$clientData$pixelratio
    if (is.null(pixelratio))
      pixelratio <- 1
    
    coordmap <- NULL
    
    plotFunc <- function() {
      result <- withVisible(func())
      coordmap <<- NULL
      if (result$visible) {
        if (inherits(result$value, "ggplot")) {
          utils::capture.output(coordmap <<- getGgplotCoordmap(result$value,
                                                               pixelratio))
        } else {
          utils::capture.output(..stacktraceon..(print(result$value)))
        }
      }
      if (is.null(coordmap)) {
        coordmap <<- shiny:::getPrevPlotCoordmap(width, height)
      }
    }
    
    outfile <- ..stacktraceoff..(
      do.call(
        plotPNG,
        c(plotFunc, width = width * pixelratio, height = height * pixelratio,
          res = res * pixelratio, args)
      )
    )
    
    on.exit(unlink(outfile))
    res <- list(src = shinysession$fileUrl(name, outfile,
                                           contentType = "image/png"),
                width = width, height = height,
                coordmap = coordmap)
    error <- attr(coordmap, "error", exact = TRUE)
    if (!is.null(error)) {
      res$error <- error
    }
    
    res
  }))
}
