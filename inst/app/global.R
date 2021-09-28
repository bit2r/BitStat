################################################################################
## 01. Prepare Resources
################################################################################

##==============================================================================
## 01.01. Load Packages
##==============================================================================

##------------------------------------------------------------------------------
## 01.01.01. Set the library paths
##------------------------------------------------------------------------------
.libPaths(c("/hli_appl/home/has01/R/x86_64-pc-linux-gnu-library/3.3",
            "/hli_appl/appl/bda/R/x86_64-pc-linux-gnu-library/3.3",
            "/opt/microsoft/ropen/3.4.1/lib64/R/library",
            "/hli_appl/appl/bda/R/oracle"))

##------------------------------------------------------------------------------
## 01.01.02. Load packages that are related shiny & html
##------------------------------------------------------------------------------
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(colourpicker)
library(shinybusy)
library(shinythemes)
library(shiny.i18n)
library(htmltools)

##------------------------------------------------------------------------------
## 01.01.03. Load packages that are tidyverse families
##------------------------------------------------------------------------------
library(dplyr)
library(readr)
library(vroom)
library(reactable)
library(glue)
library(dlookr)
library(xlsx)
library(flextable)

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
i18n <- Translator$new(translation_csvs_path = "www/meta/translation")
i18n$set_translation_language(get("language", envir = .BitStatEnv))


##==============================================================================
## 02.04. Widget meta
##==============================================================================
element_sep <- c(",", ";", "\t")
names(element_sep) <- c(i18n$t("컴마"), i18n$t("세미콜론"), i18n$t("탭"))

element_quote <- c("", '"', "'")
names(element_quote) <- c(i18n$t("없음"), i18n$t("큰 따옴표"), 
                          i18n$t("작은 따옴표"))

element_diag <- list("1", "2", "3")
names(element_diag) <- c(i18n$t("결측치"), i18n$t("음수값"), i18n$t("0값"))

element_manipulate_variables <- list("Rename", "Change type", "Remove",
                                     "Reorder levels", "Transform", "Bin")
names(element_manipulate_variables) <- c(i18n$t("이름 변경"), 
                                         i18n$t("형 변환"), 
                                         i18n$t("변수 삭제"),
                                         i18n$t("범주 순서변경"),
                                         i18n$t("변수변환"),
                                         i18n$t("비닝"))

element_change_type <- list("as_factor", "as_numeric", "as_integer", 
                            "as_character", "as_date")
names(element_change_type) <- c(i18n$t("범주형으로"), i18n$t("연속형으로"), 
                                i18n$t("정수형으로"), i18n$t("문자형으로"), 
                                i18n$t("날짜(Y-M-D)로"))


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


