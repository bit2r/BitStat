################################################################################
## 01. 보고서 > 데이터 진단
################################################################################
##==============================================================================
## 01.01. 보고서 > 데이터 진단 > 진단 개요
##==============================================================================
##------------------------------------------------------------------------------
## 01.01.01. 보고서 > 데이터 진단 > 진단 개요 UI 정의
##------------------------------------------------------------------------------

# 데이터 진단 개요 출력 --------------------------------------------------------
output$list_warnings <- renderReactable({
  req(input$combo_dataset)
  
  id_dataset <- input$combo_dataset
  
  tab_warning <- dslists()[[id_dataset]]$dataset %>%
    diagnose_warning()
  
  list_diagnose_warning(tab_warning)
})


# 데이터 진단 목록 출력 --------------------------------------------------------
output$detail_warnings <- renderReactable({
  req(input$combo_dataset)
  
  id_dataset <- input$combo_dataset
  
  tab_warning <- dslists()[[id_dataset]]$dataset %>%
    diagnose_warning()
  
  detail_diagnose_warning(tab_warning)
})


# 진단 개요 UI 정의 ------------------------------------------------------------
output$overview_diagnose <- renderUI({
  tagList(
    fluidRow(
      style = "padding-top:10px;padding-bottom:0px",
      useShinyjs(),
      column(
        width = 12,
        wellPanel(
          style = "padding-top:5px;padding-bottom:10px",
          h4(translate("데이터 진단 개요")),
          
          div(style="width: 300px;padding-top:5px;padding-bottom:10px",
              strong(translate("데이터 진단 집계:")),
              reactableOutput("list_warnings", width = "100%")
          ),
          div(style="width: 800px;padding-top:5px;padding-bottom:10px",
              strong(translate("데이터 진단 상세:")),
              reactableOutput("detail_warnings", width = "100%")
          )
        )
      )
    )
  )
})  
 

##==============================================================================
## 01.02. 보고서 > 데이터 진단 > 변수별 진단
##==============================================================================
##------------------------------------------------------------------------------
## 01.02.01. 보고서 > 데이터 진단 > 변수별 진단 UI 정의
##------------------------------------------------------------------------------

# 변수별 진단 출력 -------------------------------------------------------------
output$variable_warnings <- renderReactable({
  req(input$combo_dataset)
  
  id_dataset <- input$combo_dataset
  
  dslists()[[id_dataset]]$dataset %>%
    dlookr:::html_variable(theme = "blue", base_family = "NanumSquare")
})


# 변수별 진단 UI 정의 ----------------------------------------------------------
output$variable_diagnose <- renderUI({
  tagList(
    fluidRow(
      style = "padding-top:10px;padding-bottom:0px",
      useShinyjs(),
      column(
        width = 12,
        wellPanel(
          style = "padding-top:5px;padding-bottom:10px",
          h4(translate("변수별 데이터 진단")),
          div(style = "padding-top:5px;padding-bottom:10px",
              reactableOutput("variable_warnings", width = "100%")
          )
        )
      )
    )
  )
})


##==============================================================================
## 01.03. 보고서 > 데이터 진단 > 이상치(Outliers)
##==============================================================================
##------------------------------------------------------------------------------
## 01.03.01. 보고서 > 데이터 진단 > 이상치(Outliers) UI 정의
##------------------------------------------------------------------------------

# 이상치 출력 ------------------------------------------------------------------
output$variable_outliers <- renderReactable({
  req(input$combo_dataset)
  
  id_dataset <- input$combo_dataset
  
  dslists()[[id_dataset]]$dataset %>%
    dlookr:::html_outlier(theme = "blue", base_family = "NanumSquare")
})


# 이상치(Outliers) UI 정의 -----------------------------------------------------
output$variable_outlier <- renderUI({
  tagList(
    fluidRow(
      style = "padding-top:10px;padding-bottom:0px",
      useShinyjs(),
      column(
        width = 12,
        wellPanel(
          style = "padding-top:5px;padding-bottom:10px",
          h4(translate("변수별 이상치 진단")),
          div(style="padding-top:5px;padding-bottom:10px",
              reactableOutput("variable_outliers", width = "100%")
          )
        )
      )
    )
  )  
})



##==============================================================================
## 01.04. 보고서 > 데이터 진단 > 보고서 출력
##==============================================================================
##------------------------------------------------------------------------------
## 01.04.01. 보고서 > 데이터 진단 > 보고서 출력 UI 정의
##------------------------------------------------------------------------------

# 보고서 출력 UI 정의 ----------------------------------------------------------
output$pdf_diagnose <- renderUI({
  req(input$combo_dataset)
  
  id_dataset <- input$combo_dataset
  dataset_name <- dslists()[[id_dataset]]$dataset_name
  
  tagList(
    fluidRow(
      style = "padding-top:10px;padding-bottom:0px",
      useShinyjs(),
      column(
        width = 4,
        wellPanel(
          style = "padding-top:5px;padding-bottom:75px !important;",
          h4(translate("보고서 커버 설정")),
          style = "padding-top:0px;",
          div(style="display: inline-block;vertical-align:top; width: 100%;",
              textInput("title_diag", label = translate("보고서 제목:"),
                        value = translate("데이터 진단 보고서"))),
          div(style="display: inline-block;vertical-align:top; width: 100%;",
              textInput("subtitle_diag", label = translate("보고서 부제목:"),
                        value = dataset_name)),
          div(style="display: inline-block;vertical-align:top; width: 50%",
              colourInput("title_col_diag", translate("보고서 제목 색상:"), "white",
                          returnName = TRUE, palette = "limited",
                          closeOnClick = TRUE)),
          div(style="display: inline-block;vertical-align:top; width: 50%;",
              colourInput("subtitle_col_diag", translate("보고서 부제목 색상:"),
                          "sandybrown", returnName = TRUE, palette = "limited",
                          closeOnClick = TRUE))
        )
      ),
      
      column(
        width = 4,
        wellPanel(
          style = "padding-top:5px;padding-bottom:10px !important;",
          h4(translate("보고서 파라미터 설정")),
          style = "padding-top:0px;",
          div(
            style="display: inline-block;vertical-align:top; width: 100%;",
            sliderInput(
              inputId = "thres_uniq_cat",
              label = translate("범주형 변수의 유일값 비율 상한 임계치 (0 ~ 1]:"),
              min = 0, max = 1, value = 0.5
            )
          ),
          div(
            style="display: inline-block;vertical-align:top; width: 100%;",
            sliderInput(
              inputId = "thres_uniq_num",
              label = translate("수치형 변수의 유일값 개수 하한 임계치 (0 ~ 10]:"),
              min = 0, max = 10, value = 5
            )
          ),
          div(
            style="display: inline-block;vertical-align:top; width: 100%;",
            checkboxGroupInput(
              inputId = "flag_diag",
              label = translate("부가 진단 항목:"), inline = TRUE,
              choices = element_diag,
              selected = c(1, 2, 3)
            )
          ),
          div(
            style="display: inline-block;vertical-align:top; width: 100%;",
            sliderInput(
              inputId = "sample_percent_diag",
              label = translate("보고서 작성에 사용할 데이터 백분율 (5 ~ 100%]:"),
              min = 5, max = 100, value = 100
            )
          )
        )
      ),
      column(
        width = 4,
        wellPanel(
          style = "padding-top:5px;padding-bottom:35px !important;",
          h4(translate("보고서 출력 설정")),
          style = "padding-top:0px;",
          div(
            style = "display: inline-block;vertical-align:top; width: 100%;",
            selectInput(
              inputId = "format_diag", 
              label = translate("출력 포맷:"),
              choices = c("pdf", "html"),
              selected = "pdf"
            )
          ),
          div(
            style = "display: inline-block;vertical-align:top; width: 100%;",
            selectInput(
              inputId = "theme_color_diag", 
              label = translate("색상 테마:"),
              choices = c("blue", "orange"),
              selected = "blue"
            )
          ),
          div(
            style = "display: inline-block;vertical-align:top; width: 100%;",
            textInput(
              inputId = "author_diag", 
              label = translate("보고서 작성자:"),
              value = "dlookr")),
          div(
            style = "display: inline-block;vertical-align:top; width: 100%;",
            textInput(
              inputId = "file_diag", 
              label = translate("파일 이름:"), 
              value = paste0("diag_", dataset_name)
            )
          ),
          actionButton(
            inputId = "printDiagnose", 
            label = translate("보고서 출력"), 
            icon = icon("print"),
            style = "background-color: #90CAF9; border: none;"
          )
        )
      )
    )
  )  
})


##------------------------------------------------------------------------------
## 01.04.02. 보고서 > 데이터 진단 > 보고서 출력 이벤트 정의
##------------------------------------------------------------------------------

# 데이터셋 변경 이벤트  --------------------------------------------------------
observeEvent(input$combo_dataset, {
  req(input$combo_dataset)

  id_dataset <- input$combo_dataset
  dataset_name <- dslists()[[id_dataset]]$dataset_name

  updateTextInput(session, "subtitle_diag", value = dataset_name)
  updateTextInput(session, "file_diag", value = paste0("diag_", dataset_name))
})


# 출력포맷 변경 이벤트  --------------------------------------------------------
observeEvent(input$format_diag, {
  if (input$format_diag %in% "html") {
    updateColourInput(session, "title_col_diag", value = "gray50")
  } else{
    updateColourInput(session, "title_col_diag", value = "white")
  }
})


# 보고서 출력 ------------------------------------------------------------------
observeEvent(input$printDiagnose, {
  req(input$combo_dataset)
  req(input$title_diag)
  req(input$subtitle_diag)
  req(input$author_diag)
  
  id_dataset <- input$combo_dataset
  dataset_name <- dslists()[[id_dataset]]$dataset_name
  
  title <- input$title_diag
  subtitle <- input$subtitle_diag
  abstract <- glue::glue("이 보고서는 {dataset_name}의 데이터 품질 진단을 위해 작성되었습니다. 탐색적 데이터 분석(EDA, 기술통계)를 수행하기 전, 개별 변수들의 유효성을 판단하기 위해 작성되었습니다.")
  
  title_col <- input$title_col_diag
  subtitle_col <- input$subtitle_col_diag
  
  format <- input$format_diag
  theme <- input$theme_color_diag
  
  thres_uniq_cat <- input$thres_uniq_cat
  thres_uniq_num <- input$thres_uniq_num
  
  flag_diag <- input$flag_diag
  
  flag_content_missing <- FALSE
  flag_content_minus <- FALSE
  flag_content_zero <- FALSE
  
  if ("1" %in% flag_diag) {
    flag_content_missing <- TRUE
  }
  if ("2" %in% flag_diag) {
    flag_content_minus <- TRUE
  }
  if ("3" %in% flag_diag) {
    flag_content_zero <- TRUE
  }
  
  sample_percent <- input$sample_percent_diag
  author <- input$author_diag
  
  output_file <- glue::glue("{input$file_diag}.{format}")
  
  if (format %in% "pdf") {
    dslists()[[id_dataset]]$dataset %>%
      dlookr::diagnose_paged_report(
        output_format = format,
        title = title,
        subtitle = subtitle,
        title_color = title_col,
        abstract_title = "보고서 개요",
        abstract = abstract,
        title_col = title_col,
        subtitle_color = subtitle_col,
        thres_uniq_cat = thres_uniq_cat,
        thres_uniq_num = thres_uniq_num,
        flag_content_zero = flag_content_zero,
        flag_content_minus = flag_content_minus,
        flag_content_missing = flag_content_missing,
        sample_percent = sample_percent,
        theme = theme,
        author = author,
        output_file = output_file,
        base_family = "NanumSquare"
      )
  } else {
    dslists()[[id_dataset]]$dataset %>%
      dlookr::diagnose_web_report(
        output_format = format,
        browse = FALSE,
        title = title,
        subtitle = subtitle,
        title_color = title_col,
        title_col = title_col,
        thres_uniq_cat = thres_uniq_cat,
        thres_uniq_num = thres_uniq_num,
        flag_content_zero = flag_content_zero,
        flag_content_minus = flag_content_minus,
        flag_content_missing = flag_content_missing,
        sample_percent = sample_percent,
        theme = theme,
        author = author,
        # output_dir = "./",
        output_file = output_file,
        base_family = "NanumSquare"
      )
    
    # browseURL(paste(".", output_file, sep = "/"))
  }
  
  # Change for server side excute
  file.copy(paste(tempdir(), output_file, sep = "/"),
            paste("www", output_file, sep = "/"),
            overwrite = TRUE)
  URL <- paste(".", output_file, sep = "/")  
  
  js$browseURL(URL)
})



################################################################################
## 04. 데이터 메뉴 정의
################################################################################
output$ui_report_data <- renderUI({
  tagList(
    tabBox(
      width = 12,
      tabPanel(
        title = translate("데이터 진단"),
        tabsetPanel(
          tabPanel(
            title = translate("진단 개요"),
            uiOutput("overview_diagnose"),
            icon = shiny::icon("stethoscope")
          ),
          tabPanel(
            title = translate("변수별 진단"),
            uiOutput("variable_diagnose"),
            icon = shiny::icon("list-ul")
          ),
          tabPanel(
            title = translate("이상치"),
            uiOutput("variable_outlier"),
            icon = shiny::icon("balance-scale-left")
          ),
          tabPanel(
            title = translate("보고서 작성"),
            uiOutput("pdf_diagnose"),
            icon = shiny::icon("file-pdf")
          )
        )
      )
    ) 
  )  
})

