################################################################################
## 01. 데이터 > 데이터 준비
################################################################################
##==============================================================================
## 01.01. 데이터 > 데이터 준비 > 데이터 업로드
##==============================================================================
##------------------------------------------------------------------------------
## 01.01.01. 데이터 > 데이터 준비 > 데이터 업로드 UI 정의
##------------------------------------------------------------------------------

# fileInput 선택적 정의  -------------------------------------------------------
output$data_file <- renderUI({
  input$file_format
  
  if (input$file_format == 'csv') {
    fileInput(
      inputId = "data_file", 
      label = NULL,  
      buttonLabel = i18n$t("파일선택"),
      multiple = FALSE,
      accept = c(
        "text/csv",
        "text/comma-separated-values,text/plain",
        ".csv"
      )
    )
  } else if (input$file_format == 'xlsx') {
    fileInput(
      inputId = "data_file", 
      label = NULL,  
      buttonLabel = i18n$t("파일선택"),
      multiple = FALSE,
      accept = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )
  } else if (input$file_format == 'rds') {
    fileInput(
      inputId = "data_file", 
      label = NULL,  
      buttonLabel = i18n$t("파일선택"),
      multiple = FALSE, 
      accept = c(".rds")
    )
  }
})


# 샘플 데이터 미리보기 테이블 정의 ---------------------------------------------
output$file_contents <- renderReactable({
  req(input$data_file)
  
  if (is.null(import_file$upload_state) | import_file$upload_state == "reset") {
    return()
  }
  
  if (input$file_format == "csv") {
    encode <- readr::guess_encoding(input$data_file$datapath) %>%
      select(encoding) %>%
      pull() %>%
      "["(1)
    
    tryCatch({
      df <- vroom(
        input$data_file$datapath,
        col_names = input$header_csv,
        delim = input$sep,
        quote = input$quote,
        n_max = 10,
        locale = locale(encoding = encode)
      )
    },
    error = function(e) {
      stop(safeError(e))
    })
  } else if (input$file_format == "xlsx") {
    df <- xlsx::read.xlsx(
      input$data_file$datapath, endRow = 10,
      sheetIndex = input$sheet_index,
      header = input$header_xlsx
    )
  } else if (input$file_format == "rds") {
    df <- readr::read_rds(input$data_file$datapath)
    
    assign("import_rds", df, envir = .BitStatEnv)
  }
  
  updateNumericInput(session, "flag_upload", value = 1)
  
  df %>%
    filter(row_number() <= 10) %>%
    reactable(
      defaultColDef = colDef(minWidth = 150),
      sortable = FALSE,
      bordered = TRUE
    )
})

# 데이터 업로드 UI 정의 --------------------------------------------------------
output$upload_data <- renderUI({
  tagList(
    fluidRow(
      shinyjs::useShinyjs(),
      style = "padding-top:10px;padding-bottom:0px",
      column(
        width = 3,
        wellPanel(
          style = "padding-top:5px;padding-bottom:10px",
          h4(i18n$t("데이터 파일 선택")),
          div(style="display: inline-block;vertical-align:top;",
              radioGroupButtons(
                inputId = "file_format",
                label = i18n$t("파일 포맷:"),
                choices = c(`<i class="fas fa-file-csv"></i>` = "csv",
                            `<i class="fas fa-file-excel"></i>` = "xlsx",
                            `<i class="fab fa-r-project"></i>` = "rds"),
                justified = TRUE,
                width = 250
              )
          ),
          
          conditionalPanel(
            style = "padding-top:0px;",
            condition = "input.file_format == 'csv'",
            # Input: Checkbox if file has header ----
            strong(i18n$t("헤더 포함 여부:")),
            checkboxInput("header_csv", i18n$t("헤더 포함"), TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", strong(i18n$t("구분자:")),
                         choices = element_sep,
                         selected = ",",
                         inline = TRUE),
            # Input: Select quotes ----
            radioButtons("quote", strong(i18n$t("인용문자:")),
                         choices = element_quote,
                         selected = '"',
                         inline = TRUE),
            style = size_padding(bottom = 10)
          ),
          
          conditionalPanel(
            style = "padding-top:0px;",
            condition = "input.file_format == 'xlsx'",
            # Input: Checkbox if file has header ----
            strong(i18n$t("헤더 포함 여부:")),
            checkboxInput("header_xlsx", i18n$t("헤더 포함"), TRUE),
            numericInput("sheet_index", label = strong(i18n$t("대상 시트번호:")), 
                         value = 1, width = "200px"),
          ),
          
          div(style="display: inline-block;vertical-align:top; width: 300px;
              padding-top:5px;",
              shinyjs::hidden(
                numericInput("flag_upload", label = "", value = 0)
              ),
              uiOutput('data_file')
          ),
          
          conditionalPanel(
            style = "padding-top:0px;",
            condition = "input.flag_upload == 1",
            div(style="display: inline-block;vertical-align:top; width: 300px;",
                textInput("name_dataset", label = i18n$t("데이터셋 이름:"), value = "",
                          placeholder = i18n$t("생성할 데이터셋 이름을 입력하세요."))),
            div(style="display: inline-block;vertical-align:top; width: 300px;",
                textInput("desc_dataset", label = i18n$t("데이터셋 설명:"), value = "",
                          placeholder = i18n$t("생성할 데이터의 설명을 입력하세요."))),
            actionButton("save_data", label = i18n$t("저장"), icon = icon("save"))
          )
        )
      ),
      column(9,
             wellPanel(
               style = "padding-top:5px",
               h4(i18n$t("샘플 데이터 미리보기")),
               reactableOutput("file_contents")
             )
      )
    )  
  )
})


##------------------------------------------------------------------------------
## 01.01.02. 데이터 > 데이터 준비 > 데이터 업로드 이벤트 정의
##------------------------------------------------------------------------------

# 업로드 파일포맷 선택 ---------------------------------------------------------
observeEvent(input$file_format, {
  import_file$upload_state <- "reset"
  updateReactable("file_contents", data = data.frame())
  
  updateNumericInput(session, "flag_upload", value = 0)
})


# 파일 선택 ------------------------------------------------------------------
observeEvent(input$data_file, {
  import_file$upload_state <- "upload"
})


# 업로드 데이터 객체 저장 ----------------------------------------------------
observeEvent(input$save_data, {
  req(input$data_file)
  
  if (is.null(input$data_file)) {
    alert_message(session, type = "have", name = i18n$t("데이터셋"), coda = TRUE,
                  message = "업로드하지 않았습니다.")
    
    return()
  }
  
  
  if (input$name_dataset == "") {
    alert_message(session, type = "input", name = i18n$t("데이터셋 이름"), coda = TRUE)
    
    return()
  }
  
  if (input$desc_dataset == "") {
    alert_message(session, type = "input", name = i18n$t("데이터셋 설명"), coda = TRUE)
    
    return()
  }
  
  file <- input$data_file
  ext <- tools::file_ext(file$datapath)
  
  if (input$file_format %in% "csv") {
    encode <- readr::guess_encoding(input$data_file$datapath) %>%
      select(encoding) %>%
      pull() %>%
      "["(1)
    
    dataset <- vroom(
      input$data_file$datapath,
      col_names = input$header_csv,
      delim = input$sep,
      quote = input$quote,
      locale = locale(encoding = encode)
    ) %>%
      tibble::as_tibble()
  } else if (input$file_format %in% "xlsx") {
    dataset <- xlsx::read.xlsx(
      input$data_file$datapath,
      sheetIndex = input$sheet_index,
      header = input$header_xlsx
    )
  } else if (input$file_format %in% "rds") {
    dataset <- get("import_rds", envir = .BitStatEnv)
  }
  
  dataset_id <- paste0("dataset-", shiny:::createUniqueId(5))
  
  dataset_list <- get("list_datasets", envir = .BitStatEnv)
  
  dataset_list[[dataset_id]] <- list(
    dataset_id = dataset_id,
    dataset_name = input$name_dataset,
    dataset_desc = input$desc_dataset,
    n_observation = NROW(dataset),
    n_column = NCOL(dataset),
    dataset = dataset
  )
  
  assign("list_datasets", dataset_list, envir = .BitStatEnv)
  
  updateReactable("file_contents", data = data.frame())
  
  updateNumericInput(session, "rnd_dataset_list", value = sample(1:1000000, 1))
  updateNumericInput(session, "flag_upload", value = 0)
})


##==============================================================================
## 01.02. 데이터 > 데이터 준비 > 데이터셋 관리
##==============================================================================
##------------------------------------------------------------------------------
## 01.02.01. 데이터 > 데이터 준비 > 데이터셋 관리 UI 정의
##------------------------------------------------------------------------------

# 데이터셋 목록 출력 -----------------------------------------------------------
output$imported_ds_list <- renderReactable({
  input$rnd_dataset_list
  
  tabs <- dslists()
  
  tab_data_list(tabs)
})


# 데이터셋 관리 UI 정의  -------------------------------------------------------
output$manage_dataset <- renderUI({
  tagList(
    fluidRow(
      style = "padding-top:10px;padding-bottom:0px",
      useShinyjs(),
      column(
        width = 12, 
        wellPanel(
          style = "padding-top:5px;padding-bottom:10px",
          h4(i18n$t("데이터셋 목록")),
          reactableOutput("imported_ds_list", width = "100%"),
          shinyjs::hidden(
            numericInput("n_datasets_list", label = "", value = 0)
          ),
          
          conditionalPanel(
            style = "padding-top:10px;",
            condition = "input.n_datasets_list > -1",
            fluidRow(
              column(
                width = 4,
                actionButton("dropDatasetButton", i18n$t("데이터셋 삭제"),
                             icon = icon("eraser"),
                             style = "background-color: #90CAF9; border: none;"),
                actionButton("editDatasetButton", i18n$t("데이터셋 편집"),
                             icon = icon("edit"),
                             style = "background-color: #90CAF9; border: none;"),
                actionButton("downDatasetButton", i18n$t("데이터셋 받기"),
                             icon = icon("file-download"),
                             style = "background-color: #90CAF9; border: none;")
              ),
              column(
                width = 1,
                prettySwitch(inputId = "openDatasetDownload",
                             label = i18n$t("전체 다운로드"),
                             status = "success", fill = TRUE),
                style = "margin-top: 8px;"
              )
            )
          ),
          
          shinyjs::hidden(
            numericInput("editable_dataset", label = "", value = 0)
          ),
          
          conditionalPanel(
            style = "padding-top:10px;",
            condition = "input.editable_dataset == 1",
            fluidRow(
              column(
                width = 3,
                textInput("name_dataset_edit", label = i18n$t("데이터셋 이름:"),
                          value = "")
              ),
              column(
                width = 9,
                textInput("desc_dataset_edit", label = i18n$t("데이터셋 설명:"),
                          value = "")
              )
            ),
            actionButton("modifyDatasetButton", i18n$t("수정"), icon = icon("save"),
                         style = "background-color: #90CAF9; border: none;"),
            actionButton("cancelDatasetButton", i18n$t("취소"), 
                         icon = icon("window-close"),
                         style = "background-color: #90CAF9; border: none;")
          ),
          
          shinyjs::hidden(
            numericInput("downloadable_file", label = "", value = 0)
          ),
          
          conditionalPanel(
            style = "padding-top:10px;",
            condition = "input.downloadable_file == 1",
            fluidRow(
              column(
                width = 3,
                div(
                  style = "display: inline-block;vertical-align:top;",
                  radioGroupButtons(
                    inputId = "file_format_down",
                    label = i18n$t("파일 포맷:"),
                    choices = c(`<i class="fas fa-file-csv"></i>` = "csv",
                                `<i class="fas fa-file-excel"></i>` = "xlsx",
                                `<i class="fab fa-r-project"></i>` = "rds"),
                    justified = TRUE,
                    width = 250
                  )
                ),
                
                textInput("fname_d_file", i18n$t("파일이름:"), value = ""),
                downloadButton("downFileData", i18n$t("파일받기"),
                               style = "background-color: #90CAF9;border: none;"),
                actionButton("cancelFileButton", i18n$t("취소"), 
                             icon = icon("window-close"),
                             style = "background-color: #90CAF9; border: none;")
              )
            )
          ),
          
          shinyjs::hidden(
            numericInput("downloadable_dataset", label = "", value = 0)
          ),
          
          conditionalPanel(
            style = "padding-top:10px;",
            condition = "input.downloadable_dataset == 1",
            fluidRow(
              column(
                width = 3,
                textInput("fname_d_dataset", i18n$t("파일이름:"), value = ""),
                downloadButton("downDatasetData", i18n$t("파일받기"),
                               style = "background-color: #90CAF9;border: none;")
              )
            )
          )
        )
      )
    )
  )
})  


##------------------------------------------------------------------------------
## 01.02.02. 데이터 > 데이터 준비 > 데이터셋 관리 이벤트 정의
##------------------------------------------------------------------------------

# 데이터셋 제거 이벤트 ---------------------------------------------------------
selected_dataset_list <- reactive(getReactableState("imported_ds_list",
                                                    "selected"))

observeEvent(input$dropDatasetButton, {
  if (is.null(get("list_datasets", envir = .BitStatEnv))) {
    alert_message(session, type = "be", name = "데이터셋 목록", coda = TRUE)
    
    return()
  }
  
  if (is.null(selected_dataset_list())) {
    alert_message(session, type = "choice", name = "데이터셋", coda = TRUE)
    
    return()
  }
  
  datasets <- dslists()
  datasets[[selected_dataset_list()]] <- NULL
  
  assign("list_datasets", datasets, envir = .BitStatEnv)
  
  updateNumericInput(session, "rnd_dataset_list", value = sample(1:1000000, 1))
  updateNumericInput(session, "n_datasets_list", value = length(datasets))
  updateNumericInput(session, "editable_dataset", value = 0)
})


# 데이터셋 편집 이벤트 ---------------------------------------------------------
observeEvent(input$editDatasetButton, {
  if (is.null(selected_dataset_list())) {
    alert_message(session, type = "choice", name = "데이터셋", coda = TRUE)
    
    return()
  }
  
  datasets <- dslists()
  
  updateTextInput(session, "name_dataset_edit",
                  value = datasets[[selected_dataset_list()]]$dataset_name)
  updateTextInput(session, "desc_dataset_edit",
                  value = datasets[[selected_dataset_list()]]$dataset_desc)
  updateNumericInput(session, "editable_dataset", value = 1)
})


# 데이터셋 편집 저장 -----------------------------------------------------------
observeEvent(input$modifyDatasetButton, {
  datasets <- dslists()
  
  datasets[[selected_dataset_list()]]$dataset_name <- input$name_dataset_edit
  datasets[[selected_dataset_list()]]$dataset_desc <- input$desc_dataset_edit
  
  assign("list_datasets", datasets, envir = .BitStatEnv)
  
  updateNumericInput(session, "rnd_dataset_list", value = sample(1:1000000, 1))
  updateNumericInput(session, "editable_dataset", value = 0)
})



# 데이터셋 편집 취소 -----------------------------------------------------------
observeEvent(input$cancelDatasetButton, {
  updateTextInput(session, "name_dataset_edit", value = "")
  updateTextInput(session, "desc_dataset_edit", value = "")
  
  updateNumericInput(session, "editable_dataset", value = 0)
})


# 데이터셋 받기 이벤트 -----------------------------------------------------
observeEvent(input$downDatasetButton, {
  if (is.null(selected_dataset_list())) {
    alert_message(session, type = "choice", name = "데이터셋", coda = TRUE)
    
    return()
  }
  
  datasets <- dslists()
  
  name_dataset <- datasets[[selected_dataset_list()]]$dataset_name
  fname <- glue::glue("{name_dataset}.{input$file_format_down}")
  
  updateTextInput(session, "fname_d_file", value = fname)
  updateNumericInput(session, "downloadable_file", value = 1)
})


# 데이터셋 받기 취소 -----------------------------------------------------------
observeEvent(input$cancelFileButton, {
  updateTextInput(session, "fname_d_file", value = "")
  
  updateNumericInput(session, "downloadable_file", value = 0)
})


# 데이터셋 받기 파일 포맷 선택  ------------------------------------------------
observeEvent(input$file_format_down, {
  req(selected_dataset_list())
  
  datasets <- dslists()
  
  name_dataset <- datasets[[selected_dataset_list()]]$dataset_name
  fname <- glue::glue("{name_dataset}.{input$file_format_down}")
  
  updateTextInput(session, "fname_d_file", value = fname)
})


# 데이터셋 받기 핸들러 ---------------------------------------------------------
output$downFileData <- downloadHandler(
  filename = function() {
    input$fname_d_file
  },
  content = function(file) {
    datasets <- dslists()
    
    obs <- datasets[[selected_dataset_list()]]$dataset
    
    if (input$file_format_down %in% "csv") {
      readr::write_csv(obs, file)
    } else if (input$file_format_down %in% "xlsx") {
      xlsx::write.xlsx(obs, file)
    } else if (input$file_format_down %in% "rds") {
      readr::write_rds(obs, file, "xz", compression = 9L)
    }
    
    updateNumericInput(session, "downloadable_file", value = 0)
  }
)


# 데이터셋 다운로드 이벤트 -----------------------------------------------------
observeEvent(input$openDatasetDownload, {
  if (input$openDatasetDownload) {
    if (is.null(get("list_datasets", envir = .BitStatEnv))) {
      sendSweetAlert(session = session,
                     title = "미입력 오류",
                     btn_labels = c("Ok"),
                     text = "필터 목록이 없습니다.",
                     type = "error"
      )
      
      updatePrettySwitch(session, "openDatasetDownload", value = FALSE)
      
      return()
    }
    
    updateNumericInput(session, "downloadable_dataset", value = 1)
    updatePrettySwitch(session, "openFilterUpload", value = FALSE)
  } else {
    updateNumericInput(session, "downloadable_dataset", value = 0)
  }
})


# 데이터셋 다운로드 핸들러 -----------------------------------------------------
output$downDatasetData <- downloadHandler(
  filename = function() {
    paste(input$fname_d_dataset, ".rds", sep = "")
  },
  content = function(file) {
    obs <- dslists()
    
    readr::write_rds(obs, file, "xz", compression = 9L)
    
    updateNumericInput(session, "downloadable_dataset", value = 0)
  }
)




################################################################################
## 02. 데이터 > 데이터 진단
################################################################################
##==============================================================================
## 02.01. 데이터 > 데이터 진단 > 진단 개요
##==============================================================================
##------------------------------------------------------------------------------
## 02.01.01. 데이터 > 데이터 진단 > 진단 개요 UI 정의
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
          h4(i18n$t("데이터 진단 개요")),
          
          div(style="width: 300px;padding-top:5px;padding-bottom:10px",
              strong(i18n$t("데이터 진단 집계:")),
              reactableOutput("list_warnings", width = "100%")
          ),
          div(style="width: 800px;padding-top:5px;padding-bottom:10px",
              strong(i18n$t("데이터 진단 상세:")),
              reactableOutput("detail_warnings", width = "100%")
          )
        )
      )
    )
  )
})  
 

##==============================================================================
## 02.02. 데이터 > 데이터 진단 > 변수별 진단
##==============================================================================
##------------------------------------------------------------------------------
## 02.02.01. 데이터 > 데이터 진단 > 변수별 진단 UI 정의
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
          h4(i18n$t("변수별 데이터 진단")),
          div(style = "padding-top:5px;padding-bottom:10px",
              reactableOutput("variable_warnings", width = "100%")
          )
        )
      )
    )
  )
})


##==============================================================================
## 02.03. 데이터 > 데이터 진단 > 이상치(Outliers)
##==============================================================================
##------------------------------------------------------------------------------
## 02.03.01. 데이터 > 데이터 진단 > 이상치(Outliers) UI 정의
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
          h4(i18n$t("변수별 이상치 진단")),
          div(style="padding-top:5px;padding-bottom:10px",
              reactableOutput("variable_outliers", width = "100%")
          )
        )
      )
    )
  )  
})



##==============================================================================
## 02.04. 데이터 > 데이터 진단 > 보고서 출력
##==============================================================================
##------------------------------------------------------------------------------
## 02.04.01. 데이터 > 데이터 진단 > 보고서 출력 UI 정의
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
          h4(i18n$t("보고서 커버 설정")),
          style = "padding-top:0px;",
          div(style="display: inline-block;vertical-align:top; width: 100%;",
              textInput("title_diag", label = i18n$t("보고서 제목:"),
                        value = i18n$t("데이터 진단 보고서"))),
          div(style="display: inline-block;vertical-align:top; width: 100%;",
              textInput("subtitle_diag", label = i18n$t("보고서 부제목:"),
                        value = dataset_name)),
          div(style="display: inline-block;vertical-align:top; width: 50%",
              colourInput("title_col_diag", i18n$t("보고서 제목 색상:"), "white",
                          returnName = TRUE, palette = "limited",
                          closeOnClick = TRUE)),
          div(style="display: inline-block;vertical-align:top; width: 50%;",
              colourInput("subtitle_col_diag", i18n$t("보고서 부제목 색상:"),
                          "sandybrown", returnName = TRUE, palette = "limited",
                          closeOnClick = TRUE))
        )
      ),
      
      column(
        width = 4,
        wellPanel(
          style = "padding-top:5px;padding-bottom:10px !important;",
          h4(i18n$t("보고서 파라미터 설정")),
          style = "padding-top:0px;",
          div(
            style="display: inline-block;vertical-align:top; width: 100%;",
            sliderInput(
              inputId = "thres_uniq_cat",
              label = i18n$t("범주형 변수의 유일값 비율 상한 임계치 (0 ~ 1]:"),
              min = 0, max = 1, value = 0.5
            )
          ),
          div(
            style="display: inline-block;vertical-align:top; width: 100%;",
            sliderInput(
              inputId = "thres_uniq_num",
              label = i18n$t("수치형 변수의 유일값 개수 하한 임계치 (0 ~ 10]:"),
              min = 0, max = 10, value = 5
            )
          ),
          div(
            style="display: inline-block;vertical-align:top; width: 100%;",
            checkboxGroupInput(
              inputId = "flag_diag",
              label = i18n$t("부가 진단 항목:"), inline = TRUE,
              choices = element_diag,
              selected = c(1, 2, 3)
            )
          ),
          div(
            style="display: inline-block;vertical-align:top; width: 100%;",
            sliderInput(
              inputId = "sample_percent_diag",
              label = i18n$t("보고서 작성에 사용할 데이터 백분율 (5 ~ 100%]:"),
              min = 5, max = 100, value = 100
            )
          )
        )
      ),
      column(
        width = 4,
        wellPanel(
          style = "padding-top:5px;padding-bottom:35px !important;",
          h4(i18n$t("보고서 출력 설정")),
          style = "padding-top:0px;",
          div(
            style = "display: inline-block;vertical-align:top; width: 100%;",
            selectInput(
              inputId = "format_diag", 
              label = i18n$t("출력 포맷:"),
              choices = c("pdf", "html"),
              selected = "pdf"
            )
          ),
          div(
            style = "display: inline-block;vertical-align:top; width: 100%;",
            selectInput(
              inputId = "theme_color_diag", 
              label = i18n$t("색상 테마:"),
              choices = c("blue", "orange"),
              selected = "blue"
            )
          ),
          div(
            style = "display: inline-block;vertical-align:top; width: 100%;",
            textInput(
              inputId = "author_diag", 
              label = i18n$t("보고서 작성자:"),
              value = "dlookr")),
          div(
            style = "display: inline-block;vertical-align:top; width: 100%;",
            textInput(
              inputId = "file_diag", 
              label = i18n$t("파일 이름:"), 
              value = paste0("diag_", dataset_name)
            )
          ),
          actionButton(
            inputId = "printDiagnose", 
            label = i18n$t("보고서 출력"), 
            icon = icon("print"),
            style = "background-color: #90CAF9; border: none;"
          )
        )
      )
    )
  )  
})


##------------------------------------------------------------------------------
## 02.04.02. 데이터 > 데이터 진단 > 보고서 출력 이벤트 정의
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
        output_dir = "./",
        output_file = output_file,
        base_family = "NanumSquare"
      )
    
    browseURL(paste(".", output_file, sep = "/"))
  }
  
})



################################################################################
## 03. 데이터 > 데이터 변환
################################################################################
##==============================================================================
## 03.01. 데이터 > 데이터 변환 > 변수 조작
##==============================================================================
##------------------------------------------------------------------------------
## 03.01.01. 데이터 > 데이터 변환 > 변수 변경 UI 정의
##------------------------------------------------------------------------------

# 변수 리스트 출력 -------------------------------------------------------------
output$list_variables <- renderUI({
  req(input$combo_dataset)
  
  id_dataset <- input$combo_dataset
  
  nm_variables <- dslists()[[id_dataset]]$dataset %>%
    names()
  
  tab_variables <- dslists()[[id_dataset]]$dataset %>%
    get_class()    
  
  list_nm <- tab_variables %>% 
    mutate(nm = glue::glue("{variable} - {class}")) %>% 
    select(nm) %>% 
    pull()
  
  list_value <- tab_variables$variable 
  
  list_var <- seq(list_value) %>% 
    purrr::map(
      function(x) {
        list_value[x] %>% 
          as.character()
      }
    )
  
  names(list_var) <- list_nm
  
  updateNumericInput(session, "rnd_trans_list", value = 0)
  
  selectInput("list_variables", i18n$t("변수 목록:"),
              choices = list_var,
              selected = list_var[1],
              width = "250")
})


# sample data list -------------------------------------------------------------
output$data_contents <- renderReactable({
  req(input$combo_dataset)
  
  id_dataset <- input$combo_dataset
  
  dslists()[[id_dataset]]$dataset %>%
    filter(row_number() <= 5) %>%
    reactable(
      # defaultColDef = colDef(minWidth = 50),
      sortable = FALSE,
      bordered = TRUE
    )
})


# 데이터 형 변환 목록 출력 -----------------------------------------------------
output$list_change_type <- renderUI({
  req(input$combo_dataset)
  
  selectInput(
    inputId = "list_change_type", 
    label = i18n$t("변경 데이터 형:"),
    choices = element_change_type,
    selected = element_change_type[1],
    width = "250"
  )
})


# 범주 순서변경 출력 -----------------------------------------------------------
output$panel_reorder_levels <- renderUI({
  req(input$combo_dataset)
  
  id_dataset <- input$combo_dataset
  
  target_variable <- dslists()[[id_dataset]]$dataset %>% 
    select_at(vars(input$list_variables)) %>% 
    pull()
  
  list_levels <- if (is.factor(target_variable)) {
    levels(target_variable) 
  } else {
    levels(as_factor(target_variable))
  }

  validate(
    need(is.factor(target_variable), 
         i18n$t("범주 순서변경은 범주형 데이터만 지원합니다. 원한다면 먼저 범주형 데이터로 변경 후 진행하세요."))
  )
  
  validate(
    need(length(list_levels) < 31, 
         i18n$t("범주 순서변경은 범주 레벨의 개수가 30개까지만 지원합니다."))
  )
  
  fluidRow(
    column(
      width = 12,
      selectizeInput(
        inputId = "reorder_levels",
        i18n$t("범주 순서변경:"),
        choices = list_levels,
        selected = list_levels,
        multiple = TRUE,
        options = list(plugins = list("drag_drop"))
      ),
      actionButton(
        inputId = "reorderVariable",
        label = i18n$t("범주 순서변경"),
        icon = icon("sort-alpha-down"),
        style = "background-color: #90CAF9; border: none;"
      )
    )
  )
})



# 변수변환 출력 ----------------------------------------------------------------
output$panel_transform <- renderUI({
  req(input$combo_dataset)
  
  id_dataset <- input$combo_dataset
  
  numerical_variable <- dslists()[[id_dataset]]$dataset %>% 
    find_class("numerical", index = FALSE)
  
  validate(
    need(input$list_variables %in% numerical_variable, 
         i18n$t("변수변환은 정수와 실수만 지원합니다."))
  )
  
  fluidRow(
    column(
      width = 12,
      selectizeInput(
        inputId = "trans_method", 
        label = i18n$t("적용 함수:"),
        choices = c(
          "zscore", "minmax", "log", "log+1", "sqrt", "1/x", "x^2", "x^3"
        ),
        width = "250"
      ),
      numericInput(
        inputId = "trans_digit", 
        label = i18n$t("소수점 자리수:"), 
        min = 0,
        max = 7,
        value = 2,
        width = "250"
      ),
      shinyjs::hidden(
        numericInput("rnd_trans_list", label = "", value = 0)
      ),
      actionButton(
        inputId = "transformVariable",
        label = i18n$t("변수변환"),
        icon = icon("square-root-alt"),
        style = "background-color: #90CAF9; border: none;"
      )
    )
  )
})


# 변수변환 시각화 출력 ---------------------------------------------------------
output$densityOut <- renderPlot({
  req(input$combo_dataset)
  req(input$trans_method)
  
  id_dataset <- input$combo_dataset
  
  numerical_variable <- dslists()[[id_dataset]]$dataset %>% 
    find_class("numerical", index = FALSE)
  
  validate(
    need(input$list_variables %in% numerical_variable, 
         i18n$t("변수변환은 정수와 실수만 지원합니다."))
  )
  
  target_variable <- dslists()[[id_dataset]]$dataset %>% 
    select_at(vars(input$list_variables)) %>% 
    pull()
  
  trans <- dlookr::transform(target_variable, method = input$trans_method)
  assign("trans", trans, envir = .BitStatEnv)
  
  plot(trans)
})


## reactive variable object
bin_variable <- reactive({
  id_dataset <- input$combo_dataset
  
  target_variable <- dslists()[[id_dataset]]$dataset %>%
    select_at(vars(input$list_variables)) %>%
    pull()
})



##------------------------------------------------------------------------------
## 비닝
##------------------------------------------------------------------------------

# 비닝 출력 --------------------------------------------------------------------
output$panel_bin <- renderUI({
  req(input$combo_dataset)
  
  id_dataset <- input$combo_dataset
  
  numerical_variable <- dslists()[[id_dataset]]$dataset %>% 
    find_class("numerical", index = FALSE)
  
  validate(
    need(input$list_variables %in% numerical_variable, 
         i18n$t("비닝은 정수와 실수만 지원합니다."))
  )
  
  fluidRow(
    column(
      width = 12,
      selectizeInput(
        inputId = "cut_method",
        label = i18n$t("비닝 방법:"),
        choices = c("Manual" = "fixed", "Standard deviation" = "sd",
                    "Equal width" = "equal", "Pretty" = "pretty",
                    "Quantile" = "quantile", "K-means" = "kmeans"),
        selected = "quantile",
        width = "250"
      ),
      uiOutput("no_breaks"),
      textInput("breaks", i18n$t("비닝 컷 포인트:"), width = "250"),
      checkboxInput(
        inputId = "right",
        label = i18n$t("오른쪽 폐구간 여부 (right)"),
        FALSE
      ),
      checkboxInput(
        inputId = "inclowest",
        label = i18n$t("극단값 포함 여부 (include.lowest)"),
        FALSE
      ),
      checkboxInput(
        inputId = "addext",
        label = i18n$t("가능할 경우의 극단값 추가 여부"),
        FALSE
      ),
      numericInput(
        inputId = "diglab",
        label = i18n$t("소수점 라벨 표현 자리수 (dig.lab):"),
        min = 0, max = 10, value = 4,
        width = "250"
      ),
      textInput(
        inputId = "bin_variable",
        label = i18n$t("생성 변수 접미어:"),
        value = "_bin", width = "250"
      ),      
      actionButton(
        inputId = "binVariable",
        label = i18n$t("비닝"),
        icon = icon("cut"),
        style = "background-color: #90CAF9; border: none;"
      )
    )
  )
})


# 변수의 분포-------------------------------------------------------------------    
output$bin_distribution <- renderUI({ 
  req(input$combo_dataset)
  
  id_dataset <- input$combo_dataset

  numerical_variable <- dslists()[[id_dataset]]$dataset %>%
    find_class("numerical", index = FALSE)

  validate(
    need(input$list_variables %in% numerical_variable,
         i18n$t("비닝은 정수와 실수만 지원합니다."))
  )
  
  suppressWarnings(
    dslists()[[id_dataset]]$dataset %>% 
      select_at(vars(input$list_variables)) %>% 
      dlookr::describe(statistics = c("mean", "quantiles"),
               quantiles = c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1)) %>% 
      select(p00, p05, p10, p25, mean, p50, p75, p90, p95, p100) %>% 
      mutate_all(round, 2) %>% 
      rename(!!i18n$t("최솟값")   := p00,
             !!i18n$t("5%분위")   := p05,
             !!i18n$t("10%분위")  := p10,             
             !!i18n$t("1/4분위")  := p25,
             !!i18n$t("중위수")   := p50,
             !!i18n$t("산술평균") := mean,
             !!i18n$t("3/4분위")  := p75,
             !!i18n$t("90%분위")  := p90,
             !!i18n$t("95%분위")  := p95,
             !!i18n$t("최댓값")   := p100) %>% 
      flextable() %>% 
      htmltools_value()
  )
})  


# Breaks 개수 ------------------------------------------------------------------  
# referenced by icut.R of questionr package 
get_breaks <- function(b, compute = FALSE) {
  if (b == "") return(NULL)
  if (!stringr::str_detect(b, ",")) return(NULL)
  if (stringr::str_detect(b, ",$")) return(NULL)
  
  b <- gsub(", *$", "", b)
  b <- paste0("c(", b, ")")
  
  breaks <- sort(unique(eval(parse(text = b))))
  
  ## Code taken directly from `cut` source code
  if (length(breaks) == 1L && compute) {
    if (is.na(breaks) || breaks < 2L)
      stop("invalid number of intervals")
    nb <- as.integer(breaks + 1)
    dx <- diff(rx <- range(bin_variable(), na.rm = TRUE))
    if (dx == 0)
      dx <- abs(rx[1L])
    
    breaks <- seq.int(rx[1L] - dx / 1000, rx[2L] + dx / 1000, length.out = nb)
  }
  
  if (length(breaks) > 1 && input$addext) {
    if (min(breaks, na.rm = TRUE) > min(bin_variable(), na.rm = TRUE)) 
      breaks <- c(min(bin_variable(), na.rm = TRUE), breaks)
    if (max(breaks, na.rm = TRUE) < max(bin_variable(), na.rm = TRUE)) 
      breaks <- c(breaks, max(bin_variable(), na.rm = TRUE))
  }
  
  breaks
}


output$no_breaks <- renderUI({
  numericInput(
    inputId = "no_breaks", 
    label = i18n$t("범주 레벨 갯수:"), 
    value = 6, 
    min = 2, 
    step = 1,
    width = "250"
  )
})



# Breaks -----------------------------------------------------------------------
## referenced by icut.R of questionr package 
observe(
  if (req(input$cut_method) != "fixed") {
    id_dataset <- input$combo_dataset
    
    numerical_variable <- dslists()[[id_dataset]]$dataset %>% 
      find_class("numerical", index = FALSE)
    
    if (input$list_variables %in% numerical_variable) {
      x <- bin_variable()
    } else {
      x <- 1:100
    }
    
    no_breaks <- reactive({
      if (is.null(req(input$no_breaks))) return(2)
      if (is.na(req(input$no_breaks))) return(2)
      if (req(input$no_breaks) < 2) return(2)
  
      return(input$no_breaks)
    })
    
    updateTextInput(
      session,
      inputId = "breaks",
      value = classInt::classIntervals(
        x, n = ifelse(is.null(no_breaks()), 6, no_breaks()),
        style = req(input$cut_method))$brks
    )
  }
)


# Breaks 개수 토글 -------------------------------------------------------------
observe({
  toggleState(
    id = "no_breaks",
    condition = !is.null(input$list_variables) &
      !input$cut_method %in% c("", "fixed"))
})



# 히스토그램 출력 --------------------------------------------------------------
## referenced by icut.R of questionr package 
output$histOut <- renderPlot({
  req(input$combo_dataset)
  req(input$list_variables)
  req(input$no_breaks)  
  
  if (is.null(bin_variable())) return()

  id_dataset <- input$combo_dataset

  numerical_variable <- dslists()[[id_dataset]]$dataset %>%
    find_class("numerical", index = FALSE)

  validate(
    need(input$list_variables %in% numerical_variable,
         i18n$t("비닝은 정수와 실수만 지원합니다."))
  )

  if (!input$list_variables %in% numerical_variable) return()
    
  graphics::hist(bin_variable(),
                 col = "steelblue", border = "white",
                 main = paste("Binning with", input$cut_method),
                 xlab = input$list_variables)

  if (!is.null(input$breaks)) {
    breaks <- get_breaks(input$breaks, compute = TRUE)
 
    for (b in breaks)
      graphics::abline(v = b, col = "red", lwd = 1, lty = 2)
  }
})



# 비닝 리스트 ------------------------------------------------------------------ 
bins_list <- reactive({
  req(input$list_variables)
  req(input$no_breaks)  
  
  id_dataset <- input$combo_dataset

  numerical_variable <- dslists()[[id_dataset]]$dataset %>%
    find_class("numerical", index = FALSE)

  if (!input$list_variables %in% numerical_variable) {
    return()
  }
  
  if (is.null(input$breaks) | input$breaks == "") {
    return()
  }
  
  cut(bin_variable(),
      include.lowest = input$inclowest,
      right = input$right,
      dig.lab = input$diglab,
      breaks = get_breaks(input$breaks))
}) 



# 비닝 Bar plot ---------------------------------------------------------------- 
output$barOut <- renderPlot({
  req(input$list_variables)
  req(bins_list())
  
  graphics::plot(bins_list(), col = "steelblue", border = "white")
})



# 비닝 Summary table ----------------------------------------------------------- 
output$tab_bins <- renderReactable({
  if (is.null(bins_list())) return()
  
  bins_list() %>% 
    table(useNA = "always") %>% 
    as.data.frame() %>% 
    rename("Bins" = 1, 
           "Frequency" = 2) %>% 
    mutate("Ratio" = Frequency / sum(Frequency)) %>% 
    reactable(
      columns = list(
        Bins = colDef(
          name = i18n$t("범주 레벨"),
          na = "<NA>"
        ),
        Frequency = colDef(
          name = i18n$t("돗수"),
          format = colFormat(
            separators = TRUE
          )
        ),
        Ratio = colDef(
          name = "상대돗수 (백분율)",
          format = colFormat(
            percent = TRUE,
            digits = 2
          )
        )
      )
    )
})


output$panel_bin_out <- renderUI({ 
  req(input$combo_dataset)
  
  id_dataset <- input$combo_dataset
  
  numerical_variable <- dslists()[[id_dataset]]$dataset %>% 
    find_class("numerical", index = FALSE)
  
  validate(
    need(input$list_variables %in% numerical_variable, 
         i18n$t("비닝은 정수와 실수만 지원합니다."))
  )
  
  tagList(
    wellPanel(
      style = "padding-top:5px",
      fluidRow(
        column(
          width = 12, 
          style = "padding-top:0px;padding-bottom:10px;",
          h4(i18n$t("비닝 미리보기")),
          h5(strong(i18n$t("데이터 분포:"))),      
          uiOutput("bin_distribution")
        ),  
        column(
          width = 6, 
          h5(strong(i18n$t("데이터 분포 시각화:"))),   
          plotOutput("histOut")
        ),  
        column(
          width = 6, 
          h5(strong(i18n$t("돗수 분포 시각화:"))),
          plotOutput("barOut")
        )
      )
    ),
    
    wellPanel(
      h4(i18n$t("비닝 정의")),
      h5(strong(i18n$t("돗수 분포 테이블:"))),  
      reactableOutput("tab_bins", width = "100%")
    ) 
  )
})  



# 변수 조작 UI 정의 ------------------------------------------------------------
output$manipulate_variables <- renderUI({
  tagList(
    fluidRow(
      style = "padding-top:10px;padding-bottom:0px",
      column(
        width = 3,
        wellPanel(
          style = "padding-top:5px;padding-bottom:10px",
          h4(i18n$t("변수 조작 수행")),
          div(
            style = "display: inline-block;vertical-align:top;",
            selectInput(
              inputId = "manipulation_method",
              label = i18n$t("조작 방법:"),
              choices = element_manipulate_variables,
              width = "250"
            )
          ),
          uiOutput('list_variables'),      
          
          conditionalPanel(
            style = "padding-top:0px;",
            condition = "input.manipulation_method == 'Rename'",
            fluidRow(
              column(
                width = 10,
                textInput(
                  inputId = "rename_variable",
                  label = i18n$t("수정 변수 이름:"),
                  value = "", width = "250"
                ),
                actionButton(
                  inputId = "renameVariable",
                  label = i18n$t("변수이름 변경"),
                  icon = icon("signature"),
                  style = "background-color: #90CAF9; border: none;"
                )
              )
            )
          ),
          
          conditionalPanel(
            style = "padding-top:0px;",
            condition = "input.manipulation_method == 'Change type'",
            fluidRow(
              column(
                width = 10,
                uiOutput('list_change_type'),
                textInput(
                  inputId = "ext_change_type",
                  label = i18n$t("변환 변수 접미어:"),
                  value = "", width = "250",
                  placeholder = i18n$t("새로 만들 변수의 접미어 입력")
                ),
                actionButton(
                  inputId = "changeType",
                  label = i18n$t("변수 형 변환"),
                  icon = icon("reply"),
                  style = "background-color: #90CAF9; border: none;"
                )
              )
            )
          ),
          
          conditionalPanel(
            style = "padding-top:0px;",
            condition = "input.manipulation_method == 'Remove'",
            fluidRow(
              column(
                width = 10,
                actionButton(
                  inputId = "removeVariable",
                  label = i18n$t("변수 삭제"),
                  icon = icon("trash-alt"),
                  style = "background-color: #90CAF9; border: none;"
                )
              )
            )
          ),
          
          conditionalPanel(
            style = "padding-top:0px;",
            condition = "input.manipulation_method == 'Reorder levels'",
            uiOutput('panel_reorder_levels')
          ),
          
          conditionalPanel(
            style = "padding-top:0px;",
            condition = "input.manipulation_method == 'Transform'",
            
            uiOutput('panel_transform')
          ),
          
          conditionalPanel(
            style = "padding-top:0px;",
            condition = "input.manipulation_method == 'Bin'",
            
            uiOutput('panel_bin')
          )           
          
        )
      ),
      
      column(
        width = 9,
        wellPanel(
          style = "padding-top:5px",
          h4(i18n$t("샘플 데이터 미리보기")),
          reactableOutput("data_contents"),
          conditionalPanel(
            style = "padding-top:0px;",
            condition = "input.manipulation_method == 'Change type'",      
            fluidRow(
              style = "padding-top:10px",
              column(
                h4(i18n$t("형 변환 전 데이터 요약")),
                width = 6,
                verbatimTextOutput("summary_before")
              ),
              column(
                h4(i18n$t("형 변환 후 데이터 요약")),
                width = 6,
                verbatimTextOutput("summary_after")
              )
            ) 
          ),
          
          conditionalPanel(
            style = "padding-top:0px;",
            condition = "input.manipulation_method == 'Transform'",      
            fluidRow(
              style = "padding-top:10px",
              column(
                h4(i18n$t("데이터 분포 비교")),
                width = 12,
                plotOutput("densityOut")
              )  
            )    
          ),
          
          conditionalPanel(
            style = "padding-top:0px;",
            condition = "input.manipulation_method == 'Bin'",      
            style = "padding-top:10px",
            uiOutput('panel_bin_out')
          )            
        )
      )
    )
  )
})  


##------------------------------------------------------------------------------
## 03.01.02. 데이터 > 데이터 변환 > 변수 변경 이벤트 정의
##------------------------------------------------------------------------------

# 수정 변수 이름 입력  ---------------------------------------------------------
observeEvent(input$list_variables, {
  req(input$list_variables)
  
  updateTextInput(session, inputId = "rename_variable",
                  value = input$list_variables)
})


# 변수 이름 변경 이벤트 --------------------------------------------------------
observeEvent(input$renameVariable, {
  new_name <- input$rename_variable
  
  datasets <- dslists()
  
  id_dataset <- input$combo_dataset
  dfm <- datasets[[id_dataset]]$dataset 
  names(dfm)[names(dfm) %in% input$list_variables] <- new_name
  
  datasets[[id_dataset]]$dataset <- dfm
  
  assign("list_datasets", datasets, envir = .BitStatEnv)
  assign("choosed_dataset", id_dataset, envir = .BitStatEnv)
  
  updateNumericInput(session, "rnd_dataset_list", value = sample(1:1000000, 1))
})


# 형 변환 이벤트 ---------------------------------------------------------------
observeEvent(input$changeType, {
  change_name <- input$list_variables
  new_name <- paste0(change_name, input$ext_change_type)
  
  datasets <- dslists()
  
  id_dataset <- input$combo_dataset
  
  dfm <- datasets[[id_dataset]]$dataset 
  dfm[, new_name] <- dfm %>% 
    transmute_at(.vars = vars(all_of(change_name)), .funs = input$list_change_type) 
  
  datasets[[id_dataset]]$dataset <- dfm
  
  assign("list_datasets", datasets, envir = .BitStatEnv)
  assign("choosed_dataset", id_dataset, envir = .BitStatEnv)
  
  updateNumericInput(session, "rnd_dataset_list", value = sample(1:1000000, 1))
})


# 변수 삭제 이벤트 -------------------------------------------------------------
observeEvent(input$removeVariable, {
  remove_name <- input$list_variables
  
  datasets <- dslists()
  
  id_dataset <- input$combo_dataset
  dfm <- datasets[[id_dataset]]$dataset %>% 
    select(!matches(glue::glue("^{remove_name}$")))
  
  datasets[[id_dataset]]$dataset <- dfm
  
  assign("list_datasets", datasets, envir = .BitStatEnv)
  assign("choosed_dataset", id_dataset, envir = .BitStatEnv)
  
  updateNumericInput(session, "rnd_dataset_list", value = sample(1:1000000, 1))
})


# 형 변환 전 변수 집계 ---------------------------------------------------------
output$summary_before <- renderPrint({ 
  change_name <- input$list_variables    
  
  datasets <- dslists()
  
  id_dataset <- input$combo_dataset
  
  datasets[[id_dataset]]$dataset %>% 
    select_at(vars(all_of(change_name))) %>% 
    pull() %>% 
    summary()
})


# 형 변환 후 변수 집계 ---------------------------------------------------------
output$summary_after <- renderPrint({ 
  req(input$list_change_type)
  
  change_name <- input$list_variables    
  
  datasets <- dslists()
  
  id_dataset <- input$combo_dataset
  
  x <- datasets[[id_dataset]]$dataset %>% 
    select_at(vars(all_of(change_name))) %>% 
    pull() 
  
  do.call(input$list_change_type, list(x)) %>% 
    summary()
})


# 범주 순서변경 이벤트 ---------------------------------------------------------
observeEvent(input$reorderVariable, {
  reorder_name <- input$list_variables
  
  datasets <- dslists()
  
  reorder <- function(x, reorder_levels) {
    ordered(x, levels = reorder_levels)
  }
    
  id_dataset <- input$combo_dataset
  dfm <- datasets[[id_dataset]]$dataset %>% 
    mutate_at(vars(reorder_name), reorder, input$reorder_levels)
  
  datasets[[id_dataset]]$dataset <- dfm
  
  assign("list_datasets", datasets, envir = .BitStatEnv)
  assign("choosed_dataset", id_dataset, envir = .BitStatEnv)
  
  updateNumericInput(session, "rnd_dataset_list", value = sample(1:1000000, 1))
})


# 변수변환 이벤트 --------------------------------------------------------------
observeEvent(input$transformVariable, {
  trans_name <- input$list_variables
  
  datasets <- dslists()
  
  trans <- get("trans", envir = .BitStatEnv) %>% 
    as.numeric() %>% 
    round(input$trans_digit)
  
  id_dataset <- input$combo_dataset
  datasets[[id_dataset]]$dataset[[trans_name]] <- trans
  
  assign("list_datasets", datasets, envir = .BitStatEnv)
  assign("choosed_dataset", id_dataset, envir = .BitStatEnv)
  
  updateNumericInput(session, "rnd_dataset_list", value = sample(1:1000000, 1))
})


# 비닝 이벤트 ------------------------------------------------------------------
observeEvent(input$binVariable, {
  bin_name <- paste0(input$list_variables, input$bin_variable)
  
  datasets <- dslists()
  id_dataset <- input$combo_dataset
  
  datasets[[id_dataset]]$dataset <- datasets[[id_dataset]]$dataset %>% 
    mutate(!!bin_name := bins_list())

  assign("list_datasets", datasets, envir = .BitStatEnv)
  assign("choosed_dataset", id_dataset, envir = .BitStatEnv)
  
  updateNumericInput(session, "rnd_dataset_list", value = sample(1:1000000, 1))
})



################################################################################
## 04. 데이터 메뉴 정의
################################################################################
output$ui_manage_data <- renderUI({
  tagList(
    tabBox(
      width = 12,
      tabPanel(
        title = i18n$t("데이터 준비"),
        tabsetPanel(
          tabPanel(
            title = i18n$t("데이터 업로드"), 
            uiOutput("upload_data"),
            icon = shiny::icon("upload")
          ),
          tabPanel(
            title = i18n$t("데이터셋 관리"), 
            uiOutput("manage_dataset"),
            icon = shiny::icon("archive")
          )
        )
      ),
      
      tabPanel(
        title = i18n$t("데이터 진단"),
        tabsetPanel(
          tabPanel(
            title = i18n$t("진단 개요"), 
            uiOutput("overview_diagnose"),
            icon = shiny::icon("stethoscope")
          ),
          tabPanel(
            title = i18n$t("변수별 진단"), 
            uiOutput("variable_diagnose"),
            icon = shiny::icon("list-ul")
          ),
          tabPanel(
            title = i18n$t("이상치"), 
            uiOutput("variable_outlier"),
            icon = shiny::icon("balance-scale-left")
          ),
          tabPanel(
            title = i18n$t("보고서 작성"), 
            uiOutput("pdf_diagnose"),
            icon = shiny::icon("file-pdf")
          )
        )
      ),
      
      tabPanel(
        title = i18n$t("데이터 변환"),
        tabsetPanel(
          tabPanel(
            title = i18n$t("변수 조작"), 
            uiOutput("manipulate_variables"),
            icon = shiny::icon("exchange-alt")
          ),
          tabPanel(
            title = i18n$t("데이터 정제"),
            icon = shiny::icon("broom")
          ),
          tabPanel(
            title = i18n$t("데이터 분할"),
            icon = shiny::icon("cut")
          )
        )
      )
    ) 
  )  
})
  