server <- function(input, output, session) {
  ##============================================================================
  ## Hidden Components
  ##============================================================================
  ## 데이터 업로드
  shinyjs::hide("n_datasets_list")
  shinyjs::hide("rnd_dataset_list")
  shinyjs::hide("flag_upload")

  ## 데이터셋 관리
  shinyjs::hide("editable_dataset")
  shinyjs::hide("downloadable_dataset")
  shinyjs::hide("downloadable_file")

  encoding <- reactiveVal()

  observeEvent(input$skin_color, {
    session$sendCustomMessage("change_skin", paste0("skin-", input$skin_color))
  })


  ##############################################################################
  ## 01.데이터
  ##############################################################################

  ##============================================================================
  ## 01.01. 데이터 > 데이터 준비
  ##============================================================================

  ##----------------------------------------------------------------------------
  ## 01.01.01. 데이터 > 데이터 준비 > 데이터 업로드
  ##----------------------------------------------------------------------------

  # 데이터셋 목록 객체 ---------------------------------------------------------
  dslists <- reactive({
    input$rnd_dataset_list

    get("list_datasets", envir = .BitStatEnv)
  })

  # 데이터셋 목록 콤보박스 정의 ------------------------------------------------
  output$combo_dataset <- renderUI({
    tab_ds <- dslists() %>%
      purrr::map_chr("dataset_id")

    names(tab_ds) <- dslists() %>%
      purrr::map_chr("dataset_name")

    choosed_dataset <- get("choosed_dataset", envir = .BitStatEnv)
    
    if (is.null(choosed_dataset)) {
      choosed_dataset <- tab_ds[1]
    }
    
    selectInput("combo_dataset", i18n$t("데이터셋:"),
                choices = tab_ds,
                selected = choosed_dataset)
  })

  import_file <- reactiveValues(
    upload_state = NULL
  )

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


  # upload the data file -------------------------------------------------------
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


  # 파일포맷 선택 --------------------------------------------------------------
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
      alert_message(session, type = "have", name = "데이터셋", coda = TRUE,
                    message = "업로드하지 않았습니다.")

      return()
    }


    if (input$name_dataset == "") {
      alert_message(session, type = "input", name = "데이터셋 이름", coda = TRUE)

      return()
    }

    if (input$desc_dataset == "") {
      alert_message(session, type = "input", name = "데이터셋 설명", coda = TRUE)

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


  ##----------------------------------------------------------------------------
  ## 01.01.02. 데이터 > 데이터 준비 > 데이터셋 관리
  ##----------------------------------------------------------------------------

  # 데이터셋 목록 출력 ---------------------------------------------------------
  output$imported_ds_list <- renderReactable({
    input$rnd_dataset_list

    tabs <- dslists()

    tab_data_list(tabs)
  })



  # 데이터셋 제거 이벤트 -------------------------------------------------------
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


  # 데이터셋 편집 이벤트 -------------------------------------------------------
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


  # 데이터셋 편집 저장 ---------------------------------------------------------
  observeEvent(input$modifyDatasetButton, {
    datasets <- dslists()

    datasets[[selected_dataset_list()]]$dataset_name <- input$name_dataset_edit
    datasets[[selected_dataset_list()]]$dataset_desc <- input$desc_dataset_edit

    assign("list_datasets", datasets, envir = .BitStatEnv)

    updateNumericInput(session, "rnd_dataset_list", value = sample(1:1000000, 1))
    updateNumericInput(session, "editable_dataset", value = 0)
  })



  # 데이터셋 편집 취소 ---------------------------------------------------------
  observeEvent(input$cancelDatasetButton, {
    updateTextInput(session, "name_dataset_edit", value = "")
    updateTextInput(session, "desc_dataset_edit", value = "")

    updateNumericInput(session, "editable_dataset", value = 0)
  })


  # 데이터셋 받기 이벤트 ---------------------------------------------------
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


  # 데이터셋 받기 취소 ---------------------------------------------------------
  observeEvent(input$cancelFileButton, {
    updateTextInput(session, "fname_d_file", value = "")

    updateNumericInput(session, "downloadable_file", value = 0)
  })


  # 데이터셋 받기 파일 포맷 선택  ----------------------------------------------
  observeEvent(input$file_format_down, {
    req(selected_dataset_list())

    datasets <- dslists()

    name_dataset <- datasets[[selected_dataset_list()]]$dataset_name
    fname <- glue::glue("{name_dataset}.{input$file_format_down}")

    updateTextInput(session, "fname_d_file", value = fname)
  })


  # 데이터셋 받기 핸들러 -------------------------------------------------------
  output$downFileData <- downloadHandler(
    filename = function() {
      input$fname_d_file
    },
    content = function(file) {
      datasets <- dslists()

      print(file)

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


  # 데이터셋 다운로드 이벤트 ---------------------------------------------------
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


  # 데이터셋 다운로드 핸들러 ---------------------------------------------------
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


  ##============================================================================
  ## 01.02. 데이터 > 데이터 진단
  ##============================================================================

  ##----------------------------------------------------------------------------
  ## 01.02.01. 데이터 > 데이터 진단 > 진단 개요
  ##----------------------------------------------------------------------------

  # 데이터 진단 개요 출력 ------------------------------------------------------
  output$list_warnings <- renderReactable({
    req(input$combo_dataset)

    id_dataset <- input$combo_dataset

    tab_warning <- dslists()[[id_dataset]]$dataset %>%
      diagnose_warning()

    list_diagnose_warning(tab_warning)
  })


  # 데이터 진단 목록 출력 ------------------------------------------------------
  output$detail_warnings <- renderReactable({
    req(input$combo_dataset)

    id_dataset <- input$combo_dataset

    tab_warning <- dslists()[[id_dataset]]$dataset %>%
      diagnose_warning()

    detail_diagnose_warning(tab_warning)
  })


  ##----------------------------------------------------------------------------
  ## 01.02.02. 데이터 > 데이터 진단 > 변수별 진단
  ##----------------------------------------------------------------------------


  # 변수별 진단 출력 ------------------------------------------------------
  output$variable_warnings <- renderReactable({
    req(input$combo_dataset)

    id_dataset <- input$combo_dataset

    dslists()[[id_dataset]]$dataset %>%
      dlookr:::html_variable(theme = "blue", base_family = "NanumSquare")
  })


  ##----------------------------------------------------------------------------
  ## 01.02.03. 데이터 > 데이터 진단 > 이상치(Outliers)
  ##----------------------------------------------------------------------------


  # 이상치 출력 ------------------------------------------------------
  output$variable_outliers <- renderReactable({
    req(input$combo_dataset)

    id_dataset <- input$combo_dataset

    dslists()[[id_dataset]]$dataset %>%
      dlookr:::html_outlier(theme = "blue", base_family = "NanumSquare")
  })


  ##----------------------------------------------------------------------------
  ## 01.02.04. 데이터 > 데이터 진단 > 보고서 출력
  ##----------------------------------------------------------------------------
  # 데이터셋 변경 이벤트  ------------------------------------------------------
  observeEvent(input$combo_dataset, {
    req(input$combo_dataset)

    id_dataset <- input$combo_dataset

    dataset_name <- dslists()[[id_dataset]]$dataset_name

    updateTextInput(session, "subtitle_diag", value = dataset_name)
    updateTextInput(session, "file_diag", value = paste0("diag_", dataset_name))
  })


  # 출력포맷 변경 이벤트  ------------------------------------------------------
  observeEvent(input$format_diag, {
    if (input$format_diag %in% "html") {
      updateColourInput(session, "title_col_diag", value = "gray50")
    } else{
      updateColourInput(session, "title_col_diag", value = "white")
    }
  })


  # 보고서 출력 ----------------------------------------------------------------
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
  

  ##============================================================================
  ## 01.03. 데이터 > 데이터 변환
  ##============================================================================
  
  ##----------------------------------------------------------------------------
  ## 01.03.01. 데이터 > 데이터 변환 > 변수 조작
  ##----------------------------------------------------------------------------
  
  # 변수 리스트 출력 ------------------------------------------------------
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
          list_value[x]
        }
      )
    names(list_var) <- list_nm
    
    selectInput("list_variables", i18n$t("변수 목록:"),
                choices = list_var,
                selected = list_var[1],
                width = 250)
  })
  
  
  # sample data list -----------------------------------------------------------
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
  
  
  # 데이터 형 변환 목록 출력 ---------------------------------------------------
  output$list_change_type <- renderUI({
    req(input$combo_dataset)
    
    selectInput(
      inputId = "list_change_type", 
      label = i18n$t("변경 데이터 형:"),
      choices = element_change_type,
      selected = element_change_type[1],
      width = 250
    )
  })
  
  
  # 수정 변수 이름 입력  -------------------------------------------------------
  observeEvent(input$list_variables, {
    req(input$list_variables)
    
    updateTextInput(session, inputId = "rename_variable",
                    value = input$list_variables)
  })

  
  # 변수 이름 변경 이벤트 ------------------------------------------------------
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
  

  # 형 변환 이벤트 -------------------------------------------------------------
  observeEvent(input$changeType, {
    change_name <- input$list_variables
    new_name <- paste0(change_name, input$ext_change_type)
    
    datasets <- dslists()
    
    id_dataset <- input$combo_dataset
    
    dfm <- datasets[[id_dataset]]$dataset 
    dfm[, new_name] <- dfm %>% 
      transmute_at(.vars = vars(change_name), .funs = input$list_change_type) 
    
    datasets[[id_dataset]]$dataset <- dfm
    
    assign("list_datasets", datasets, envir = .BitStatEnv)
    assign("choosed_dataset", id_dataset, envir = .BitStatEnv)
    
    updateNumericInput(session, "rnd_dataset_list", value = sample(1:1000000, 1))
  })
  
  
  # 변수 삭제 이벤트 -----------------------------------------------------------
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
  
  
  # 형 변환 전 변수 집계 -------------------------------------------------------
  output$summary_before <- renderPrint({ 
    change_name <- input$list_variables    
    
    datasets <- dslists()
    
    id_dataset <- input$combo_dataset
    
    datasets[[id_dataset]]$dataset %>% 
      select_at(vars(change_name)) %>% 
      pull() %>% 
      summary()
  })
  
  
  # 형 변환 후 변수 집계 -------------------------------------------------------
  output$summary_after <- renderPrint({ 
    change_name <- input$list_variables    
    
    datasets <- dslists()
    
    id_dataset <- input$combo_dataset
    
    x <- datasets[[id_dataset]]$dataset %>% 
      select_at(vars(change_name)) %>% 
      pull() 
      
    do.call(input$list_change_type, list(x)) %>% 
      summary()
  })
  
}



