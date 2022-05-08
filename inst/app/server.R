shinyServer(function(input, output, session) {
  # A notification ID
  notice_id <- NULL  
  
  ## load source for menu
  for (file in list.files(c("menu"), pattern = "\\.(r|R)$", full.names = TRUE)) {
    source(file, local = TRUE)
  }
  
  ## load source for report
  for (file in list.files(c("report"), pattern = "\\.(r|R)$", full.names = TRUE)) {
    source(file, local = TRUE)
  }

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
    
    selectInput("combo_dataset", translate("데이터셋:"),
                choices = tab_ds,
                selected = choosed_dataset)
  })

  import_file <- reactiveValues(
    upload_state = NULL
  )

  
})



