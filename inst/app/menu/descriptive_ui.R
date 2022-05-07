################################################################################
## 03. 기술통계 > 상관관계
################################################################################
##==============================================================================
## 03.01. 기술통계 > 상관관계 > 상관행렬
##==============================================================================
##------------------------------------------------------------------------------
## 03.01.01. 기술통계 > 상관관계 > 상관행렬 UI 정의
##------------------------------------------------------------------------------
output$correlation_matrix <- renderUI({
  tagList(
    fluidRow(
      style = "padding-top:10px;padding-bottom:0px",
      column(
        width = 3,
        wellPanel(
          style = "padding-top:5px;padding-bottom:10px",
          h4(translate("상관행렬 설정")),
          selectizeInput(
            inputId = "correlation_method",
            label = translate("상관계수 종류:"),
            choices = element_corr_method,
            selected = "pearson",
            width = "250"
          ),
          checkboxInput(
            inputId = "is_group_corr_mat",
            label = translate("범주별 계산"),
            value = FALSE
          ),      
          conditionalPanel(
            style = "padding-top:0px;",
            condition = "input.is_group_corr_mat == 1",      
            fluidRow(
              column(
                width = 12,
                uiOutput('list_cat_var_corrmat')
              )  
            )    
          ),          
          numericInput(
            inputId = "diglab_corr_mat",
            label = translate("상관계수 소수점 자리수:"),
            min = 0, max = 10, value = 3,
            width = "250"
          ),
          checkboxInput(
            inputId = "viz_corr_mat",
            label = translate("시각화 여부"),
            value = FALSE
          ),      
          actionButton(
            inputId = "runCorrelationMatrix",
            label = translate("실행"),
            icon = icon("cogs"),
            style = "background-color: #90CAF9; border: none;"
          )   
        )
      ),
      
      column(
        width = 9,
        wellPanel(
          style = "padding-top:10px; padding-left:10px; padding-right:10px",
          htmlOutput("mat_correlation", style = "height: 700px;")
        )
      )
    )
  )
})


##------------------------------------------------------------------------------
## 03.01.02. 데이터 > 데이터 변환 > 변수 변경 이벤트 정의
##------------------------------------------------------------------------------
# 변수 리스트 출력 -------------------------------------------------------------
output$list_cat_var_corrmat <- renderUI({
  req(input$combo_dataset)
  
  id_dataset <- input$combo_dataset
  
  list_cat <- dslists()[[id_dataset]]$dataset %>%
    get_class() %>% 
    filter(class %in% c("factor", "ordered")) %>% 
    select(variable) %>% 
    pull()
  
  selectInput("list_cat_var_corrmat", translate("범주형 변수 목록:"),
              choices = list_cat,
              multiple = TRUE,
              width = "250")
})

##------------------------------------------------------------------------------
## 03.03. 범주별 계산 체크버튼 선택
##------------------------------------------------------------------------------
observeEvent(input$is_group_corr_mat, {
  req(input$combo_dataset)
  
  if (!input$is_group_corr_mat) {
    return()
  }
  
  id_dataset <- input$combo_dataset
  
  list_cat <- dslists()[[id_dataset]]$dataset %>%
    get_class() %>% 
    filter(class %in% c("factor", "ordered")) %>% 
    select(variable) %>% 
    pull() %>% 
    as.character()
  
  updateSelectInput(session, 
                    "list_cat_var_corrmat",
                    choices = list_cat)
})

##------------------------------------------------------------------------------
## 03.03. 실행 버튼 클릭
##------------------------------------------------------------------------------
observeEvent(input$runCorrelationMatrix, {
  req(input$combo_dataset)
  
  id_dataset <- input$combo_dataset
  
  rmd_content <- create_mat_corr(
    id_dataset = id_dataset, 
    method = input$method,
    digits = input$diglab_corr_mat,
    group_flag = input$is_group_corr_mat,
    group_variable = input$list_cat_var_corrmat,
    plot = input$viz_corr_mat
  )
  
  output$mat_correlation <- renderUI({
    input$runCorrelationMatrix
    
    tags$iframe(
      seamless = "seamless",
      src = "report/corr_mat.html",
      width = "100%",
      height = "100%"
    )
  })
})



##==============================================================================
## 03.02. 기술통계 > 상관관계 > 상관검정
##==============================================================================
##------------------------------------------------------------------------------
## 03.02.01. 기술통계 > 상관관계 > 상관검정 UI 정의
##------------------------------------------------------------------------------
output$correlation_test <- renderUI({
  tagList(
    fluidRow(
      style = "padding-top:10px;padding-bottom:0px",
      column(
        width = 3,
        wellPanel(
          style = "padding-top:5px;padding-bottom:10px",
          h4(translate("상관검정 설정")),
          uiOutput("list_varriable_corrtest"),
          selectizeInput(
            inputId = "correlation_method_test",
            label = translate("상관계수 종류:"),
            choices = element_corr_method,
            selected = "pearson",
            width = "250"
          ),
          selectizeInput(
            inputId = "alternative",
            label = translate("대립가설:"),
            choices = element_alternative_test,
            selected = "two.sided",
            width = "250"
          ),          
          checkboxInput(
            inputId = "is_group_corr_test",
            label = translate("범주별 계산"),
            value = FALSE
          ),      
          conditionalPanel(
            style = "padding-top:0px;",
            condition = "input.is_group_corr_test == 1",      
            fluidRow(
              column(
                width = 12,
                uiOutput('list_cat_var_corrtest')
              )  
            )    
          ), 
          checkboxInput(
            inputId = "viz_corr_test",
            label = translate("시각화 여부"),
            value = FALSE
          ),      
          actionButton(
            inputId = "runCorrelationTest",
            label = translate("실행"),
            icon = icon("cogs"),
            style = "background-color: #90CAF9; border: none;"
          )   
        )
      ),
      
      column(
        width = 9,
        wellPanel(
          style = "padding-top:10px; padding-left:10px; padding-right:10px",
          htmlOutput("test_correlation", style = "height: 700px;")
        )
      )
    )
  )
})


##==============================================================================
## 03.03. 리포트 생성 이벤트 핸들러
##==============================================================================
##------------------------------------------------------------------------------
## 03.03. 수치형 변수 목록 생성
##------------------------------------------------------------------------------
output$list_varriable_corrtest <- renderUI({
  req(input$combo_dataset)
  
  id_dataset <- input$combo_dataset
  
  list_num <- dslists()[[id_dataset]]$dataset %>%
    get_class() %>% 
    filter(class %in% c("numeric", "integer")) %>% 
    select(variable) %>% 
    pull()
  
  selectInput("list_varriable_corrtest", translate("수치형 변수 목록(두개 선택):"),
              choices = list_num,
              multiple = TRUE,
              width = "250")
})

##------------------------------------------------------------------------------
## 03.03. 범주형 변수 목록 생성
##------------------------------------------------------------------------------
output$list_cat_var_corrtest <- renderUI({
  req(input$combo_dataset)
  
  id_dataset <- input$combo_dataset
  
  list_cat <- dslists()[[id_dataset]]$dataset %>%
    get_class() %>% 
    filter(class %in% c("factor", "ordered")) %>% 
    select(variable) %>% 
    pull()
  
  selectInput("list_cat_var_corrtest", translate("범주형 변수 목록:"),
              choices = list_cat,
              multiple = TRUE,
              width = "250")
})


##------------------------------------------------------------------------------
## 03.03. 변수선택 버튼 선택
##------------------------------------------------------------------------------
observeEvent(input$list_varriable_corrtest, {
  if (!is.null(notice_id))
    removeNotification(notice_id)
  
  notice_id <<- NULL
})


##------------------------------------------------------------------------------
## 03.04. 범주별 계산 체크버튼 선택
##------------------------------------------------------------------------------
observeEvent(input$is_group_corr_test, {
  req(input$combo_dataset)
  
  if (!is.null(notice_id))
    removeNotification(notice_id)
  
  notice_id <<- NULL
  
  if (!input$is_group_corr_test) {
    return()
  }
    
  id_dataset <- input$combo_dataset
  
  list_cat <- dslists()[[id_dataset]]$dataset %>%
    get_class() %>% 
    filter(class %in% c("factor", "ordered")) %>% 
    select(variable) %>% 
    pull() %>% 
    as.character()
  
  updateSelectInput(session, 
                    "list_cat_var_corrtest",
                    choices = list_cat)
})


##------------------------------------------------------------------------------
## 03.05. 실행 버튼 클릭
##------------------------------------------------------------------------------
observeEvent(input$runCorrelationTest, {
  req(input$combo_dataset)
  
  id_dataset <- input$combo_dataset
  
  if (length(input$list_varriable_corrtest) != 2) {
    message <- translate("수치변수는 2개를 선택해야 합니다.")
    
    # Save the ID for removal later
    notice_id <<- showNotification(message, duration = 0, type = "error")
    
    return()
  }
  
  if (input$is_group_corr_test & length(input$list_cat_var_corrtest) == 0) {
    message <- translate("범주별 검정을 체크했으나 범주형 변수는 선택하지 않았습니다. 체크를 해제하거나 변수를 선택하세요.")
    
    # Save the ID for removal later
    notice_id <<- showNotification(message, duration = 0, type = "error")
    
    return()
  }  
  
  rmd_content <- create_mat_test(
    id_dataset = id_dataset, 
    variables =  input$list_varriable_corrtest,
    method = input$correlation_method_test,
    alternative = input$alternative,
    group_flag = input$is_group_corr_test,
    group_variable = input$list_cat_var_corrtest,
    plot = input$viz_corr_test
  )
  
  output$test_correlation <- renderUI({
    input$runCorrelationTest
    
    tags$iframe(
      seamless = "seamless",
      src = "report/corr_test.html",
      width = "100%",
      height = "100%"
    )
  })
})


################################################################################
## 04. 기술통계 메뉴 정의
################################################################################
output$ui_desc_data <- renderUI({
  tagList(
    tabBox(
      width = 12,
      tabPanel(
        title = translate("상관관계"),
        tabsetPanel(
          tabPanel(
            title = translate("상관행렬"), 
            uiOutput("correlation_matrix"),
            icon = shiny::icon("cubes")
          ),
          tabPanel(
            title = translate("상관검정"),
            uiOutput("correlation_test"),
            icon = shiny::icon("stethoscope")
          )
        )
      )
      
    ) 
  )  
})
  