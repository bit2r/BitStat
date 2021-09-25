################################################################################
## 01. Define JS
################################################################################
##==============================================================================
## 01.01. Change Shinydashboard Skin
##==============================================================================
# Reference https://stackoverflow.com/questions/57592774/how-to-select-shinydashboard-skin-dynamically
js <- "Shiny.addCustomMessageHandler('change_skin', function(skin) {
        document.body.className = skin;
       });"


################################################################################
## 02. Header content of the dashboard
################################################################################
##==============================================================================
## 02.01. Right controller
##==============================================================================
controlbar_menu <- controlbarMenu(
  id = "controlbarMenu",
  controlbarItem(
    icon = icon("paint-brush"),
    title = "Skin",
    tags$head(tags$script(js)),
    selectInput(
      inputId = "skin_color", 
      label = "스킨 색상",
      c("블루" = "blue", "오렌지" = "yellow", "화이트" = "black",
        "퍼플" = "purple", "그린" = "green", "레드" = "red")
    )
  )
)

##==============================================================================
## 02.02. Define header
##==============================================================================
header <- dashboardHeader(
  title = tagList(
    span(class = "logo-lg", "BitStat"),
    img(class = "logo-mini", src = "r2bit.png",
        style = 'float: right;padding-right:10px;padding-top:15px;
        height:35px;width:40px'))
)


################################################################################
## 03. Sidebar content of the dashboard
################################################################################
sidebar <- dashboardSidebar(
  id = "stats_sidebar",
  sidebarMenu(
    menuItem(i18n$t("데이터 선택"),
             numericInput("rnd_dataset_list", label = "", value = 0),
             uiOutput("combo_dataset"),
             tabName = "choice_data", icon = icon("table")),
    menuItem(i18n$t("데이터"),   tabName = "manage_data", icon = icon("database")),
    menuItem(i18n$t("기술통계"), tabName = "desc_data",   icon = icon("calculator")),
    menuItem(i18n$t("가설검정"), tabName = "hypothesis",  icon = icon("vials")),
    menuItem(i18n$t("회귀분석"), tabName = "regression",  icon = icon("chart-line")),
    menuItem(i18n$t("분산분석"), tabName = "anova",       icon = icon("balance-scale-right")),
    menuItem(i18n$t("보고서"),   tabName = "report",      icon = icon("file-pdf")),
    menuItem(i18n$t("도움말"),   tabName = "help",        icon = icon("question-circle"))
  )
)



################################################################################
## 04. 데이터
################################################################################
##==============================================================================
## 04.01. 데이터 > 데이터 준비
##==============================================================================
##------------------------------------------------------------------------------
## 04.01.01. 데이터 > 데이터 준비 > 데이터 업로드
##------------------------------------------------------------------------------
upload_data_frow <- fluidRow(
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
          numericInput("flag_upload", label = "", value = 0),
          uiOutput('data_file')
      ),

      conditionalPanel(
        style = "padding-top:0px;",
        condition = "input.flag_upload == 1",
        div(style="display: inline-block;vertical-align:top; width: 300px;",
            textInput("name_dataset", label = i18n$t("데이터셋 이름:"), value = "")),
        div(style="display: inline-block;vertical-align:top; width: 300px;",
            textInput("desc_dataset", label = i18n$t("데이터셋 설명:"), value = "")),
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


##------------------------------------------------------------------------------
## 04.01.02. 데이터 > 데이터 준비 > 데이터셋 관리
##------------------------------------------------------------------------------
manage_dataset_frow <- fluidRow(
  style = "padding-top:10px;padding-bottom:0px",
  useShinyjs(),
  column(
    width = 12, 
    wellPanel(
      style = "padding-top:5px;padding-bottom:10px",
      h4(i18n$t("데이터셋 목록")),
      reactableOutput("imported_ds_list", width = "100%"),
      numericInput("n_datasets_list", label = "", value = 0),

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

      numericInput("editable_dataset", label = "", value = 0),

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

      numericInput("downloadable_file", label = "", value = 0),

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

      numericInput("downloadable_dataset", label = "", value = 0),

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


##==============================================================================
## 04.02. 데이터 > 데이터 진단
##==============================================================================
##------------------------------------------------------------------------------
## 04.02.01. 데이터 > 데이터 진단 > 진단 개요
##------------------------------------------------------------------------------
overview_diagnose_frow <- fluidRow(
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


##------------------------------------------------------------------------------
## 04.02.02. 데이터 > 데이터 진단 > 변수별 진단
##------------------------------------------------------------------------------
variable_diagnose_frow <- fluidRow(
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


##------------------------------------------------------------------------------
## 04.02.03. 데이터 > 데이터 진단 > 이상치(Outliers)
##------------------------------------------------------------------------------
variable_outlier_frow <- fluidRow(
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


##------------------------------------------------------------------------------
## 04.02.04. 데이터 > 데이터 진단 > 보고서 출력
##------------------------------------------------------------------------------
pdf_diagnose <- fluidRow(
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
                   value = "")),
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
          value = ""
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


##==============================================================================
## 04.03. 데이터 > 데이터 변환
##==============================================================================
##------------------------------------------------------------------------------
## 04.03.01. 데이터 > 데이터 변환 > 변수 변경
##------------------------------------------------------------------------------
manipulate_variables_frow <- fluidRow(
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
          width = 250
        )
      ),
      uiOutput('list_variables'),      
      
      conditionalPanel(
        style = "padding-top:0px;",
        condition = "input.manipulation_method == 'rename'",
        fluidRow(
          column(
            width = 10,
            textInput(
              inputId = "rename_variable",
              label = i18n$t("수정 변수 이름:"),
              value = "", width = 250
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
        condition = "input.manipulation_method == 'change_type'",
        fluidRow(
          column(
            width = 10,
            uiOutput('list_change_type'),
            textInput(
              inputId = "ext_change_type",
              label = i18n$t("변환 변수 접미어:"),
              value = "", width = 250
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
        condition = "input.manipulation_method == 'remove'",
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
        condition = "input.manipulation_method == 'change_type'",      
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
      )  
    )
  )
)


##==============================================================================
## 04.04. 데이터 메뉴 정의
##==============================================================================
manage_data <- fluidRow(
  tabBox(
    width = 12,
    tabPanel(
      title = i18n$t("데이터 준비"),
      tabsetPanel(
        tabPanel(
          title = i18n$t("데이터 업로드"), 
          upload_data_frow,
          icon = shiny::icon("upload")
        ),
        tabPanel(
          title = i18n$t("데이터셋 관리"), 
          manage_dataset_frow,
          icon = shiny::icon("archive")
        )
      )
    ),
    
    tabPanel(
      title = i18n$t("데이터 진단"),
      tabsetPanel(
        tabPanel(
          title = i18n$t("진단 개요"), 
          overview_diagnose_frow,
          icon = shiny::icon("stethoscope")
        ),
        tabPanel(
          title = i18n$t("변수별 진단"), 
          variable_diagnose_frow,
          icon = shiny::icon("list-ul")
        ),
        tabPanel(
          title = i18n$t("이상치"), 
          variable_outlier_frow,
          icon = shiny::icon("balance-scale-left")
        ),
        tabPanel(
          title = i18n$t("보고서 작성"), 
          pdf_diagnose,
          icon = shiny::icon("file-pdf")
        )
      )
    ),
    
    tabPanel(
      title = i18n$t("데이터 변환"),
      tabsetPanel(
        tabPanel(
          title = i18n$t("변수 조작"), 
          manipulate_variables_frow,
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



################################################################################
## 10. Body structures
################################################################################
##==============================================================================
## 10.01. Left side menus
##==============================================================================
body <- dashboardBody(
  ## Spinner somewhere in UI
  add_busy_spinner(
    spin = "fading-circle",
    margins = c(50, 50),
    height = "60px",
    width  = "60px"
  ),

  tabItems(
    tabItem(
      tabName = "manage_data",
      manage_data
    ),
    tabItem(
      tabName = "desc_data"
    ),
    tabItem(
      tabName = "hypothesis"
    ),
    tabItem(
      tabName = "regression"
    ),
    tabItem(
      tabName = "anova"
    ),
    tabItem(
      tabName = "report"
    ),
    tabItem(
      tabName = "help"
    )
  )
)


################################################################################
## 11. UX structures
################################################################################
ui <- dashboardPage(
  useShinyjs(),
  header = header,
  sidebar = sidebar,
  body = body,
  controlbar = dashboardControlbar(
    id = "filters",
    width = 300,
    skin = "dark",
    controlbar_menu
  ),
  footer = dashboardFooter(
    left = "By Choonghyun Ryu",
    right = "bitR, 2021"
  ),
  title = 'Statistical Analyzer with R - BitStat',
  skin = 'blue')

