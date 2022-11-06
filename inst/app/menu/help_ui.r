## reference https://stackoverflow.com/questions/24875943/display-html-file-in-shiny-app
addResourcePath("tmpuser", getwd())

help_language <- reactive({
  get("language", envir = .BitStatEnv)
}) 

output$data_prepare <- renderUI({
  tags$iframe(
    seamless = "seamless",
    src = glue::glue("tmpuser/help/data_prepare_{help_language()}.html"),
    width = "100%",
    height = "100%"
  )
})


output$data_disgnose <- renderUI({
  tags$iframe(
    seamless = "seamless",
    src = glue::glue("tmpuser/help/data_diagnose_{help_language()}.html"),
    width = "100%",
    height = "100%"
  )
})


output$data_trans <- renderUI({
  tags$iframe(
    seamless = "seamless",
    src = glue::glue("tmpuser/help/data_trans_{help_language()}.html"),
    width = "100%",
    height = "100%"
  )
})


output$descriptive_summary <- renderUI({
  tags$iframe(
    seamless = "seamless",
    src = glue::glue("tmpuser/help/descriptive_summary_{help_language()}.html"),
    width = "100%",
    height = "100%"
  )
})


output$descriptive_correlation <- renderUI({
  tags$iframe(
    seamless = "seamless",
    src = glue::glue("tmpuser/help/descriptive_correlation_{help_language()}.html"),
    width = "100%",
    height = "100%"
  )
})


output$ui_help <- renderUI({
  tagList(
    tabBox(
      width = 12,
      tabPanel(
        title = translate("데이터"),
        tabsetPanel(
          tabPanel(
            title = translate("데이터 준비"),
            htmlOutput("data_prepare", style = "height: 700px;")
          ),
          tabPanel(
            title = translate("데이터 변환"),
            htmlOutput("data_trans", style = "height: 700px;")            
          )          
        ),
        icon = icon("database")
      ),
      
      tabPanel(
        title = translate("기술통계"),
        tabsetPanel(
          tabPanel(
            title = translate("집계표"),
            htmlOutput("descriptive_summary", style = "height: 700px;")            
          ),
          tabPanel(
            title = translate("상관관계"),
            htmlOutput("descriptive_correlation", style = "height: 700px;")            
          )          
        ),
        icon = icon("calculator")
      ),
      
      tabPanel(
        title = translate("보고서"),
        tabsetPanel(
          tabPanel(
            title = translate("데이터 진단"),
            htmlOutput("data_disgnose", style = "height: 700px;")            
          )      
        ),
        icon = icon("file-pdf")
      )      
    ) 
  )  
})