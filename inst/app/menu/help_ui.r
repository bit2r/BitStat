## reference https://stackoverflow.com/questions/24875943/display-html-file-in-shiny-app
addResourcePath("tmpuser", getwd())

output$date_prepare <- renderUI({
  tags$iframe(
    seamless = "seamless",
    src = "tmpuser/help/date_prepare.html",
    width = "100%",
    height = "100%"
  )
})

# output$date_prepare <- renderUI({
#   includeHTML("help/date_prepare.html")
# })

output$ui_help <- renderUI({
  tagList(
    tabBox(
      width = 12,
      tabPanel(
        title = translate("데이터"),
        tabsetPanel(
          tabPanel(
            title = translate("데이터 준비"),
            htmlOutput("date_prepare", style = "height: 700px;")
          ),
          tabPanel(
            title = translate("데이터 진단")
          ),
          tabPanel(
            title = translate("데이터 변환")
          )          
        )
      ),
      
      tabPanel(
        title = translate("기술통계"),
        tabsetPanel(
          tabPanel(
            title = translate("탐색적 데이터분석")
          )
        )
      )
    ) 
  )  
})