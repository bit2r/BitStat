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

# Change for server side excute
# define js function for opening urls in new tab/window
js_code <- "shinyjs.browseURL = function(url) {
              window.open(url,'_blank');
           }"

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
    menuItem(translate("데이터 선택"),
             shinyjs::hidden(
               numericInput("rnd_dataset_list", label = "", value = 0)
             ),
             uiOutput("combo_dataset"),
             tabName = "choice_data", icon = icon("table")),
    menuItem(translate("데이터"),   tabName = "manage_data", icon = icon("database")),
    menuItem(translate("기술통계"), tabName = "desc_data",   icon = icon("calculator")),
    menuItem(translate("가설검정"), tabName = "hypothesis",  icon = icon("vials")),
    menuItem(translate("회귀분석"), tabName = "regression",  icon = icon("chart-line")),
    menuItem(translate("분산분석"), tabName = "anova",       icon = icon("balance-scale-right")),
    menuItem(translate("보고서"),   tabName = "report",      icon = icon("file-pdf")),
    menuItem(translate("도움말"),   tabName = "help",        icon = icon("question-circle"))
  )
)


################################################################################
## 04. Body structures
################################################################################
##==============================================================================
## 04.01. Left side menus
##==============================================================================
body <- dashboardBody(
  ## Spinner somewhere in UI
  add_busy_spinner(
    spin = "fading-circle",
    margins = c(50, 50),
    height = "60px",
    width  = "60px"
  ),

  # Change for server side excute
  useShinyjs(),
  extendShinyjs(text = js_code, functions = 'browseURL'),
  
  tabItems(
    tabItem(
      tabName = "manage_data",
      uiOutput("ui_manage_data")
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
      tabName = "help",
      uiOutput("ui_help")      
    )
  )
)


################################################################################
## 05. UX structures
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

