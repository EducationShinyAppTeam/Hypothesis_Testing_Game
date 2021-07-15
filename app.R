library(shiny)
library(boastUtils)
library(shinyalert)
library(shinyBS)
library(shinyWidgets)

source("ticTacToe.R")

questionBank <- read.csv(
  file = "questionBank.csv",
  header = TRUE,
  stringsAsFactors = FALSE,
  as.is = TRUE
)

# UI ----
ui <- dashboardPage(
  skin = "purple",
  # Header ----
  dashboardHeader(
    title = 'Hypothesis Testing Game',
    titleWidth = 250,
    tags$li(
      class = "dropdown",
      boastUtils::surveyLink(name = "Hypothesis_Testing_TicTacToe_Game")
      ),
    tags$li(
      class = "dropdown",
      tags$a(href = "https://shinyapps.science.psu.edu/", icon("home"))
    )
  ),
  # Sidebar ----
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "pages",
      menuItem(text = "Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem(text = "Game", tabName = "game", icon = icon("gamepad")),
      menuItem(text = "References", tabName = "references", icon = icon("leanpub"))
    ),
    tags$div(
      class = "sidebar-logo",
      boastUtils::sidebarFooter()
    )
  ),
  #Pages ----
  dashboardBody(tabItems(
    #Overview Page ----
    tabItem(
      tabName = "overview",
      withMathJax(),
      h1("Null Hypothesis Significance Testing Tic-Tac-Toe"),
      p(
        "This app quizzes your knowledge of null hypothesis significance testing (NHST) concepts using a tic-tac-toe game."
      ),
      h2("Instructions"),
      p("To play the game: "),
      tags$ol(
        tags$li("Review any pre-requisite ideas."),
        tags$li("Click the GO! button to go the game page."),
        tags$li("Select whether you'll play as the O's or the X's."),
        tags$li("Select the square that you want to place your marker."),
        tags$li(
          "Answer the question that is given. If you're correct, you get that square. If not, the computer will."
        ),
        tags$li(
          "Win by filling a row, a column, or a main diagonal with your mark (X's or O's)."
        )
      ),
      div(
        style = "text-align: center;",
        bsButton(
          inputId = "go1",
          label = "Go!",
          size = "large",
          icon = icon("bolt")
        )
      ),
      br(),
      br(),
      h2("Acknowledgements"),
      p(
        "This version of the app was developed and coded by Neil J. Hatfield and Robert P. Carey, III.",
        "The concept of the app stems from David Robinson (2017) and Ryan Voyack (2018).",
        br(),
        "The question bank was written by David Robinson and Neil J. Hatfield.",
        br(),
        br(),
        br(),
        div(class = "updated", "Last Update: 7/15/2021 by NJH.")
      )
    ),
    #Game Page ----
    tabItem(
      tabName = "game",
      withMathJax(),
      useShinyalert(),
      h2("NHST Tic-Tac-Toe"),
      p(
        "To play, click on any one of the buttons that have a question mark.
        A question will appear to the right with possible answers. If you answer
        correctly, you will take the square; if not, the computer will take the square. Try your best to win the game!"
      ),
      tttUI(namespaceID = "gameTab")
    ),
    # References ----
    tabItem(
      tabName = "references",
      withMathJax(),
      h2("References"),
      p(
        class = "hangingindent",
        "Attali, D. and Edwards, T. (2018). shinyalert: Easily create pretty
        popup messages (modals) in 'Shiny'. (v1.0). [R package]. Available from
        https://CRAN.R-project.org/package=shinyalert"
      ),
      p(
        class = "hangingindent",
        "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
        (v0.61). [R package]. Available from
        https://CRAN.R-project.org/package=shinyBS"
      ),
      p(
        class = "hangingindent",
        "Carey, R. (2019). boastUtils: BOAST Utilities. (v0.1.0). [R Package].
        Available from https://github.com/EducationShinyAppTeam/boastUtils"
      ),
      p(
        class = "hangingindent",
        "Chang, W. and Borges Ribeio, B. (2018). shinydashboard: Create
        dashboards with 'Shiny'. (v0.7.1) [R Package]. Available from
        https://CRAN.R-project.org/package=shinydashboard"
      ),
      p(
        class = "hangingindent",
        "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J. (2019).
        shiny: Web application framework for R. (v1.4.0) [R Package]. Available
        from https://CRAN.R-project.org/package=shiny"
      ),
      p(
        class = "hangingindent",
        "Perrier, V., Meyer, F., Granjon, D. (2019). shinyWidgets: Custom inputs
        widgets for shiny. (v0.5.0) [R Package]. Available from
        https://CRAN.R-project.org/package=shinyWidgets"
      ),
      br(),
      br(),
      br(),
      boastUtils::copyrightInfo()
    )
  ))
)

# Server ----

server <- function(input, output, session) {
  # Define navigation buttons
  observeEvent(
    eventExpr = input$go1,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "game"
      )
    })

  tttServer(
    namespaceID = "gameTab",
    gridSize =  3,
    questionBank =  questionBank,
    parent = session
  )
}

# Boast App Call ----
boastApp(ui = ui, server = server)
