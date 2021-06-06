library(boastUtils)
library(shinyalert)
library(shinyBS)
library(shinyWidgets)
library(shiny)

GRID_SIZE <- 3
TILE_COUNT <- GRID_SIZE ^ 2
APP_TITLE <<- "Hypothesis Testing Game"

ui <- dashboardPage(
  skin = "purple",
  # Header ----
  dashboardHeader(
    title = APP_TITLE,
    titleWidth = 250,
    tags$li(
      class = "dropdown", 
      actionLink("info",icon("info"))
    ),
    tags$li(
      class = "dropdown",
      tags$a(target = "_blank", icon("comments"),
             href = "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=Hypothesis_Testing_Game"
      )
    ),
    tags$li(class = "dropdown", 
            tags$a(href='https://shinyapps.science.psu.edu/', 
                   icon("home", lib = "font-awesome")
                   )
            )
    ),
  
  # Sidebar ----
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "pages",
      menuItem(
        text = "Overview",
        tabName = "overview",
        icon = icon("dashboard")
      ),
      menuItem(
        text = "Prerequisites",
        tabName = "prereq",
        icon = icon("book")
      ),
      menuItem(
        text = "Game",
        tabName = "game",
        icon = icon("gamepad")
      ),
      menuItem(
        text = "References",
        tabName = "references",
        icon = icon("leanpub")
      )
    ),
    tags$div(
      class = "sidebar-logo",
      boastUtils::sidebarFooter()
    )
  ),
  
  # Pages ----
  dashboardBody(
    tabItems(
      
      ## Overview Page ----
      tabItem(
        tabName = "overview",
        withMathJax(),
        h1("Null Hypothesis Significance Testing Tic-Tac-Toe"),
        p("This app quizzes your knowledge of null hypothesis significance testing 
        (NHST) concepts using a tic-tac-toe game."),
        h2("Instructions"),
        p("To play the game: "),
        tags$ol(
          tags$li("Review any prerequistes before playing the game."),
          tags$li("Start the game and choose whether you'll play as the O's or the X's."),
          tags$li("Select the square that you want to place your mark."),
          tags$li(
            "Answer the question that is given. If you're correct, you get that square. If not, the computer will."
          ),
          tags$li(
            "Win by filling a row, a column, or a main diagonal with your mark (X's or O's)."
          )
        ),
        div(
          style = "text-align: center",
          bsButton(
            inputId = "toprereq",
            label = "Read the Prerequisites",
            size = "large",
            icon = icon("bolt")
          )
        ),
        br(),
        br(),
        h2("Acknowledgements"),
        p(
          "This version of the app was developed and coded by Neil J. Hatfield and Robert P. Carey, III.",
          br(),
          "The concept of the app stems from David Robinson (2017) and Ryan Voyack (2018).",
          br(),
          "The question bank was written by David Robinson and Neil J. Hatfield.",
          br(),
          "The app was updated by Kellien Peritz (2021).")
      ),
      
      ## Prereq Page ----
      tabItem(
        tabName = "prereq",
        h2("NHST Concepts"),
        br(),
        h3("p-values"),
        p(strong("Definition of the p-value:"), 
          "Given that the null hypothesis is true, the probability of obtaining 
          a sample statistic as extreme or more extreme than the one in the 
          observed sample, in the direction of the alternative hypothesis.",
          br(),
          strong("Statistical Significance:"),
          "A test is considered to be statistically significant when 
            the p-value is less than or equal to the level of significance, 
            also known as the alpha",
          withMathJax(helpText('$\alpha$')), "level."
        ),
        br(),
        h3("Writing Hypotheses"),
        p(strong("Null Hypothesis"),
          "Always a statement of equality because there is not a difference in 
          the populations, denoted as XXX.",
          br(),
          strong("Alternative Hypothesis"),
          "Always a statement of inequality because there is some difference in 
          the populations, denoted as XXX or XXX."
        ),
        tags$ol(
          tags$li("Two-tailed"),
          p("Use $\neq$ in alternative hypothesis."),
          tags$li("Left-tailed"),
          p("Use XXX in alternative hypothesis."),
          tags$li("Right-tailed"),
          p("Use XXX in alternative hypothesis.")
        ),
        br(),
        div(
          style = "text-align: center",
          bsButton(
            inputId = "togame",
            label = "Let's play the game!",
            size = "large",
            icon = icon("bolt")
          )
        )
      ),
      
      ## Game Page ----
      tabItem(
        tabName = "game",
        withMathJax(),
        useShinyalert(),
        h2("NHST Tic-Tac-Toe"),
        p(
          "To play, click on any one of the buttons that have a question mark. A question will appear to the right with possible answers. If you answer correctly, you will take the square; if not, the computer will take the square. Try your best to win the game!"
        ),
        h3(uiOutput("player")),
        fluidRow(
          div(
            class = "col-sm-12 col-md-4",
            h3("Game Board"),
            br(),
            uiOutput("gameBoard", class = "game-board")
          ),
          div(
            class = "col-sm-12 col-md-8",
            h3("Question"),
            withMathJax(uiOutput("question")),
            uiOutput("extraOutput"),
            h3("Answer"),
            uiOutput("answer"),
            bsButton(
              inputId = "submit",
              label = "Submit",
              size = "large",
              style = "default",
              disabled = TRUE
            ),
            bsButton(
              inputId = "reset",
              label = "Reset Game",
              color = "primary",
              size = "large",
              style = "default"
            ),
            br(),
            #These two triggers help with MathJax re-rendering
            uiOutput("trigger1"),
            uiOutput("trigger2")
          )
        )
      ),
      ## References ----
      tabItem(
        tabName = "references",
        h2("References"),
        p(
          class = "hangingindent",
          "Attali, D. and Edwards, T. (2018). shinyalert: Easily create pretty popup messages (modals) in 'Shiny'. (v1.0). [R package]. Available from https://CRAN.R-project.org/package=shinyalert"
        ),
        p(
          class = "hangingindent",
          "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny. (v0.61). [R package]. Available from https://CRAN.R-project.org/package=shinyBS"
        ),
        p(
          class = "hangingindent",
          "Carey, R. (2019). boastUtils: BOAST Utilities. (v0.1.0). [R Package]. Available from https://github.com/EducationShinyAppTeam/boastUtils"
        ),
        p(
          class = "hangingindent",
          "Chang, W. and Borges Ribeio, B. (2018). shinydashboard: Create dashboards with 'Shiny'. (v0.7.1) [R Package]. Available from https://CRAN.R-project.org/package=shinydashboard"
        ),
        p(
          class = "hangingindent",
          "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J. (2019). shiny: Web application framework for R. (v1.4.0) [R Package]. Available from https://CRAN.R-project.org/package=shiny"
        ),
        p(
          class = "hangingindent",
          "Perrier, V., Meyer, F., Granjon, D. (2019). shinyWidgets: Custom inputs widgets for shiny. (v0.5.0) [R Package]. Available from https://CRAN.R-project.org/package=shinyWidgets"
        ),
        br(),
        br(),
        br(),
        boastUtils::copyrightInfo()
      )
    )
  )
)

server <- function(input, output, session) {
  # Variables
  activeBtn <- NA
  activeQuestion <- NA
  player <- NA
  opponent <- NA
  scoreMatrix <-
    matrix(
      data = rep.int(0, times = TILE_COUNT),
      nrow = GRID_SIZE,
      ncol = GRID_SIZE
    )
  gameProgress <- FALSE
  
  # Helper Functions
  .tileCoordinates <- function(tile = NULL, index = NULL) {
    row <- -1
    col <- -1

    # if: button tile is given, derive from id
    # else: derive from index
    if (!is.null(tile)) {
      # grid-[row]-[col]
      tile <- strsplit(tile, "-")[[1]]
      tile <- tile[-1] # remove oxo

      row <- strtoi(tile[1])
      col <- strtoi(tile[2])
    } else {
      row <- (index - 1) %/% GRID_SIZE + 1
      col <- index - (GRID_SIZE * (row - 1))
    }

    coordinates <- list("row" = row,
                        "col" = col)

    return(coordinates)
  }

  .tileIndex <- function(tile) {
    coords <- .tileCoordinates(tile)

    index = GRID_SIZE * (coords$row - 1) + coords$col

    return(index)
  }

  .btnReset <- function(index) {
    coords <- .tileCoordinates(index = index)
    id <- paste0("grid-", coords$row, "-", coords$col)
    updateButton(
      session = session,
      inputId = id,
      label = "?",
      disabled = FALSE
    )
  }

  .score <- function(score, tile, value) {
    i <- .tileCoordinates(tile)

    score[i$row, i$col] <- value

    return(score)
  }

  .gameCheck <- function(mat) {
    rows <- rowSums(mat)
    cols <- colSums(mat)

    if (GRID_SIZE > 1) {
      mainD <- sum(diag(mat))
      rotated <- apply(t(mat), 2, rev)
      offD <- sum(diag(rotated))

      if (GRID_SIZE %in% rows ||
          GRID_SIZE %in% cols ||
          mainD == GRID_SIZE || offD == GRID_SIZE) {
        return("win")
      } else if (-GRID_SIZE %in% rows ||
                 -GRID_SIZE %in% cols == 1 ||
                 mainD == -GRID_SIZE || offD == -GRID_SIZE) {
        return("lose")
      } else if (any(mat == 0)) {
        return("continue")
      } else {
        return("draw")
      }
    } else {
      ifelse(rows == 1 && rows != 0, return("win"), return("lose"))
    }
  }

  .boardBtn <- function(tile) {
    index <- .tileIndex(tile)
    activeQuestion <<- gameSet[index, "id"]

    output$question <- renderUI({
      withMathJax()
      return(gameSet[index, "question"])
    })

    output$answer <- .ansFunc(index, gameSet)

    if (gameSet[index, "extraOutput"] != "") {
      output$extraOutput <- renderText({
        gameSet[index, "extraOutput"]
      })
    } else {
      output$extraOutput <- NULL
    }

    #Retrigger MathJax processing
    output$trigger1 <- renderUI({
      withMathJax()
    })
    output$trigger2 <- renderUI({
      withMathJax()
    })

    #Enable Submit Button
    updateButton(session = session,
                 inputId = "submit",
                 disabled = FALSE)
  }

  .ansFunc <- function(index, df) {
    if (df[index, "format"] == "numeric") {
      renderUI({
        numericInput(inputId = "ans",
                     label = df[index, "label"],
                     value = 0)
      })
    } else if (df[index, "format"] == "two") {
      renderUI({
        radioGroupButtons(
          inputId = "ans",
          choices = list(df[index, "A"],
                         df[index, "B"]),
          selected = character(0),
          checkIcon = list(
            yes = icon("check-square"),
            no = icon("square-o")
          ),
          status = "textGame",
          direction = "horizontal",
          individual = TRUE
        )
      })
    } else if (df[index, "format"] == "three") {
      renderUI({
        radioGroupButtons(
          inputId = "ans",
          choices = list(df[index, "A"],
                         df[index, "B"],
                         df[index, "C"]),
          selected = character(0),
          checkIcon = list(
            yes = icon("check-square"),
            no = icon("square-o")
          ),
          status = "textGame",
          direction = "vertical"
        )
      })
    } else {
      renderUI({
        radioGroupButtons(
          inputId = "ans",
          choices = list(df[index, "A"],
                         df[index, "B"],
                         df[index, "C"],
                         df[index, "D"]),
          selected = character(0),
          checkIcon = list(
            yes = icon("check-square"),
            no = icon("square-o")
          ),
          status = "textGame",
          direction = "vertical"
        )
      })
    }
  }

  .gameReset <- function() {
    lapply(1:TILE_COUNT, .btnReset)
    qSelected <<-
      sample(seq_len(nrow(questionBank)), size = TILE_COUNT, replace = FALSE)
    gameSet <<- questionBank[qSelected,]

    output$question <-
      renderUI({
        return("Click a button on the game board to get started on your new game.")
      })
    output$answer <- renderUI({
      ""
    })
    output$extraOutput <- renderUI({
      ""
    })
    scoreMatrix <<-
      matrix(
        data = rep.int(0, times = TILE_COUNT),
        nrow = GRID_SIZE,
        ncol = GRID_SIZE
      )
    gameProgress <- FALSE
    activeBtn <- NA

    updateButton(session = session,
                 inputId = "submit",
                 disabled = TRUE)
  }

  ## BEGIN App Specific xAPI Wrappers ----
  .generateStatement <- function(session, verb = NA, object = NA, description = NA) {
    if(is.na(object)){
      object <- paste0("#shiny-tab-", session$input$pages)
    }

    stmt <- boastUtils::generateStatement(
      session,
      verb = verb,
      object = object,
      description = description
    )

    response <- boastUtils::storeStatement(session, stmt)

    return(response)
  }

  .generateAnsweredStatement <- function(session, verb = NA, object = NA, description = NA, interactionType = NA, response = NA, success = NA, completion = FALSE) {

    stmt <- boastUtils::generateStatement(
      session,
      verb = verb,
      object = object,
      description = paste0("Question ", activeQuestion, ": ", description),
      interactionType = interactionType,
      success = success,
      response = response,
      completion = completion,
      extensions = list(
        ref = "https://educationshinyappteam.github.io/BOAST/xapi/result/extensions/scoreMatrix",
        value = paste(as.data.frame(scoreMatrix), collapse = ", ")
      )
    )

    response <- boastUtils::storeStatement(session, stmt)

    return(response)
  }
  ## END App Specific xAPI Wrappers ----

  # Define navigation buttons
  
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "Answer questions correctly about hypothesis testing in order to 
      fill the tic-tac-toe board and win the game.",
      type = NULL
    )
  })
  
  observeEvent(input$toprereq, {
    updateTabItems(
      session = session, 
      inputId = "pages", 
      selected = "prereq")
  })
  
  observeEvent(input$togame, {
    updateTabItems(
      session = session, 
      inputId = "pages", 
      selected = "game")
  })

  # Read in data and generate the first subset
  questionBank <-
    read.csv("questionBank.csv",
             stringsAsFactors = FALSE,
             as.is = TRUE)
  qSelected <-
    sample(seq_len(nrow(questionBank)), size = TILE_COUNT, replace = FALSE)
  gameSet <- questionBank[qSelected,]

  # Program the Reset Button
  observeEvent(input$reset, {
    .generateStatement(session, object = "reset", verb = "interacted", description = "Game board has been reset.")
    .gameReset()
  })

  # Render Game Board / Attach Observers
  output$gameBoard <- renderUI({
    board <- list()
    index <- 1

    sapply(1:GRID_SIZE, function(row) {
      sapply(1:GRID_SIZE, function(column) {
        id <- paste0("grid-", row, "-", column)

        board[[index]] <<- tags$li(
          actionButton(
            inputId = paste0("grid-", row, "-", column),
            label = "?",
            color = "primary",
            style = "bordered",
            class = "grid-fill"
          ),
          class = "grid-tile"
        )

        observeEvent(session$input[[id]], {
          activeBtn <<- id
          .boardBtn(id)
          .generateStatement(session, object = activeBtn, verb = "interacted", description = paste0("Tile ", activeBtn, " selected. Rendering question: ", activeQuestion, "."))
        })

        index <<- index + 1
      })
    })

    tags$ol(board, class = paste(
      "grid-board",
      "grid-fill",
      paste0("grid-", GRID_SIZE, "x", GRID_SIZE)
    ))
  })

  # Program Submit Button
  observeEvent(input$submit, {
    index <- .tileIndex(activeBtn)
    answer <- ""

    if (gameSet[index, "format"] == "numeric") {
      answer <- gameSet[index, "answer"]
    } else {
      answer <- gameSet[index, gameSet[index, "answer"]]
    }

    success <- input$ans == answer

    if (is.null(success) || length(success) == 0) {
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "Please select an answer before pressing Submit.",
        type = "error"
      )
    } else if (success) {
      updateButton(
        session = session,
        inputId = activeBtn,
        label = player,
        disabled = TRUE
      )
      scoreMatrix <<- .score(scoreMatrix, activeBtn, 1)
    } else {
      updateButton(
        session = session,
        inputId = activeBtn,
        label = opponent,
        disabled = TRUE
      )
      scoreMatrix <<- .score(scoreMatrix, activeBtn,-1)
    }

    # Check for game over states
    .gameState <- .gameCheck(scoreMatrix)
    completion <- ifelse(.gameState == "continue", FALSE, TRUE)
    interactionType <- ifelse(gameSet[index,]$format == "numeric", "numeric", "choice")

    .generateAnsweredStatement(
      session,
      object = activeBtn,
      verb = "answered",
      description = gameSet[index,]$question,
      response = input$ans,
      interactionType = interactionType,
      success = success,
      completion = completion
    )

    if (.gameState == "win") {
      .generateStatement(session, object = "game", verb = "completed", description = "Player has won the game.")
      confirmSweetAlert(
        session = session,
        inputId = "endGame",
        title = "You Win!",
        text = "You've filled either a row, a column, or a main diagonal. Start over and play a new game.",
        btn_labels = "Start Over"
      )
    } else if (.gameState == "lose") {
      .generateStatement(session, object = "game", verb = "completed", description = "Player has lost the game.")
      confirmSweetAlert(
        session = session,
        inputId = "endGame",
        title = "You lose :(",
        text = "Take a moment to review the concepts and then try again.",
        btn_labels = "Start Over"
      )
    } else if (.gameState == "draw") {
      .generateStatement(session, object = "game", verb = "completed", description = "Game has ended in a draw.")
      confirmSweetAlert(
        session = session,
        inputId = "endGame",
        title = "Draw!",
        text = "Take a moment to review the concepts and then try again.",
        btn_labels = "Start Over"
      )
    }
    if (is.null(success) || length(success) == 0) {
      updateButton(
        session = session,
        inputId = "submit",
        disabled = FALSE
      )
    } else{
      updateButton(
        session = session,
        inputId = "submit",
        disabled = TRUE
      )
    }
  })

  observeEvent(input$pages, {
    if (input$pages == "game") {
      if (!gameProgress) {
        shinyalert(
          title = "Player Select",
          text = "Select whether you want to play as O or X.",
          showConfirmButton = TRUE,
          confirmButtonText = "Play as X",
          showCancelButton = TRUE,
          cancelButtonText = "Play as O"
        )
        gameProgress <<- TRUE
      }
    }
    .generateStatement(session, verb = "experienced", description = paste0("Navigated to ", input$pages, " tab."))
  }, ignoreInit = TRUE)

  observeEvent(input$endGame, {
    .generateStatement(session, object = "endGame", verb = "interacted", description = paste("Game has been reset."))
    .gameReset()
  })

  observeEvent(input$shinyalert, {
    if (input$shinyalert == TRUE) {
      player <<- "X"
      opponent <<- "O"
    }
    if (input$shinyalert == FALSE) {
      player <<- "O"
      opponent <<- "X"
    }

    .generateStatement(session, object = "shinyalert", verb = "interacted", description = paste0("User has selected player: ", player))

    output$player <- renderUI({
      return(paste0("You are playing as ", player, "."))
    })
  })
}

boastApp(ui = ui, server = server)

## End(Not run)
