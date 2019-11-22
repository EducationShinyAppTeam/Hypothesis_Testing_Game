packages <-
  c("shiny",
    "shinyWidgets",
    "shinydashboard",
    "shinyBS",
    "boastUtils",
    "shinyalert")

lapply(packages, library, character.only = TRUE)

GRID_SIZE <- 3
TILE_COUNT <- GRID_SIZE ^ 2

ui <- dashboardPage(
  skin = "blue",
  # Header
  dashboardHeader(
    title = "NHST Tic-Tac-Toe",
    titleWidth = 300,
    tags$li(
      class = "dropdown",
      tags$a(href = "https://shinyapps.science.psu.edu/", icon("home"))
    )
  ),
  # Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem(
        text = "Overview",
        tabName = "overview",
        icon = icon("dashboard")
      ),
      menuItem(
        text = "Pre-requisites",
        tabName = "prereq",
        icon = icon("book")
      ),
      menuItem(
        text = "Game",
        tabName = "game",
        icon = icon("gamepad")
      ),
      menuItem(
        text = "To Do",
        tabName = "toDo",
        icon = icon("tasks")
      )
    ),
    tags$div(class = "sidebar-logo", boastUtils::psu_eberly_logo("reversed"))
  ),
  #Pages
  dashboardBody(
    tabItems(
      #Overview Page
      tabItem(
        tabName = "overview",
        withMathJax(),
        h1("Null Hypothesis Significance Testing Tic-Tac-Toe"),
        p(
          "This app quizzes your knowledge of null hypothesis significance testing (NHST) concepts using a tic-tac-toe game."
        ),
        h2("Instructions"),
        p(
          "Why are the instructions for the game on a different page than the game? This strikes me as poor design."
        ),
        p("To play the game: "),
        tags$ol(
          tags$li("Review any pre-requiste ideas."),
          tags$li("Click the GO! button to go the game page."),
          tags$li("Select whether you'll play as the O's or the X's."),
          tags$li("Select the square that you want to place your marker."),
          tags$li(
            "Answer the question that is given. If you're correct, you get that square. If not, the computer will."
          ),
          tags$li(
            "Win by filling three squares in a row with your mark (i.e., when you have 3 of your marks line up horizontally, vertically, or diagonally)."
          )
        ),
        div(
          style = "text-align: center",
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
          "This version of the app was developed and coded by Neil J. Hatfield. Portions of the code used were written by Robert Carey III. The concept of the app stems from David Robinson (2017) and Ryan Voyack (2018).",
          br(),
          "The question bank was written by Dennis Pearl and Neil J. Hatfield.",
          br(),
          br(),
          br(),
          div(class = "updated", "Last Update: 11/20/2019 by NJH.")
        )
      ),
      #Prereq Page
      tabItem(
        tabName = "prereq",
        withMathJax(),
        h2("Pre-requisite Meanings"),
        p("What should go here? If anything?"),
        box(
          title = strong("First Pre-requisite meaning"),
          status = "primary",
          collapsible = TRUE,
          collapsed = TRUE,
          width = "100%",
          "Text for the first pre-requisite meaning. Replicate this format for subsequent pre-req meanings."
        ),
        p(
          "When you're ready to play the game, press the button below or use the sidebar menu."
        ),
        div(
          style = "text-align: center",
          bsButton(
            inputId = "go2",
            label = "GO!",
            size = "large",
            icon = icon("bolt")
          )
        )
      ),
      #Game Page
      tabItem(
        tabName = "game",
        withMathJax(),
        useShinyalert(),
        h2("NHST Tic-Tac-Toe"),
        p(
          "To play, click on any one of the nine buttons that have a question mark. A question will appear to the right with possible answers. If you answer correctly, you will take the square; if not, the computer will take the square. Try your best to win the game and get three in a row!"
        ),
        h3(uiOutput("player")),
        fluidRow(
          column(
            4,
            h3("Game Board"),
            br(),
            uiOutput("gameBoard")
          ),
          column(
            8,
            h3("Question"),
            withMathJax(uiOutput("question")),
            uiOutput("extraOutput"),
            h3("Answer"),
            uiOutput("answer"),
            actionButton(
              inputId = "submit",
              label = "Submit",
              color = "primary",
              size = "large",
              style = "bordered",
              disabled = TRUE
            ),
            actionButton(
              inputId = "reset",
              label = "Reset Game",
              color = "primary",
              size = "large",
              style = "bordered"
            ),
            br(),
            uiOutput("trigger1"),
            uiOutput("trigger2")
          )
        )
      ),
      #To Do list
      tabItem(
        tabName = "toDo",
        withMathJax(),
        h2("Things to do:"),
        tags$ul(
          tags$li("√D Wrap app in dashboard"),
          tags$li("√D  Finish the button styling--no hover colors?"),
          tags$li("√D  Program the buttons to bring up MC questions"),
          tags$li(
            "√D  Program answering checking to update button's label and disable"
          ),
          tags$li("√D  Program reading in question/answer bank"),
          tags$li("√D  Program setting of questions"),
          tags$li("√D  Program the inclusion of the pictures"),
          tags$li("√D  Program Submit button"),
          tags$li("√D  Make score keeper"),
          tags$li("√D  Add win/lose alert"),
          tags$li("√D  Program Reset button"),
          tags$li("√D  Don't forget MathJax inclusion of template"),
          tags$li("√D  Program Go buttons"),
          tags$li("√D  Program Player Select"),
          tags$li("√D  Add the buttons in the upper right corner"),
          tags$li("√D  Set submit button to only allow for one guess"),
          tags$li("√D  Set the win/lose start over to activate a reset"),
          tags$li("√D  Reset needs to clear extraOutput"),
          tags$li("√D  Double check reset works"),
          tags$li("√D  Lose message doesn't want to appear"),
          tags$li("√D  top row, left column, diagonals works, others don't"),
          tags$li("√D  mid-game tab change"),
          tags$li("  Decide on the pre-req's page"),
          tags$li("  Do wording/concept check"),
          tags$li("  Remove the To Do List")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Variables
  activeBtn <- NA
  player <- NA
  opponent <- NA
  scoreMatrix <-
    matrix(data = rep.int(0, times = TILE_COUNT),
           nrow = GRID_SIZE,
           ncol = GRID_SIZE)
  gameProgress <- FALSE
  
  # Helper Functions
  .tileCoordinates <- function(tile = NULL, index = NULL) {
    
    row <- -1
    col <- -1
    
    # if: button tile is given, derive from id
    # else: derive from index
    if(!is.null(tile)){
      # oxo-[row]-[col]
      tile <- strsplit(tile, "-")[[1]]
      tile <- tile[-1] # remove oxo
      
      row <- strtoi(tile[1])
      col <- strtoi(tile[2])
    } else {
      row <- (index - 1) %/% GRID_SIZE + 1
      col <- index - (GRID_SIZE * (row - 1))
    }
    
    coordinates <- list(
      "row" = row,
      "col" = col
    )
    
    return(coordinates) 
  }
  
  .tileIndex <- function(tile) {
    coords <- .tileCoordinates(tile)

    index = GRID_SIZE * (coords$row - 1) + coords$col
    
    return(index)
  }
  
  .btnReset <- function(index) {
    coords <- .tileCoordinates(index = index)
    id <- paste0("oxo-", coords$row, "-", coords$col)
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
    
    if(GRID_SIZE > 1){
      mainD <- sum(diag(mat))
      rotated <- apply(t(mat), 2, rev)
      offD <- sum(diag(rotated))
      
      if (GRID_SIZE %in% rows ||
          GRID_SIZE %in% cols || mainD == GRID_SIZE || offD == GRID_SIZE) {
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
    gameSet <<- questionBank[qSelected, ]
    
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
      matrix(data = rep.int(0, times = TILE_COUNT),
             nrow = GRID_SIZE,
             ncol = GRID_SIZE)
    gameProgress <- FALSE
    activeBtn <- NA
    
    updateButton(session = session,
                 inputId = "submit",
                 disabled = TRUE)
  }
  
  # Define navigation buttons
  observeEvent(input$go1, {
    updateTabItems(session, "tabs", "game")
  })
  
  observeEvent(input$go2, {
    updateTabItems(session, "tabs", "game")
  })
  
  # Read in data and generate the first subset
  questionBank <-
    read.csv("questionBank.csv",
             stringsAsFactors = FALSE,
             as.is = TRUE)
  qSelected <-
    sample(seq_len(nrow(questionBank)), size = TILE_COUNT, replace = FALSE)
  gameSet <- questionBank[qSelected, ]
  
  # Program the Reset Button
  observeEvent(input$reset, {
    .gameReset()
  })
  
  # Render Game Board / Attach Observers
  output$gameBoard <- renderUI({
    board <- list()
    index <- 1
    
    sapply(1:GRID_SIZE, function(row) {
      sapply(1:GRID_SIZE, function(column) {
        id <- paste0("oxo-", row, "-", column)
        
        board[[index]] <<- actionButton(
          inputId = paste0("oxo-", row, "-", column),
          label = "?",
          color = "primary",
          style = "bordered",
          class = "btn-ttt"
        )
        
        observeEvent(session$input[[id]], {
          activeBtn <<- id
          .boardBtn(id)
        })
        
        index <<- index + 1
      })
    })
    
    board
  })
  
  # Program Submit Button
  observeEvent(input$submit, {
    index <- .tileIndex(activeBtn)
    
    if (gameSet[index, "format"] == "numeric") {
      answer <- gameSet[index, "answer"]
    } else {
      answer <- gameSet[index, gameSet[index, "answer"]]
    }
    
    if (input$ans == answer) {
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
      scoreMatrix <<- .score(scoreMatrix, activeBtn, -1)
    }
    if (.gameCheck(scoreMatrix) == "win") {
      confirmSweetAlert(
        session = session,
        inputId = "endGame",
        title = "You Win!",
        text = "You've gotten three in a row. Start over and play a new game.",
        btn_labels = "Start Over"
      )
    } else if (.gameCheck(scoreMatrix) == "lose") {
      confirmSweetAlert(
        session = session,
        inputId = "endGame",
        title = "You lose :(",
        text = "Take a moment to review the concepts and then try again.",
        btn_labels = "Start Over"
      ) 
    } else if (.gameCheck(scoreMatrix) == "draw") {
      confirmSweetAlert(
        session = session,
        inputId = "endGame",
        title = "Draw!",
        text = "Take a moment to review the concepts and then try again.",
        btn_labels = "Start Over"
      ) 
    }
    updateButton(
      session = session,
      inputId = "submit",
      disabled = TRUE
    )
  })
  
  observeEvent(input$tabs, {
    if (input$tabs == "game") {
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
  }, ignoreInit = TRUE)
  
  observeEvent(input$endGame, {
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
    
    output$player <- renderUI({
      return(paste0("You are playing as ", player, "."))
    })
  })
}

boastApp(ui = ui, server = server)

## End(Not run)
