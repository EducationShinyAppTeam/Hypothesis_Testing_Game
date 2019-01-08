library(shiny)
library(shinydashboard)
library(DT)
library(shinyBS)
library(shinyjs)
library(V8)
library(discrimARTs)
library(leaflet)
library(raster)
library(shinyWidgets)


shinyUI(dashboardPage(skin="blue",
              #Title
              dashboardHeader(title="Hypothesis Testing Game",titleWidth=250),
              #Sidebar
              dashboardSidebar(
                width = 260,
                sidebarMenu(id = "tabs",
                            
                            menuItem("Overview", tabName = "rules", icon = icon("dashboard")),
                            #menuItem("Part 1", tabName = "first", icon = icon("table")),
                            menuItem("Concept Check Game", tabName = "qqq", icon = icon("gamepad"))
                )),
              
              #Content within the tabs
              dashboardBody(
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css")
                ),
                tags$style(
                  type = "text/css",
                  ".content-wrapper,.right-side {
                  background-color: white;
                  }"
                ),
                tabItems(
                  tabItem(tabName = "rules",
                          tags$a(href='http://stat.psu.edu/',tags$img(src='PS-HOR-RGB-2C.png', align = "left", width = 180)),
                          br(),br(),br(),
                          
                          h3(strong("About: ")),
                          h4("This app quizzes your knowledge of hypothesis testing concepts using a tic-tac-toe game format."),
                          br(),
                          h3(strong("Instructions:")),
                          h4(tags$li("Click the tic-tac-toe image to begin.")),
                          h4(tags$li("To play the game, you will select the square that you want to place an X.")),
                          h4(tags$li("Then you will answer the question that is given, if you get it right, an X will go in the square you selected - if not, an O will go in that spot.")),
                          h4(tags$li("You can only submit an answer after choosing a spot on the image.")),
                          h4(tags$li("You are playing as the X's, the object of the game is to get 3 X's in a row. (i.e., When you have 3 X's line up horizontally, vertically, or diagonally).")),
                       
                          div(style = "text-align: center;",
                              bsButton(inputId = "go", label = "G O !", size = "large",icon = icon("bolt"))
                          ),
                          br(),
                          h3(strong("Acknowledgements:")),
                          h4("This app was originally conceived by David Robinson in 2017. In 2018, using the original question bank, the game was redesigned and the entire app was then reprogrammed and developed by Ryan Voyack.")
                          
                  ),
                  tabItem(tabName = "qqq",
                          div(style="display: inline-block;vertical-align:top;",
                              tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 19))
                          ),
                          div(style="display: inline-block;vertical-align:top;",
                              circleButton("info",icon = icon("info"), status = "myClass",size = "xs")
                          ),
                          fluidRow(
                            
                            div(style = "text-align: center;",
                                h4("If you answer correctly, You will receive an X in the square you chose, if not, it will be an O."),
                                h4("Try your best to win the game and get 3 X's in a row !"),
                                br()
                            )
                          ),
                          fluidRow(
                            # tags$style(
                            #   type = "text/css",
                            #   ".content-wrapper,.right-side {background-color: white;}"
                            # ),
                            
                            fluidRow(
                              column(4,
                                     leafletOutput('image'),
                                     br(),
                                     textOutput("warning"),
                                     textOutput("gameMessage")
                              ),
                              column(8,
                                     conditionalPanel("output.temp != 2",
                                                      conditionalPanel("input.image_click",
                                                                       uiOutput("CurrentQuestion"),
                                                                       uiOutput("CurrentQuestion.extra"),
                                                                       br(),
                                                                       br(),
                                                                       br()
                                                      ),
                                     textOutput("directions"),
                                     br()
                                     )
                              ),
                              column(2,
                                     bsButton(inputId = 'submit', label = 'Submit Answer')
                              ),
                              column(1,
                                     bsButton(inputId = "nextButton",label = "Next Question")
                                     )
                            ),
                            fluidRow(
                              column(width=12, offset = 5,
                                     bsButton(inputId="reset", label="Start new game")
                              )
                            )
                          )
                  )
                )
))
)


