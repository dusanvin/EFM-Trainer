checkPackages <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      library(package, character.only = TRUE)
    }
  }
}

checkPackages(c("shiny", "shinydashboard", "shinyBS", "truncnorm", "jmv", "wakefield", "googlesheets4", "dplyr", "shinyjs", "SimCorrMix", "shinybusy", "jsonlite", "lubridate", "reader", "MASS", "stringr", "httr"))

googlesheets4::gs4_auth(
  path = ".secrets/able-hull-366008-48e78f3bc110.json",
  email = "efm.trainer.data@gmail.com"
)

gsheetID <- "1tnQMaMutYZvIvEVrZGwQ44GKFK8VSb4-ZO4q0pAeSp4"


sidebar <- shinydashboard::dashboardSidebar(
  collapsed = FALSE,
  width = 160,
  selectInput(
    "selectTest",
    "Test ausw\u00E4hlen",
    c("t-Test", "Paired t-Test","Korrelation", "Regression", "log. Regression", "ANOVA","Chi-Quadrat", "t-Test (GPT)")
  ),
  checkboxInput(inputId = "checkPrereq", label = "Voraussetzungen testen", value = FALSE, width = NULL),
  shinyBS::bsButton("newTaskButton", label = "Neue Aufgabe", block = F, style="success")
  
)

body <- shinydashboard::dashboardBody(
  fluidRow(
    useShinyjs(),
    hidden(div(id = "box1", box(width = 12, 
     textOutput("textTask"),
     hidden(div(id = "scaleTextID",uiOutput("scaleText"), style = "font-size:100%")),
     hidden(div(id = "scaleTableID",tableOutput("scaleTable"), style = "font-size:80%")),
     verbatimTextOutput("tTest"),
     
     hidden(div(id = "plot1ID",style = "display:inline-block; float:left", plotOutput("plot1", height = "auto", width = "auto"))),
    ))),
    
    hidden(div(id = "box2", box(width = 12, 
     textOutput("textQuestion"),
     hidden(textAreaInput("solutionInput", label = ("Lösung eingeben:"), value = "", width = '100%', height = '160%')),
     hidden(shinyBS::bsButton("showSolutionButton", label = "Lösung anzeigen", block = F, style="success")),
    ))),
    hidden(div(id = "box3", box(width = 12, 
     uiOutput("textSolution")
    ))),
))


