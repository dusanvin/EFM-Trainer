library(shiny)
library(shinydashboard)
library(shinyBS)
library(truncnorm)
library(jmv)
library(wakefield)
library(googlesheets4)
library(dplyr)
library(shinyjs)
library(SimCorrMix)
library(shinybusy)
library(jsonlite)
library(lubridate)
library(reader)
library(MASS)
library(httr)

gsheetID <- "1DOhttBbZnAmYLWvToB5GuCTh9tMGCdE_aCOziFA7Iv4"

sidebar <- shinydashboard::dashboardSidebar(
  collapsed = FALSE,
  width = 230,
  selectInput(
    "selectTest",
    "Test auswählen",
    c("t-Test", "Paired t-Test", "Korrelation", "Regression", "log. Regression", "ANOVA", "Chi-Quadrat")
  ),
  div(
    style = "margin-top: 0;",
    checkboxInput(inputId = "checkPrereq", label = "Voraussetzungen testen", value = FALSE, width = NULL)
  ),
  shinyBS::bsButton("newTaskButton", label = "Neue Aufgabe", block = FALSE, style = "primary"),
  
  # Add toggle for feedback mode
  radioButtons(
    inputId = "feedbackMode",
    label = "Feedback-Modus wählen",
    choices = c("Static" = "static", "Dynamic" = "dynamic"),
    selected = "static"  # Default value
  )
)

body <- shinydashboard::dashboardBody(
  fluidRow(
    useShinyjs(),
    hidden(div(id = "box1", box(width = 12, 
                                h3("Aufgabe:", style = "margin-top: 0; margin-bottom: 0.5em; font-size: 16px; font-weight: 700;"),
                                div(style = "margin-bottom: 0.5em;", textOutput("textTask")),
                                hidden(div(id = "scaleTextID",uiOutput("scaleText"), style = "font-size:100%; margin-bottom: 0.5em;")),
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