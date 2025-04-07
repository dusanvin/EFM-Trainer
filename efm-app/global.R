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
library(markdown)



gsheetID <- "1DOhttBbZnAmYLWvToB5GuCTh9tMGCdE_aCOziFA7Iv4"

sidebar <- shinydashboard::dashboardSidebar(
  collapsed = FALSE,
  width = 230,
  tags$p("Testen Sie Ihr Wissen zu empirischen Forschungsmethoden (EFM).",
         style = "padding: 10px; padding-left:15px; font-size: 90%; margin-bottom: 0; margin-top:10px;"),
  tags$p("Wählen Sie zwischen der Art des Test und des Feedbacks aus. Vertiefen Sie das Gelernte, indem Sie neue Aufgaben bearbeiten. ",
         style = "padding-right: 10px; padding-left:15px; font-size: 90%;"),
  selectInput(
    "selectTest",
    "Art des Tests",
    c("t-Test", "Paired t-Test", "Korrelation", "Regression", "log. Regression", "ANOVA", "Chi-Quadrat")
  ),
  div(
    style = "padding-top: 0 !important;",
    checkboxInput(inputId = "checkPrereq", label = "Voraussetzungen testen", value = FALSE, width = NULL)
  ),
  
  # Add toggle for feedback mode
  radioButtons(
    inputId = "feedbackMode",
    label = "Art des Feedbacks",
    choices = c("Statisch" = "static", "Dynamisch" = "dynamic"),
    selected = "static"  # Default value
  ),

  tags$div(
    tags$p("Übung", style = "margin-left: 15px; margin-top: 25px; font-weight: bold;"),
    tags$p("Wählen Sie eine neue Aufgabe, um Ihr Wissen (erneut) zu testen.", style = "margin-left: 15px; margin-right: 15px; margin-top: 10px; margin-bottom:10px; font-size: 90%;"),
    shinyBS::bsButton("newTaskButton", label = "Neue Aufgabe", block = FALSE, style = "primary")
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
                                h3("Ergebnisse:", style = "margin-top: 0; margin-bottom: 0.5em; font-size: 16px; font-weight: 700;"),
                                textOutput("textQuestion"),
                                hidden(textAreaInput("solutionInput", label = ("Nutzen Sie das Textfeld zur Eingabe:"), value = "", width = '100%', height = '160%')),
                                hidden(shinyBS::bsButton("showSolutionButton", label = "Lösung anzeigen", block = F, style="success")),
    ))),
    hidden(div(id = "box3", box(width = 12, 
      uiOutput("textSolution")
    ))),
  ))