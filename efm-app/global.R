# Import
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
library(dotenv)

# Anbindung
dotenv::load_dot_env()
Sys.setenv(OPENAI_API_KEY = Sys.getenv("OPENAI_API_KEY"))
gs4_auth(path = Sys.getenv("GSHEET_PATH"))
gsheetID <- Sys.getenv("GSHEET_ID")

# Sidebar
sidebar <- shinydashboard::dashboardSidebar(
  useShinyjs(),
  collapsed = FALSE,
  width = 230,
  div(
    style="margin-bottom: 1em;",
    tags$p("Testen Sie Ihr Wissen zu empirischen Forschungsmethoden (EFM).",
    style =
      "
      padding: 10px; 
      padding-left:15px; 
      font-size: 90%; 
      margin-bottom: 0; 
      margin-top:10px;
      "
    ),
    tags$p(
      "
        Wählen Sie zwischen der Art des Test und des Feedbacks aus. 
        Vertiefen Sie das Gelernte, indem Sie neue Aufgaben bearbeiten.
      "
      ,
      style = "
        padding-right: 10px; 
        padding-left:15px; 
        font-size: 90%;"
    ),
    tags$a(
      class="sidebar-link-digillab",
      style = "
        padding-right: 10px; 
        padding-left:15px; 
        font-size: 90%;
      ",
      href = "#",
      onclick = "Shiny.setInputValue('showHelp', Math.random())",  # Trigger wie bei 'Über'
      "Wie nutze ich den EFM-Trainer?"
    )
  ),
  selectInput(
    "selectTest",
    "Art des Tests",
    c(
      "t-Test",
      "Paired t-Test",
      "Korrelation",
      "Regression",
      "log. Regression",
      "ANOVA", "Chi-Quadrat"
    )
  ),
  div(
    class="prereqs-digillab",
    checkboxInput(
      inputId = "checkPrereq",
      label = "Voraussetzungen testen",
      value = FALSE,
      width = NULL
    )
  ),
  # Add toggle for feedback mode
  radioButtons(
    inputId = "feedbackMode",
    label = "Art des Feedbacks",
    choices = c("Statisch" = "static", "Dynamisch" = "dynamic"),
    selected = "static"  # Default value
  ),

  tags$div(
    tags$p(
      "Übung",
      style = "
        margin-left: 15px; 
        margin-top: 25px; 
        font-weight: bold;
      "
    ),
    tags$p("
      Wählen Sie eine neue Aufgabe, um Ihr Wissen (erneut) zu testen.",
      style = "
        margin-left: 15px; 
        margin-right: 15px; 
        margin-top: 10px; 
        margin-bottom:10px; 
        font-size: 90%;
      "
    ),
    shinyBS::bsButton(
      "newTaskButton",
      label = "Neue Aufgabe",
      block = FALSE,
      style = "primary"
    ),
    tags$div(
      style = "
        position: absolute; 
        bottom: 20px; 
        width: 100%;
        text-align: center;
      ",
      # Logo
      tags$a(
        href = "https://digillab.uni-augsburg.de",
        target = "_blank",
        tags$img(
          src = "logo.png",
          height = "70px",
          style = "
            margin-left: 8px;
          "
        )
      ),
      # Links
      tags$div(
        style = "
          font-size: 80%; 
          display: flex; 
          justify-content: center; 
          gap: 10px;"
        ,
        tags$a(
          class="misc-links",
          href = "https://www.uni-augsburg.de/de/impressum/datenschutz/",
          "Datenschutz",
          target = "_blank"
        ),
        tags$span("|"),
        tags$a(
          class="misc-links",
          href = "https://www.uni-augsburg.de/de/impressum/",
          "Impressum",
          target = "_blank"
        ),
        tags$span("|"),
        tags$a(
          class = "misc-links",
          href = "#",
          onclick = "Shiny.setInputValue('showAbout', Math.random())",  # Trigger für UI-Wechsel
          "Über"
        )
      )
    )
  )
)

# Body
body <- shinydashboard::dashboardBody(
                                      useShinyjs(),
                                      fluidRow(
                                        hidden(
                                          div(
                                            id = "box1",
                                            box(width = 12,
                                              div(
                                                style="padding:1em;",
                                                h3(
                                                  "Aufgabe:",
                                                  style = "
                                                    margin-top: 0; 
                                                    margin-bottom: 0.5em; 
                                                    font-size: 16px; 
                                                    font-weight: 700;
                                                  "
                                                ),
                                                div(
                                                  style = "
                                                  margin-bottom: 0.5em;
                                                ",
                                                  textOutput("textTask")
                                                ),
                                                hidden(
                                                  div(
                                                    id = "scaleTextID",
                                                    uiOutput("scaleText"),
                                                    style = "
                                                      font-size:100%;
                                                      margin-bottom: 0.5em;"
                                                  )
                                                ),
                                                hidden(div(id = "scaleTableID",tableOutput("scaleTable"), style = "margin-bottom: 1em;font-size:80%;border: 1px solid #ccc;border-radius: 4px;max-width: fit-content;")),
                                                div(
                                                  style = "max-width: fit-content;", 
                                                  verbatimTextOutput("tTest")
                                                ),
                                                hidden(div(id = "plot1ID",style = "display:inline-block; float:left", plotOutput("plot1", height = "auto", width = "auto"))),
                                              )
                                            )
                                          )
                                        ),
                                        hidden(div(id = "box2", box(width = 12, 
                                          div(
                                            style = "
                                                  margin-bottom: 0.5em;
                                                  padding: 1em;
                                                ",
                                            h3(
                                            "Bericht:", style = "
                                            margin-top: 0; 
                                            margin-bottom: 0.5em; 
                                            font-size: 16px; 
                                            font-weight: 700;"
                                            ),
                                            # textOutput("textQuestion"),
                                            hidden(textAreaInput("solutionInput", label = ("Wie würden Sie diese Ergebnisse berichten?"), value = "", width = '100%', height = '160%')),
                                            hidden(shinyBS::bsButton("showSolutionButton", label = "Lösung anzeigen", block = F, style="success")),
                                          )
                                        ))),
                                        hidden(div(id = "box3", box(width = 12, 
                                          div(
                                            style = "
                                                  margin-bottom: 0.5em;
                                                  padding: 1em 1em 0 1em;
                                                ",
                                            uiOutput("textSolution")
                                          )
                                        ))),
                                        hidden(
                                                div(id = "aboutBox", class = "about-page", style = "max-width: 700px;",
                                                  box(width = 12,
                                                    div(
                                                      style="padding-left: 1em; padding-right: 1em;",
                                                      h3("Über"),
                                                      p(
                                                        class="margin-bottom:0.3em;",
                                                        "Der Empirische Forschungsmethoden (EFM)-Trainer ist eine interaktive Applikation (App) zur Vermittlung und Anwendung empirischer Forschungsmethoden. 
                                                      Die App integriert statistische Testverfahren mit einer ChatGPT-Schnittstelle für KI-gestütztes Feedback."),
                                                      p("Ursprünglich an der Ludiwg-Maximilians-Universität (LMU) von Prof. Dr. Michael Sailer und Prof. Dr. Michael Stadler konzipiert, wurde sie im Rahmen des DigiLLabs an der Universität Augsburg mittels eins Projektantrags vom Lehrstuhl für Learning Analytics and Educational Data Mining weiterentwickelt."),
                                                      # tags$hr()
                                                    ),
                                                    div(
                                                      style="padding-left: 1em; padding-right: 1em; padding-bottom: 1em;",
                                                      h3("Kontakt"),
                                                      p(
                                                        class="margin-bottom:0.3em;",
                                                        "Wenden Sie sich bei",
                                                        span("inhaltlichen Rückfragen", style = "font-weight: 700;"),
                                                        "an:"
                                                      ),
                                                      tags$a(href = "mailto:michael.sailer@uni-a.de", "Prof. Dr. Michael Sailer"),
                                                      p("Lehrstuhl für ",
                                                        tags$a(href = "https://www.uni-augsburg.de/de/fakultaet/philsoz/fakultat/learning-analytics/", target = "_blank", "Learning Analytics and Educational Data Mining"),
                                                      ),
                                                      p(
                                                        "Wenden Sie sich bei",
                                                        span("technischen Rückfragen", style = "font-weight: 700;"),
                                                        "an:"
                                                      ),
                                                      tags$a(href = "mailto:vincent.dusanek@uni-a.de", "Vincent Dusanek"),
                                                      p("",
                                                        tags$a(href = "https://digillab.uni-augsburg.de", target = "_blank", "Zentrum für digitales Lehren und Lernen (DigiLLab)"),
                                                      ),
                                                    ),
                                                    div(
                                                      style="padding-left: 1em; padding-right: 1em; padding-bottom: 1em;",
                                                      h3(
                                                        style="margin-top:0;",
                                                        "Lizenz"),
                                                      p("Version 1.1: 04:2025"),
                                                      p(
                                                        class="margin-bottom:0.3em;",
                                                        "Der Empirische Forschungsmethoden (EFM)-Trainer ist eine Applikation zur Vermittlung und Anwendung empirischer Forschungsmethoden. Weiterentwickelt von Vincent Dusanek und Norman Szabo für DigiLLab im Rahmen eines Projektantrags des Lehrstuhls für",
                                                          tags$a(href = "https://www.uni-augsburg.de/de/fakultaet/philsoz/fakultat/learning-analytics/", target = "_blank", "Learning Analytics and Educational Data Mining"),
                                                          "2025,",
                                                          tags$a(href = "https://tlo.mit.edu/understand-ip/exploring-mit-open-source-license-comprehensive-guide", target = "_blank", "MIT-Lizenz"),"."
                                                      ),
                                                      tags$hr(),
                                                      actionButton("backButton", "Zurück zur App", icon = icon("arrow-left"))
                                                    )
                                                  )
                                                )
                                              ),
                                              hidden(
                                                div(id = "hilfeBox", class = "hilfe-page", style = "max-width: auto;",
                                                  box(width = 12,
                                                    div(
                                                      style="padding-left: 1em; padding-right: 1em;",
                                                      h3("Anleitung"),
                                                      
                                                      # eingebettetes YouTube-Video
                                                      # Responsive YouTube-Embed
                                                      tags$ol(
                                                        tags$li("Wählen Sie einen Test aus dem Dropdown-Menü aus."),
                                                        tags$li("Falls gewünscht, aktivieren Sie die Option 'Voraussetzungen testen'."),
                                                        tags$li("Entscheiden Sie sich zwischen statischem und dynamischem Feedback."),
                                                        tags$li("Klicken Sie auf 'Neue Aufgabe', um eine neue Übungsfrage zu generieren."),
                                                        tags$li("Tragen Sie Ihre Lösung ein und klicken Sie auf 'Lösung anzeigen', um Feedback zu erhalten. Wiederholen Sie diesen Schritt, um dynamisches/ statisches Feedback zu erhalten."),
                                                        tags$li("Nutzen Sie das Feedback, um Ihr Wissen zu vertiefen und Ihre Kompetenzen zu verbessern.")
                                                      ),
                                                      tags$div(
                                                        style = "position: relative; padding-bottom: 56.25%; height: 0; overflow: hidden; margin-bottom: 1em;",
                                                        tags$iframe(
                                                          src = "https://www.youtube.com/embed/lTK4_UYtie0",
                                                          style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%;",
                                                          frameborder = "0",
                                                          allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
                                                          allowfullscreen = NA
                                                        )
                                                      ),
                                                      tags$hr(),
                                                      actionButton("backFromHelpButton", "Zurück zur App", icon = icon("arrow-left"))
                                                    )
        )
      )
    ),
  ))