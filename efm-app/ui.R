ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "EFM-Trainer"),
  sidebar,
  body,
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  )
)
