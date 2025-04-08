ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(
    title = tags$img(
      src = "efm-logo.png",
      height = "35px",        # Höhe anpassen nach Bedarf
      style = "
      margin-top: 12px;
      "
    )
  ),
  sidebar,
  body,
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  )
)
