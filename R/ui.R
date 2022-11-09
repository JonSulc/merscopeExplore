ui <- bs4Dash::bs4DashPage(
  header = bs4Dash::bs4DashNavbar(
    title = "Merscope data",
    shiny::textInput(
      "datadir",
      label = "Path to data",
      value = "~/merscope/data"
    ),
    shiny::selectInput(
      "selected_data",
      label = "Select sample",
      choices = NULL
    ),
    shinyWidgets::actionBttn(
      "load_data",
      label = "Load data"
    ),
    shiny::checkboxInput(
      "remove_zeros",
      "Ignore zeros"
    )
  ),
  sidebar = bs4Dash::bs4DashSidebar(),
  body = bs4Dash::bs4DashBody(
    DT::DTOutput("features"),
    shiny::plotOutput("histogram")
  )
)
