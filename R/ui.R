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
      "log_y",
      "Y axis log-scale",
      value = TRUE
    )
  ),
  sidebar = bs4Dash::bs4DashSidebar(
    skin = "dark",
    inputId = "sidebar",
    bs4Dash::bs4SidebarMenu(
      id = "menu",
      bs4Dash::bs4SidebarMenuItem(
        "Feature expression",
        tabName = "feature_expression"
      ),
      bs4Dash::bs4SidebarMenuItem(
        "Feature expression (heatmap)",
        tabName = "feature_heatmap"
      )
    )
  ),
  body = bs4Dash::bs4DashBody(
    bs4Dash::bs4TabItems(
      bs4Dash::tabItem(
        "feature_expression",
        featureSelectUI("histogram"),
        shiny::plotOutput("histogram")
      ),
      bs4Dash::tabItem(
        "feature_heatmap",
        shiny::fluidRow(
          column(6,
            featureSelectUI("heatmap_x")
          ),
          column(6,
            featureSelectUI("heatmap_y")
          )
        ),
        shiny::fluidRow(
          column(3),
          column(6,
            shiny::plotOutput("heatmap")
          ),
          column(3)
        )
      )
    )
  )
)
