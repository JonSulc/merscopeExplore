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
      ),
      bs4Dash::bs4SidebarMenuItem(
        "X-sample comparison",
        tabName = "x-sample_comparison"
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
          shiny::column(6,
            featureSelectUI("heatmap_x")
          ),
          shiny::column(6,
            featureSelectUI("heatmap_y")
          )
        ),
        shiny::fluidRow(
          shiny::column(3),
          shiny::column(6,
            shiny::plotOutput("heatmap")
          ),
          shiny::column(3)
        )
      ),
      bs4Dash::tabItem(
        "x-sample_comparison",
        shiny::fluidRow(
          shiny::column(4,
            shiny::selectInput(
              "x_sample",
              label = "Select sample to compare",
              choices = NULL
            ),
            shinyWidgets::actionBttn(
              "load_x_sample",
              label = "Load data"
            )
          ),
          shiny::column(8,
            shiny::plotOutput("x_sample")
          )
        )
      )
    )
  )
)
