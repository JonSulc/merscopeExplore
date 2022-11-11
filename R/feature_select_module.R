featureSelectUI <- function(id) {
  ns <- shiny::NS(id)

  DT::DTOutput(ns("features"))
}

featureSelectServer <- function(id, values) {
  ns <- shiny::NS(id)

  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$features <- DT::renderDT(
        DT::datatable(
          values$features,
          filter = "top",
          selection = "single"
        )
      )
    }
  )
}
