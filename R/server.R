server <- function(input, output, session) {
  observe({
    choices <- list.files(input$datadir)
    choices <- choices[grepl("cell_by_gene[.]csv$", choices)]
    updateSelectInput(
      inputId = "selected_data",
      choices = choices
    )
  }) |>
    bindEvent(input$datadir)

  values <- reactiveValues()
  observe({
    print(sprintf("Loading %s", file.path(input$datadir, input$selected_data)))
    values$expression_data <-
      data.table::fread(file.path(input$datadir, input$selected_data),
                        drop = "cell")
  }) |>
    bindEvent(input$load_data)

  observe({
    values$features <- data.table::data.table(
      Feature = colnames(values$expression_data)
    )[
      ,
      .(Feature,
        "N cells express" = colSums(values$expression_data != 0),
        "N cells without" = colSums(values$expression_data == 0)
      )
    ]
  }) |>
    bindEvent(values$expression_data)

  output$features <- DT::renderDT(
    DT::datatable(
      values$features,
      filter = "top",
      selection = "single"
    )
  )

  output$histogram <- renderPlot({
    req(values$expression_data)
    req(input$features_rows_selected)
    feature_name <-
      colnames(values$expression_data)[input$features_rows_selected]
    if (input$remove_zeros)
      ggplot2::ggplot(values$expression_data[get(feature_name) != 0],
                      ggplot2::aes(x = .data[[feature_name]])) +
      ggplot2::geom_histogram(binwidth = 1)
    else
      ggplot2::ggplot(values$expression_data,
                      ggplot2::aes(x = .data[[feature_name]])) +
      ggplot2::geom_histogram(binwidth = 1)
  }) |>
    bindEvent(input$features_rows_selected,
              input$remove_zeros)
}
