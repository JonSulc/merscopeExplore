
server <- function(input, output, session) {
  observe({
    choices <- list.files(input$datadir)
    choices <- choices[grepl("(cell_by_gene|exprMat_file|cell_feature_matrix)[.]csv$", choices)]
    updateSelectInput(
      inputId = "selected_data",
      choices = choices
    )
    updateSelectInput(
      inputId = "x_sample",
      choices = choices
    )
  }) |>
    bindEvent(input$datadir)

  values <- reactiveValues()
  observe({
    print(sprintf("Loading %s", file.path(input$datadir, input$selected_data)))
    values$expression_data <-
      data.table::fread(file.path(input$datadir, input$selected_data),
                        drop = c("cell", "fov", "cell_ID"))
    if (grepl("exprMat_file", input$selected_data))
      values$expression_data <-
        values$expression_data[rowSums(values$expression_data) < 4000]
  }) |>
    bindEvent(input$load_data)

  observe({
    values$features <- data.table::data.table(
      Feature = colnames(values$expression_data)
    )[
      ,
      .(Feature,
        "N cells express" = colSums(values$expression_data != 0),
        "N cells without" = colSums(values$expression_data == 0),
        "Prop. cells express" = colSums(values$expression_data != 0) /
          nrow(values$expression_data)
      )
    ]
  }) |>
    bindEvent(values$expression_data)

  featureSelectServer("histogram", values)
  featureSelectServer("heatmap_x", values)
  featureSelectServer("heatmap_y", values)

  output$histogram <- renderPlot({
    req(values$expression_data)
    req(input[["histogram-features_rows_selected"]])
  feature_name <-
    colnames(values$expression_data)[input[["histogram-features_rows_selected"]]]
  feature_hist(values$expression_data,
               feature_name,
               log_y = input$log_y)
  }) |>
    bindEvent(input[["histogram-features_rows_selected"]],
              input$log_y)

  output$heatmap <- renderPlot({
    req(values$expression_data)
    req(input[["heatmap_x-features_rows_selected"]])
    req(input[["heatmap_y-features_rows_selected"]])
    feature_name_x <-
      colnames(values$expression_data)[input[["heatmap_x-features_rows_selected"]]]
    feature_name_y <-
      colnames(values$expression_data)[input[["heatmap_y-features_rows_selected"]]]
    feature_2d(values$expression_data,
                    feature_name_x,
                    feature_name_y,
                    log_y = input$log_y)
  }) |>
    bindEvent(input[["heatmap_x-features_rows_selected"]],
              input[["heatmap_y-features_rows_selected"]],
              input$log_y)

  output$heatmap_cor <- renderPrint({
    req(values$expression_data)
    req(input[["heatmap_x-features_rows_selected"]])
    req(input[["heatmap_y-features_rows_selected"]])
    feature_name_x <-
      colnames(values$expression_data)[input[["heatmap_x-features_rows_selected"]]]
    feature_name_y <-
      colnames(values$expression_data)[input[["heatmap_y-features_rows_selected"]]]
    sprintf("Correlation:
Among cells expressing either: %.2f
Overall: %.2f",
            cor(values$expression_data[, ..feature_name_x],
                values$expression_data[, ..feature_name_y]),
            cor(values$expression_data[!!feature_name_x != 0 & !!feature_name_y !=0, ..feature_name_x],
                values$expression_data[!!feature_name_x != 0 & !!feature_name_y !=0, ..feature_name_y])) |>
      cat()
  }) |>
    bindEvent(input[["heatmap_x-features_rows_selected"]],
              input[["heatmap_y-features_rows_selected"]])

  observe({
    print(sprintf("Loading %s", file.path(input$datadir, input$x_sample)))
    values$x_sample <-
      data.table::fread(file.path(input$datadir, input$x_sample),
                        drop = c("cell", "fov", "cell_ID"))
    if (grepl("exprMat_file", input$x_sample))
      values$x_sample <-
        values$x_sample[rowSums(values$x_sample) < 1e5]
  }) |>
    bindEvent(input$load_x_sample)

  observe({
    req(values$expression_data)
    req(values$x_sample)
    common_features <- intersect(
      colnames(values$expression_data),
      colnames(values$x_sample)
    )
    values$x_sample_summary <- data.table::data.table(
      x = colSums(values$expression_data[
        ,
        ..common_features
      ]),
      y = colSums(values$x_sample[
        ,
        ..common_features
      ])
    )
  }) |>
    bindEvent(values$expression_data,
              values$x_sample)

  output$x_sample_cor <- shiny::renderText({
    req(values$x_sample_summary)
    sprintf("Correlation: %s", cor(values$x_sample_summary$x,
                                   values$x_sample_summary$y))
  })

  output$x_sample <- renderPlot({
    req(values$x_sample_summary)

    ggplot2::ggplot(values$x_sample_summary,
                    ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point() +
      ggplot2::scale_x_continuous(trans = "log2") +
      ggplot2::scale_y_continuous(trans = "log2")
  })
}
