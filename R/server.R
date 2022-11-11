
server <- function(input, output, session) {
  observe({
    choices <- list.files(input$datadir)
    choices <- choices[grepl("(cell_by_gene|exprMat_file|cell_feature_matrix)[.]csv$", choices)]
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
                        drop = c("cell", "fov", "cell_ID"))
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

  # output$features <- DT::renderDT(
  #   DT::datatable(
  #     values$features,
  #     filter = "top",
  #     selection = "single"
  #   )
  # )
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
}
