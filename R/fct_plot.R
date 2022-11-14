#' @import data.table
feature_hist <- function(expression_data,
                         feature_name,
                         log_y = FALSE) {
  p <- ggplot2::ggplot(expression_data[
    ,
    .(transcripts = get(feature_name))
  ][
    ,
    .(counts = table(transcripts))
  ][
    ,
    .(N_transcripts = as.numeric(counts.transcripts),
      N_cells       = counts.N)
  ],
  ggplot2::aes(x = N_transcripts, y = N_cells)) +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(trans = "log1p")
  if (!log_y)
    return(p)
  p + ggplot2::scale_y_continuous(trans = "log1p")
}

feature_2d <- function(expression_data,
                       feature_name_x,
                       feature_name_y,
                       log_y = FALSE) {
  ggplot2::ggplot(expression_data[
    ,
    .(x = get(feature_name_x),
      y = get(feature_name_y))
  ],
  ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_hex() +
    ggplot2::scale_x_continuous(trans = "log1p") +
    ggplot2::scale_y_continuous(trans = "log1p")
}
