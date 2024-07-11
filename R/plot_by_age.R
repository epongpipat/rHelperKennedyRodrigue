#' plot_by_age
#'
#' @param data data.frame
#' @param age_var 
#' @param y_var 
#' @param age_label 
#' @param y_label 
#'
#' @return
#' @export
#'
#' @examples
#' df_fake <- data.frame(age = sample(20:100, 100, TRUE),
#'                       outcome = sample(1:100/100, 100, TRUE))
#' plot_by_age(df_fake, y_var = 'outcome')
plot_by_age <- function(data, age_var = 'age', y_var = NULL, age_label = 'Age', y_label = NULL) {
  ds_fig <- data.frame(x = data[, age_var],
                       y = data[, y_var])
  fig <- ggplot(ds_fig, aes(x, y)) +
    geom_point() +
    geom_smooth(method = 'lm', color = 'black') +
    theme_minimal() +
    scale_x_continuous(breaks = seq(from = 0, to = 100, by = 10)) +
    labs(x = age_label)
  if (is.null(y_label)) {
    fig <- fig + labs(y = y_var)
  } else {
    fig <- fig + labs(y = y_label)
  }
  return(fig)
}