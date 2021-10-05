#' plot_longitudinal_raw_by_age
#'
#' @param data_long Data in person-period format
#' @param variable Variable to plot
#' @param age_var Name of age variable
#' @param sub_var Name of subject ID variable
#' @param wave_var Name of wave variable
#' @concept visualization
#' @return plot with y-axis of variable and x-axis of age. lines are connected within sub_var and colors are estimated slopes (i.e., estimated slopes represent change in variable per year)
#' @export
#' @import ggplot2 purrr tidyr broom dplyr glue
#' @examples
#' df_fake <- data.frame(sub = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
#'                  wave = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
#'                  age = c(50, 60, 70, 20, 35, 40, 35, 37, 40),
#'                  test_var = c(60, 55, 50, 30, 40, 35, 45, 43, 40))
#' plot_longitudinal_raw_by_age(df_fake, 'test_var')
plot_longitudinal_raw_by_age <- function(data_long, variable, age_var = 'age', sub_var = 'sub', wave_var = 'wave') {
  df_fig <- data_long %>%
    group_by(eval(as.name(sub_var))) %>%
    nest() %>%
    mutate(estimates = map(data, function(x) {
      lm(eval(as.formula(glue("{variable} ~ {age_var}"))), x) %>%
        tidy() %>% select(term, estimate)
    })) %>%
    unnest(cols = c(data)) %>%
    ungroup() %>%
    unnest(cols = estimates) %>%
    mutate(term = ifelse(term == '(Intercept)', 'b0', 'b1')) %>%
    pivot_wider(., id_cols = all_of(c(sub_var, age_var, wave_var, variable)), names_from = "term", values_from = "estimate") %>%
    mutate(y_pred = b0 + b1 * eval(as.name(age_var)))
  max_value <- max(abs(na.omit(df_fig$b1)))
  fig <- ggplot(df_fig, aes(x = eval(as.name(age_var)), y = as.numeric(eval(as.name(variable))), group = eval(as.name(sub_var)), color = b1)) +
    geom_line(size = 1) +
    geom_point() +
    labs(
      x = "\nAge",
      y = paste0(variable, "\n"),
      color = "Slope\n"
    ) +
    theme(plot.caption = element_text(hjust = 0)) +
    scale_color_distiller(palette = "RdBu", limits = c(-max_value, max_value)) +
    scale_x_continuous(breaks = seq(0, 200, 10)) +
    theme_minimal()
  return(fig)
}

#' plot_longitudinal_slopes_by_age
#'
#' @param data_long Data in person-period format
#' @param variable Variable to plot
#' @param age_var Name of age variable
#' @param sub_var Name of subject ID variable
#' @param wave_var Name of wave variable
#' @concept visualization
#' @return plot with y-axis of variable and x-axis of age. lines and colors are estimated slopes within sub_vars (i.e., slopes represent change in variable over time)
#' @export
#' @import ggplot2 purrr tidyr broom dplyr
#' @examples
#' df_fake <- data.frame(sub = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
#'                  wave = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
#'                  age = c(50, 60, 70, 20, 35, 40, 35, 37, 40),
#'                  test_var = c(60, 55, 50, 30, 40, 35, 45, 43, 40))
#' plot_longitudinal_slopes_by_age(df_fake, 'test_var')
plot_longitudinal_slopes_by_age <- function(data_long, variable, age_var = 'age', sub_var = 'sub', wave_var = 'wave') {
  df_fig <- data_long %>%
    group_by(eval(as.name(sub_var))) %>%
    nest() %>%
    mutate(estimates = map(data, function(x) {
      lm(eval(as.formula(glue("{variable} ~ {age_var}"))), x) %>%
        tidy() %>% select(term, estimate)
    })) %>%
    unnest(cols = c(data)) %>%
    ungroup() %>%
    unnest(cols = estimates) %>%
    mutate(term = ifelse(term == '(Intercept)', 'b0', 'b1')) %>%
    pivot_wider(., id_cols = all_of(c(sub_var, age_var, wave_var, variable)), names_from = "term", values_from = "estimate") %>%
    mutate(y_pred = b0 + b1 * eval(as.name(age_var)))
  max_value <- max(abs(na.omit(df_fig$b1)))
  fig <- ggplot(df_fig, aes(x = eval(as.name(age_var)), y = as.numeric(eval(as.name(variable))))) +
    geom_line(aes(y = y_pred, color = b1, group = eval(as.name(sub_var))), size = 1) +
    geom_point(size = .5, alpha = 0.5) +
    geom_smooth(method = 'lm', color = 'black') +
    labs(
      x = "\nAge",
      y = paste0(variable, "\n"),
      color = "Slope\n"
    ) +
    theme(plot.caption = element_text(hjust = 0)) +
    scale_color_distiller(palette = "RdBu", limits = c(-max_value, max_value)) +
    scale_x_continuous(breaks = seq(0, 200, 10)) +
    theme_minimal() 
  return(fig)
}

