#' plot_longitudinal_raw_by_age
#'
#' @param data_long 
#' @param variable 
#' @param age_var 
#' @param sub_var 
#' @param wave_var 
#'
#' @return
#' @export
#' @import ggplot2 purrr tidyr broom dplyr
#' @examples
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
#' @param data_long 
#' @param variable 
#' @param age_var 
#' @param sub_var 
#' @param wave_var 
#'
#' @return
#' @export
#' @import ggplot2 purrr tidyr broom dplyr
#' @examples
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

