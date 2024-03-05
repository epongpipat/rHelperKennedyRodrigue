#' get_descriptives_table
#' @description generate a descriptive statistics table with means and standard deviations overall and for each group
#' @concept data_wrangling
#' @param data input dataset
#' @param grouping_variable name of variable to group
#' @param grouping_values levels of the grouping variable
#' @param variables names of variables to obtain descriptive statistics (default: c('n', 'age', 'female', 'edu_years', 'cesd', 'mmse'))
#' @return data.frame of descriptive statistics table
#' @importFrom glue glue
#' @export
get_descriptives_table <- function(data, grouping_variable, grouping_values, variables = c('n', 'age', 'female', 'edu_years', 'cesd', 'mmse')) {
  df_demo <- data.frame(variable = NA,
                        'overall' = NA)
  for (i in 1:length(variables)) {
    df_demo[i, 'variable'] <- variables[i]
    if (variables[i] == 'n') {
      df_demo[i, 'overall'] <- nrow(data)
    } else if (variables[i] == 'female') {
      n <- sum(data$female)
      p <- mean(data$female) * 100
      df_demo[i, 'overall'] <- glue("{sprintf('%.0f', n)} ({sprintf('%.2f', p)}%)")
    } else {
      m <- mean(data[, variables[i]], na.rm = TRUE)
      s <- sd(data[, variables[i]], na.rm = TRUE)
      df_demo[i, 'overall'] <- sprintf("%.2f (%.2f)", m, s)
    }
    for (j in 1:length(grouping_values)) {
      df_temp <- data[data[, grouping_variable] == grouping_values[j], ]
      if (variables[i] == 'n') {
        df_demo[i, grouping_values[j]] <- nrow(df_temp)
      } else if (variables[i] == 'female') {
        n <- sum(na.omit(df_temp$female))
        p <- mean(na.omit(df_temp$female)) * 100
        df_demo[i, grouping_values[j]] <- glue("{sprintf('%.0f', n)} ({sprintf('%.2f', p)}%)")
      } else {
        m <- mean(df_temp[, variables[i]], na.rm = TRUE)
        s <- sd(df_temp[, variables[i]], na.rm = TRUE)
        df_demo[i, grouping_values[j]] <- sprintf("%.2f (%.2f)", m, s)
      }
    }
  }
  return(df_demo)
}