#' renamed_tidy
#' @description rename the columns to be more descriptive
#' @concept data_wrangling
#' @param model input fitted model
#' @return tidy with renamed columns
#' @export
#' @importFrom broom tidy
#' @import dplyr
#' @examples renamed_tidy(lm(salary ~ yrs.since.phd, carData::Salaries))
renamed_tidy <- function(model) {
  allowed_classes <- c('lm', 'htest', 'anova')
  if (sum(attributes(model)$class %in% allowed_classes) == 0) {
    stop(glue("function currntly only supports the following class ({paste0(allowed_classes, seperate = ',')}) (input: {attributes(model)$class})"))
  }
  df_tidy <- tidy(model)
  if (sum(attributes(model)$class %in% c('lm', 'glm')) >= 1) {
    df_tidy <- df_tidy %>%
      rename(rh = term,
             b = estimate,
             se = std.error,
             t = statistic,
             p = p.value) %>%
        mutate(lh = as.character(model[["terms"]][[2]]),
               op = as.character(model[["terms"]][[1]])) %>%
        select(lh, op, rh, b, se, t, p)
  } else if (sum(attributes(model)$class == 'anova') >= 1) {
    df_tidy <- df_tidy %>%
      rename(rh = term,
             ss = sumsq,
             df = df,
             `F` = statistic,
             p = p.value) %>%
      mutate(lh = attributes(model)$heading %>% str_subset('Response') %>% str_remove('Response: '),
             op = '~',
             ms = ss / df) %>%
      select(lh, op, rh, ss, df, ms, `F`, p)
  } else if (sum(attributes(model)$class == 'htest') >= 1) {
    df_tidy <- df_tidy %>%
      rename(chisq = statistic,
             df = parameter,
             p = p.value)
  }
  return(df_tidy)
}