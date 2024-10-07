#' get_effect_size
#' @description obtains effect size and confidence intervals for a given linear model
#' @concept data_wrangling
#' @param model linear model object
#' @param a_thr alpha threshold for confidence interval (default = 0.05)
#'
#' @return data frame with effect size and confidence intervals
#' @export
#' @importFrom glue glue
#' @importFrom broom tidy
#' @importFrom broom glance
#' @importFrom effectsize t_to_eta2
#' @importFrom effectsize t_to_eta2_adj
#' @import dplyr
#' 
#' @examples
#' model_fit <- lm(mpg ~ wt, data = mtcars)
#' get_effect_size(model_fit)
get_effect_size <- function(model, a_thr = 0.05) {
  df_r <- glance(model)[['df.residual']]
  df_ci <- confint(model, level = 1-a_thr)
  level_str <- (1-a_thr)*100
  colnames(df_ci) <- c(glue('b_ci_{level_str}_ll'), glue('b_ci_{level_str}_ul'))
  r_sq <- t_to_eta2(tidy(model)$statistic, df_r) %>% as.data.frame() %>%
    select(Eta2_partial) %>%
    rename(r_sq = Eta2_partial)
  r_sq_adj <- t_to_eta2_adj(tidy(model)$statistic, df_r) %>% as.data.frame() %>%
    select(Epsilon2_partial) %>%
    rename(r_sq_adj = Epsilon2_partial)
  df_es <- cbind(df_ci, r_sq, r_sq_adj)
  return(df_es)
}