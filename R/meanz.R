#' meanz
#' @description z-score variables column-wise and then obtain the mean row-wise
#' @concept data_wrangling
#' @param data data of variables
#' @return
#' @export
#' @examples
#' df <- carData::Salaries
#' yrs_z <- meanz(df[, c('yrs.since.phd', 'yrs.service')])
#' head(yrs_z)
#' 
#' summary(lm(salary ~ meanz(df[, c('yrs.since.phd', 'yrs.service')]), df))
meanz <- function(data) {
  if (dim(data)[1] <= 1) {
    stop('there must be more than 1 column of data')
  } 
  if (dim(data)[2] <= 1) {
    stop('there must be more than 1 row of data')
  }
  return(rowMeans(apply(data, 2, scale)))
}
