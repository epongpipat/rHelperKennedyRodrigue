#' get_os
#' @concept helper
#' @return
#' @export
#'
#' @examples
#' get_os()
get_os <- function() {
  as.character(Sys.info()[1])
}
