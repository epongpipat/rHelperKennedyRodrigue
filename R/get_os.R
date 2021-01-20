#' get_os
#'
#' @return
#' @export
#'
#' @examples
get_os <- function() {
  as.character(Sys.info()[1])
}
