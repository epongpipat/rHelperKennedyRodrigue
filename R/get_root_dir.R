#' get_root_dir
#' @concept helper
#' @return
#' @export
#'
#' @examples
#' get_root_dir()
get_root_dir <- function() {
  os <- get_os()
  if (os == "Darwin") {
    root_dir <- "/Volumes"
  } else if (os == "Linux") {
    root_dir <- "/raid/data"
  } else if (os == "Windows") {
    root_dir <- "//cvlkrfs"
  } else {
    root_dir <- NULL
  }
  return(root_dir)
}
