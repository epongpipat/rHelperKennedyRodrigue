#' get_root_dir
#'
#' @return
#' @export
#'
#' @examples
get_root_dir <- function() {
  os <- get_os()
  if (os == "Darwin") {
    root_dir <- "/Volumes"
  } else if (os == "Linux") {
    root_dir <- "/raid/data"
  } else {
    root_dir <- NULL
  }
  return(root_dir)
}
