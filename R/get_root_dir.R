#' get_root_dir
#'
#' @return
#' @export
#'
#' @examples
get_root_dir <- function() {
  os <- get_os()
  if (os == "Darwin") {
    dir <- "/Volumes/shared"
  } else if (os == "Linux") {
    dir <- "/raid/data/shared"
  } 
  return(dir)
}
