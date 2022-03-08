#' @title get_ext
#'
#' @param path path
#' @concept helper
#' @return
#' @export
#' @import dplyr
#' @importFrom stringr str_split
#' @importFrom stringr str_subset
#' @examples
#' get_ext('test.nii.gz')
get_ext <- function(path) {
  path %>%
    basename() %>%
    str_split('[[.]]') %>%
    unlist() %>%
    str_subset('nii|gz|json|bval|bvec') %>%
    paste0(., collapse = '.') %>%
    paste0(".", .)
}