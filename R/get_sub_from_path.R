#' @title get_sub_from_path
#' @concept helper
#' @param path path
#' @return sub
#' @export
#' @importFrom dplyr %>%
#' @importFrom stringr str_split 
#' @importFrom stringr str_subset
#' @examples
#' get_sub_from_path('/bids/sub-eep/ses-01/func/sub-eep_ses-01_bold.nii.gz')
get_sub_from_path <- function(path) {
  sub <- path %>%
    str_split('/') %>%
    unlist() %>%
    str_subset('sub-') %>%
    str_split('_') %>%
    unlist() %>%
    str_subset('sub-') %>%
    .[[1]] %>%
    str_split('-') %>%
    unlist() %>%
    .[[2]]
  return(sub)
}
