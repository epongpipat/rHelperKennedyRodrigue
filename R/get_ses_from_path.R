#' @title get_ses_from_path
#' @concept helper
#' @param path path
#' @return ses
#' @export
#' @importFrom dplyr %>%
#' @importFrom stringr str_split 
#' @importFrom stringr str_subset
#' @examples
#' get_ses_from_path('/bids/sub-eep/ses-01/func/sub-eep_ses-01_bold.nii.gz')
get_ses_from_path <- function(path) {
  ses <- path %>%
    str_split('/') %>%
    unlist() %>%
    str_subset('ses-') %>%
    .[[1]] %>%
    str_split('-') %>%
    unlist() %>%
    .[[2]]
  return(ses)
}
