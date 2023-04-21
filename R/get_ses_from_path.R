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
  # path <- 'sub-test_ses-02_bold.nii.gz'
  ses <- path %>%
    str_split('/') %>%
    unlist() %>%
    str_subset('ses-') %>%
    str_split('_') %>%
    unlist() %>%
    str_subset('ses-') %>%
    .[[1]] %>%
    str_split('-') %>%
    unlist() %>%
    .[[2]] %>%
    str_split('_') %>%
    unlist() %>%
    .[[1]]
  return(ses)
}
