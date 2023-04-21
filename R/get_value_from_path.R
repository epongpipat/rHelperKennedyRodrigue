#' @title get_value_from_path
#' @concept helper
#' @param path path
#' @param key key
#' @return value
#' @export
#' @importFrom glue glue
#' @importFrom dplyr %>%
#' @importFrom stringr str_split 
#' @importFrom stringr str_subset
#' @examples
#' get_value_from_path('/bids/sub-eep/ses-01/func/sub-eep_ses-01_bold.nii.gz', 'sub')
get_value_from_path <- function(path, key) {
  value <- path %>%
    str_split('/|[[.]]') %>%
    unlist() %>%
    str_subset(glue('{key}-')) %>%
    .[[1]] %>%
    str_split('_') %>%
    unlist() %>%
    str_subset(glue('{key}-')) %>%
    str_remove(glue("{key}-"))
  return(value)
}
