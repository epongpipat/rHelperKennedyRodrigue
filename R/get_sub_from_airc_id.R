#' @title get_sub_from_airc_id
#' @description 
#' @param airc_id AIRC ID
#' @concept helper
#' @return
#' @export
#' @importFrom glue glue
#' @examples
get_sub_from_airc_id <- function(airc_id) {
  in_ids <- glue("{root_dir}/shared/incoming/ids_long-format.csv")
  df <- read.csv(in_ids) 
  idx <- which(df$mri_id == airc_id)
  if (length(idx) == 0) {
    warning('no study ID found for this AIRC ID')
    sub <- NA
  } else if (length(idx) == 1) {
    sub <- df[[idx, 'study_id']]
  } else if (length(idx) > 1) {
    warning('more than 1 study ID found for this AIRC ID')
    sub <- NA
  }
  return(sub)
}

