#' @title get_ses_from_airc_id
#'
#' @param airc_id AIRC ID
#' @concept helper
#' @return
#' @export
#' @importFrom glue glue
#' @examples
get_ses_from_airc_id <- function(airc_id) {
  in_ids <- glue("{get_root_dir('kenrod')}/incoming/ids_long-format.csv")
  df <- read.csv(in_ids) 
  idx <- which(df$mri_id == airc_id)
  if (length(idx) == 0) {
    warning('no study ID found for this AIRC ID')
    ses <- NA
  } else if (length(idx) == 1) {
    ses <- df[[idx, 'wave']]
  } else if (length(idx) > 1) {
    warning('more than 1 session found for this AIRC ID')
    ses <- NA
  }
  return(ses)
}
