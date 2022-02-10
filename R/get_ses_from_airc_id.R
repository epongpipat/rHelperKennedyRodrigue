#' @title get_ses_from_airc_id
#'
#' @param airc_id AIRC ID
#' @concept helper
#' @return
#' @export
#' @importFrom stringer str_remove
#' @examples
get_ses_from_airc_id <- function(airc_id) {
  airc_id_number <- as.numeric(str_remove(airc_id, '3tb'))
  if (airc_id_number < 4188) {
    ses <- 1
  } else if ((airc_id_number > 4188) & (airc_id_number < 8000)) {
    ses <- 2
  } else if ((airc_id_number > 8000)) {
    ses <- 3
  }
  return(ses)
}
