#' get_airc_id_from_path
#'
#' @param path file path
#' @concept helper 
#' @return airc id (character)
#' @export
#' @import stringr
#' @examples 
#' path <- "/fake/dir/3tb9999/3tb9999_fake_file.txt"
#' get_airc_id_from_path(path)
get_airc_id_from_path <- function(path) {
  idx_start <- str_locate(path, '3t')[1]
  idx_end <- idx_start + 6
  airc_id <- str_sub(path, idx_start, idx_end)
  return(airc_id)
}