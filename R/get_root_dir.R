#' get_root_dir
#' @concept helper
#' @param directory can be either kmk, kmr, or kenrod
#' @return root_dir
#' @export
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @examples
#' get_root_dir()
get_root_dir <- function(directory) {
  
  if (!(directory %in% c('kmk', 'kmr', 'kenrod'))) {
    stop("directory must either be: kmk, kmr, or kenrod")
  }
  
  df <- read.csv(system.file('root_dir.csv', package = 'rHelperKennedyRodrigue'))
  os <- get_os()
  
  df_sub <- df %>%
    filter(os == get_os(),
           dir == directory)
  
  if (os == 'Linux') {
    HOSTNAME <- system("echo ${HOSTNAME}", intern = TRUE)
    df_sub <- df_sub %>%
      filter(hostname == HOSTNAME)
  }
  
  return(as.character(df_sub$path))

}
