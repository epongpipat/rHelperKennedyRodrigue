#' get_root_dir
#' @concept helper
#' @param directory can be either kmk, kmr, or kenrod
#' @return root_dir
#' @export
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom glue glue
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

  if (nrow(df_sub) == 0) {
    stop("no root directory found")
  }

  if (!file.exists(df_sub$path) & os == 'Darwin') {
    # this is not the best way to do this, but it works for now
    df_sub$path <- path.expand(gsub("/Volumes/kenrod", "~/mnt/cvl/kenrod", df_sub$path))
  }

  if (!file.exists(df_sub$path)) {
    stop(glue("root directory does not exist ({df_sub$path})"))
  } 

  
  return(as.character(df_sub$path))

}
