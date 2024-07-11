#' @title get_duration
#' @concept helper
#' @description obtain duration in dd:hh:mm:sec format
#' @param duration difftime object
#' @param message message to display before the duration (default: duration)
#'
#' @return
#' @export
#' @importFrom glue glue
#' @examples start_time <- Sys.time()
#' Sys.sleep(1)
#' get_duration(Sys.time() - start_time)
#' get_duration(as.difftime(1, units = 'secs'))
#' get_duration(as.difftime(1, units = 'mins'))
#' get_duration(as.difftime(1, units = 'hours'))
#' get_duration(as.difftime(1, units = 'days'))
#' get_duration(as.difftime(1.5251, units = 'days'), message = 'test')
get_duration <- function(duration, message = 'duration') {
  d <- as.numeric(duration, units = "secs")
  days <- floor(d / (24*60*60))
  d <- d - (days * 24 * 60 * 60)
  hours <- floor(d / (60*60))
  d <- d - (hours * 60 * 60)
  minutes <- floor(d / 60)
  d <- d - (minutes * 60)
  seconds <- d %% 60
  cat(glue("{message}:\t{sprintf('%02.f:%02.f:%02.f:%02.3f', days, hours, minutes, seconds)}"), '\n')
}
