get_leading_path <- function() {
  os <- determine_os()
  if (os == "Darwin") {
    dir <- "/Volumes/shared/"
  } else if (os == "Linux") {
    dir <- "/raid/data/shared/"
  } 
  return(dir)
}
