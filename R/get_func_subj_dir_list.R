get_func_subj_dir_list <- function(wave = 1) {
  
  if (!is.numeric(wave) || wave <= 0) {
    stop(paste0("Wave (", wave,") must be a number greater than 0"))
  }
  
  dir <- paste0(get_leading_path(), "KK_KR_JLBS/Wave", wave, "/MRI/FMRI/data/")
  
  if (!dir.exists(dir)) {
    stop(paste0("directory (", dir,") does not exist"))
  }
  
  files <- list.files(dir, "3tb*") %>%
    str_subset(., ".mat", T)
  
  path <- paste0(dir, files, "/")
  
  return(path)
}
