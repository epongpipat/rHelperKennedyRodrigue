get_ica_subj_dir_list <- function(wave = 1) {
  
  if (!is.numeric(wave) || wave <= 0) {
    stop(paste0("Wave (", wave,") must be a number greater than 0"))
  }
  
  temp_df <- tibble(path = paste0(get_func_subj_dir_list(wave), "ICA/")) %>%
    mutate(run = list(1:3)) %>%
    unnest() %>%
    mutate(path = glue("{path}Run{run}/"),
           ica = map(path, function(x) list.files(x, pattern = ".ica"))) %>%
    unnest() %>%
    mutate(path = glue("{path}{ica}/"),
           exists = ifelse(file.exists(path), T, F)) %>%
    filter(exists == T)
  
  paths <- c(temp_df$path)
  return(paths)

}
