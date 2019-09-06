get_ica_subj_dir_list <- function(wave = 1) {
  
  df_temp <- paste0(path = get_func_subj_list(wave), "ICA/") %>%
    tibble(run = list(1:3)) %>%
    unnest() %>%
    mutate(path = glue("{path}Run{run}/"),
           ica = map(dir, function(x) list.files(x, pattern = ".ica"))) %>%
    unnest() %>%
    mutate(path = glue("{path}{ica}/"),
           exists = ifelse(file.exists(path), T, F)) %>%
    filter(exists == T)
  
  return(df_temp$path)

}
