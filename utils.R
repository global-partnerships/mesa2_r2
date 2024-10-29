
get_appended_download_df <- function(df) {
  data <- df |>
    mutate(across(where(is.factor), as.character))
  
  out <- rbind(c("For internal use only. Please do not share without permission.", rep(NA, ncol(data)-1)), 
               c(paste0("Data provided by ProgressBible™ on ", today()), rep(NA, ncol(data)-1)),
               names(data),
               data)
  return(out)
}

  # out <- get_appended_download_df(lang_coords_df)
  # 
  # filename <- paste0("Mesa_export_", today())
  # 
  # write_csv(out, filename, col_names = FALSE, na = "")

