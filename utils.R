
get_appended_download_df <- function(df) {
  data <- df |>
    mutate(across(where(is.factor), as.character))

  out <- rbind(c("For internal use only. Please do not share without permission.", rep(NA, ncol(data)-1)),
               c(paste0("Data provided by ProgressBible™ on ", today()), rep(NA, ncol(data)-1)),
               names(data),
               data)
  return(out)
}

aa_hist_transformations <- function(df) {
  df <- df |>
    rename(`AA Goal Met` = `A A Goal Met`,
           `Prior AA Status` = `Prior A A Status`)

  out <- df |>
    # select(SnapshotDate, month, year, `Country Code`, `Language Code`, `On All Access List`,
    #        `All Access Status`, `All Access Goal`, `AA Goal Met`) |>
    # mutate(`Is Sign Language` = factor(`Is Sign Language`, levels = c("Yes", "No"))) %>%
    mutate(`On All Access List` = factor(`On All Access List`, levels = c("Yes", "No"))) %>%
    mutate(`All Access Goal` = as.factor(str_replace_na(.$`All Access Goal`, replacement = "Not available"))) %>%
    mutate(`AAG chapters` = case_when(
      `All Access Goal` == '25 Chapters' ~ 25,
      `All Access Goal` == 'NT / 260 Chapters' ~ 260,
      `All Access Goal` == 'Bible' ~ 1189,
      `All Access Goal` == 'Two Bibles' ~ 2378,
      TRUE ~ 0
    )) |>
    mutate(`AAG chapters remaining` = if_else(is.na(`All Access Status`) | str_detect(`All Access Status`, "Goal Met"),
                                              0, `AAG chapters`)) |>
    mutate('AAG pct chapters remaining' = ifelse(`AAG chapters` == `AAG chapters remaining`, 1, 0))

  return(out)
}


main_rows_transformations <- function(df) {
  out <- df |>
    mutate(`Is Sign Language` = factor(`Is Sign Language`, levels = c("Yes", "No"))) %>%
    mutate(`Is Remaining V2025 Need` = factor(`Is Remaining V2025 Need`, levels = c("Yes", "No"))) %>%
    mutate(`On All Access List` = factor(`On All Access List`, levels = c("Yes", "No"))) %>%
    # mutate(`In The Circle` = factor(`In The Circle`, levels = c("Yes", "No"))) %>%
    mutate(`Translation Status` = as.factor(str_replace_na(.$`Translation Status`, replacement = "Not available"))) %>%
    mutate(`All Access Goal` = as.factor(str_replace_na(.$`All Access Goal`, replacement = "Not available"))) %>%
    left_join(missing_SL_coords, by = "Language Code") |>
    mutate(
      Latitude = coalesce(Latitude.x, Latitude.y),
      Longitude = coalesce(Longitude.x, Longitude.y)
    ) |>
    select(-ends_with(".x"), -ends_with(".y")) |>
    mutate(`AAG chapters` = case_when(
      `All Access Goal` == '25 Chapters' ~ 25,
      `All Access Goal` == 'NT / 260 Chapters' ~ 260,
      `All Access Goal` == 'Bible' ~ 1189,
      `All Access Goal` == 'Two Bibles' ~ 2378,
      TRUE ~ 0
    )) |>
    # mutate(`DSI Eligibility` = if_else(`Language Code` %in% dsi_langs$`Language Code` &
    #                                    `Country Code` %in% dsi_langs$`Country Code`, 'Yes', 'No') |>
    #          as_factor()) |>
    mutate(`AAG chapters remaining` = if_else(is.na(`All Access Status`) | str_detect(`All Access Status`, "Goal Met"),
                                              0, `AAG chapters`)) |>
    mutate('AAG pct chapters remaining' = ifelse(`AAG chapters` == `AAG chapters remaining`, 1, 0))

  out2 <- out |>
    fix_names()

  return(out2)
}

fix_names <- function(df) {
  out <- df |>
    rename(
      POSIXct_date = `P O S I Xct_date`,
      `Active OBT` = `Active O B T`,
      `Prior AA Status` = `Prior A A Status`,
      `Has OBT Engagements` = `Has O B T Engagements`,
      `IllumiNations Region` = `Illumi Nations Region`,
      `IllumiNations Group Name` = `Illumi Nations Group Name`,
      `AA Goal Met` = `A A Goal Met`
    )
  return(out)
}
