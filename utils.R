
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
    mutate(`Chapters Remaining` = if_else(`Chapter Goal` - `Chapters To Goal` > 0,
                                          `Chapter Goal` - `Chapters To Goal`,
                                          0))
    # mutate(`AAG chapters` = case_when(
    #   `All Access Goal` == '25 Chapters' ~ 25,
    #   `All Access Goal` == 'NT / 260 Chapters' ~ 260,
    #   `All Access Goal` == 'Bible' ~ 1189,
    #   `All Access Goal` == 'Two Bibles' ~ 2378,
    #   TRUE ~ 0
    # )) |>
    # mutate(`AAG chapters remaining` = if_else(is.na(`All Access Status`) | str_detect(`All Access Status`, "Goal Met"),
    #                                           0, `AAG chapters`)) |>
    # mutate('AAG pct chapters remaining' = ifelse(`AAG chapters` == `AAG chapters remaining`, 1, 0))

  return(out)
}


main_rows_transformations <- function(df, combined_partner_areas, grn_recorded) {
  # archive_link_prefix <- "<a href='https://www.sil.org/resources/search/language/"
  # archive_link_infix <- "' target='_blank'>"
  # archive_link_suffix <- "</a>"

  fips_lookup <- combined_partner_areas |>
    filter(Partner == "Global Partnerships") |>
    select(`Country Code`, `FIPS Code`)

  df <- df %>%
    left_join(fips_lookup, join_by(`Country Code`)) |>
    mutate(
      `Search SIL Archive` = paste0(
        archive_link_prefix,
        `Language Code`,
        archive_link_infix,
        `Language Name`,
        archive_link_suffix),
      `joshua_proj_country` = paste0(
        "<a href='https://joshuaproject.net/countries/",
        `FIPS Code`,
        archive_link_infix,
        `Country`,
        archive_link_suffix),
      `See Joshua Project` = paste0(
        "<a href='https://joshuaproject.net/languages/",
        `Language Code`,
        archive_link_infix,
        `Language Name`,
        archive_link_suffix),
      `See GRN listing` = paste0(
        "<a href='https://globalrecordings.net/en/language/",
        `Language Code`,
        archive_link_infix,
        "See GRN listing",
        archive_link_suffix)
    )

  out <- df |>
    mutate(`Chapters Remaining` = if_else(`Chapter Goal` - `Chapters To Goal` > 0,
                                          `Chapter Goal` - `Chapters To Goal`,
                                          0)) |>
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
    # mutate(`AAG chapters` = case_when(
    #   `All Access Goal` == '25 Chapters' ~ 25,
    #   `All Access Goal` == 'NT / 260 Chapters' ~ 260,
    #   `All Access Goal` == 'Bible' ~ 1189,
    #   `All Access Goal` == 'Two Bibles' ~ 2378,
    #   TRUE ~ 0
    # )) |>
    mutate(`Alternate Names` = gsub("; ", "<br>", `Alternate Names`)) |>
    # mutate(`DSI Eligibility` = if_else(`Language Code` %in% dsi_langs$`Language Code` &
    #                                    `Country Code` %in% dsi_langs$`Country Code`, 'Yes', 'No') |>
    #          as_factor()) |>
    # mutate(`AAG chapters remaining` = if_else(is.na(`All Access Status`) | str_detect(`All Access Status`, "Goal Met"),
    #                                           0, `AAG chapters`)) |>
    # mutate('AAG pct chapters remaining' = ifelse(`AAG chapters` == `AAG chapters remaining`, 1, 0)) |>
    mutate(`HB - Language` = str_trim(str_extract(`Language Name`, "^[^\\[]+"))) |>
    mutate(`HB - Pronunciation Guide` = if_else(
      Continent == "Pacific",
      generate_pronunciation_guide(`HB - Language`, "Pacific"),
      generate_pronunciation_guide(`HB - Language`, "Africa")
    )) |>
    mutate(`HB - Population` = `1st Language Pop`) |>
    # mutate(`HB - First Scripture?` =
    #          if_else(trimws(`Recorded (GRN)`) == 'Yes',
    #                  "No",
    #                  if_else(is.na(`Completed Scripture`) |
    #                            `Translation Status` %in% c("Limited or Old Scripture",
    #                                                       "Expressed Need",
    #                                                       "Potential Need"),
    #                          "Yes",
    #                          "No")
    #                  )
    #        ) |>
    mutate(`HB - First Scripture?` =
             if_else((`Translation Status` %in% c("Limited or Old Scripture",
                                                  "Expressed Need",
                                                  "Potential Need") |
                      is.na(`Completed Scripture`)) &
                      trimws(`Recorded (GRN)`) == 'No',
                     "Yes",
                     "No")
           ) |>
    # mutate(`HB - First Scripture?` =
    #          if_else(is.na(`Completed Scripture`) |
    #                    `Translation Status` %in% c("Limited or Old Scripture",
    #                                               "Expressed Need",
    #                                               "Potential Need") |
    #                    trimws(`Recorded (GRN)`) == 'No',
    #                  "Yes",
    #                  "No")
    #        ) |>
    # mutate(`HB - First Scripture?` =
    #          if_else(is.na(`Varieties (ROLV)`),
    #            if_else(is.na(`Completed Scripture`) |
    #                      `Translation Status` %in% c("Limited or Old Scripture",
    #                                                 "Expressed Need",
    #                                                 "Potential Need") |
    #                      `Recorded (GRN)` == 0,
    #                    "Yes",
    #                    "No"),
    #            `See GRN listing`)
    #        ) |>
    mutate(`HB - All Access Goal?` =
             if_else(str_detect(`All Access Status`, "^(Goal Met|Not on)"),
             # if_else(str_detect(`All Access Status`, "^Goal Met.*"),
                     `All Access Status`,
                     `All Access Goal`)) |>
    # mutate(`HB - All Access Goal?` =
    #          if_else(str_detect(`HB - All Access Goal?`, "^Not available"),
    #                  "Not on All Access Goal List",
    #                  `HB - All Access Goal?`)) |>
    mutate(`HB - Language Vitality` = `Language Vitality`)
  # |>
  #   mutate(`HB - Pseudonym` = generate_pseudonym(`Language Name`, Region))

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

# arrange_hb_columns <- function(df) {
#   hb_order <- c("HB - Language",
#                 "HB - Pronunciation Guide",
#                 "HB - Population",
#                 "HB - First Scripture?",
#                 "HB - All Access Goal?",
#                 "HB - Language Vitality")
#
#   hb_present <- hb_order[hb_order %in% names(df)]
#   other_cols <- names(df)[!names(df) %in% hb_order]
#
#   df[, c(other_cols, hb_present)]
# }

arrange_hb_columns <- function(df) {
  hb_order <- c("HB - Language",
                "HB - Pronunciation Guide",
                "HB - Population",
                "HB - First Scripture?",
                "HB - All Access Goal?",
                "HB - Language Vitality")
  hb_present <- hb_order[hb_order %in% names(df)]
  other_cols <- names(df)[!names(df) %in% hb_order]
  df[, .SD, .SDcols = c(other_cols, hb_present)]
}

remove_hb_prefix <- function(df) {
  # Remove "HB - " from the beginning of column names
  names(df) <- gsub("^HB - ", "", names(df))
  return(df)
}

factorize_factorables <- function(df, threshold = 0.1) {
  df <- df %>%
    mutate(
      across(
        where(~ is.character(.x) | is.integer(.x)),
        ~ {
          # Additional safety checks
          if (length(.x) == 0) {
            return(.x)
          }

          # Handle case where all values are NA
          if (all(is.na(.x))) {
            return(.x)
          }

          # Calculate unique proportion safely
          n_total <- length(.x)
          n_unique <- n_distinct(.x, na.rm = TRUE)

          if (n_total > 0 && n_unique / n_total <= threshold) {
            factor(.x, exclude = NULL)
          } else {
            .x
          }
        }
      )
    )
}
