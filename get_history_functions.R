
get_v2025_history_df <- function(df) {

  ########################################################################################################
  ## used for development only
  #
  # trans_status_history <- read_feather("C:\\Users\\morris_johnson\\Documents\\data_projects\\mesa_temp\\feather\\trans_status_history.feather")
  # selected_countries <- c("NG", "ET", "TZ", "GH", "UG", "SD", "SS",
  #                         "AO", "MG", "LR", "KE", "BW", "MZ", "NA")
  # selected_countries <- c("SI", "VU")
  #
  # df <- trans_status_history |>
  #   filter(`Country Code` %in% selected_countries)
  # df <- trans_status_history
  ########################################################################################################

  setDT(df)

  # Pre-calculate all the required data
  v2025 <- df[, .(count = .N), by = .(SnapshotDate, year, month, `Country Code`, `Is Remaining V2025 Need`)]

  v2025 <- dcast(v2025, SnapshotDate + year + month + `Country Code` ~ `Is Remaining V2025 Need`,
                 value.var = "count", fun.aggregate = sum, fill = 0)

  setDT(v2025)

  # Order the data by date
  setorder(v2025, SnapshotDate)

  # Convert SnapshotDate to Date format in all data.tables
  df[, SnapshotDate := as.Date(SnapshotDate)]
  v2025[, SnapshotDate := as.Date(SnapshotDate)]

  # Calculate 'Net starts this month'
  # v2025[, `Net starts this month` := Yes - shift(Yes, type = "lead")]
  # v2025[, `Net starts this month` := Yes - shift(Yes, type = "lead"), by = year]
  v2025[, `Net starts this month` := Yes - shift(Yes, type = "lead"), by = `Country Code`]


  # carry back net starts from January into December of the prior year except for latest month/year
  max_year <- v2025$year |> unique() |> max()
  v2025[, `Net starts this month` := ifelse(is.na(`Net starts this month`),
                                            ifelse((year + 1) > max_year,
                                                   # 9999,
                                                   NA_integer_,
                                                   Yes - v2025[year == .BY$year + 1, first(Yes)]),
                                            `Net starts this month`),
        by = year]

  # Step 1: Create a table of current languages
  current_langs_dt <- df[`Is Remaining V2025 Need` == "Yes",
                         .(current_langs = list(`Language Code`)),
                         by = .(SnapshotDate, `Country Code`)]

  # Step 2: Create a table of next languages
  next_langs_dt <- df[`Is Remaining V2025 Need` == "Yes",
                      .(next_langs = list(`Language Code`)),
                      by = .(SnapshotDate = SnapshotDate %m-% months(1), `Country Code`)]

  # Step 3: Join the current and next language tables
  joined_dt <- merge(current_langs_dt, next_langs_dt,
                     by = c("SnapshotDate", "Country Code"), all.x = TRUE)

  # Step 4: Calculate differences
  result <- joined_dt[, c("Removed", "Added", "Removed_Langs", "Added_Langs") := {
    if (!is.null(next_langs)) {
      removed_langs <- setdiff(current_langs[[1]], next_langs[[1]])
      added_langs <- setdiff(next_langs[[1]], current_langs[[1]])
      list(length(removed_langs), length(added_langs), list(removed_langs), list(added_langs))
    } else {
      list(NA_integer_, NA_integer_, list(character(0)), list(character(0)))
    }
  }, by = .(SnapshotDate, `Country Code`)]

  result2 <- result |>
    # select(-next_langs) |>
    mutate(across(ends_with("_Langs"), ~ map_chr(., \(x) str_c(x, collapse = ", ")))) |>
    rename(RemainingNeeds_langs = current_langs)


  # Step 5: Merge results back to v2025
  result3 <- v2025[result2, on = .(SnapshotDate, `Country Code`)]

  # Step 6: Format reactable data table
  ts_hist_table_df <- result3 |>
    group_by(SnapshotDate) |>
    summarise(
      CountryCodes = paste(unique(`Country Code`), collapse = ", "),
      year = first(year),
      month = first(month),
      No = sum(No),
      Yes = sum(Yes),
      `Net starts this month` = if_else(all(is.na(`Net starts this month`)), NA_integer_, sum(Removed, na.rm = TRUE) - sum(Added, na.rm = TRUE)),
      Removed = if_else(all(is.na(`Net starts this month`)), NA_integer_, sum(Removed, na.rm = TRUE)),
      Added = if_else(all(is.na(`Net starts this month`)), NA_integer_, sum(Added, na.rm = TRUE)),
      RemainingNeeds_Langs = paste(na.omit(unique(RemainingNeeds_langs)), collapse = ", "),
      Removed_Langs = paste(na.omit(unique(Removed_Langs)), collapse = ", "),
      Added_Langs = paste(na.omit(unique(Added_Langs)), collapse = ", ")
    ) |>
    mutate(across(c(RemainingNeeds_Langs, Removed_Langs, Added_Langs), clean_string)) |>
    arrange(SnapshotDate)

  return(ts_hist_table_df)
}

get_aa_history_df <- function(df) {

  setDT(df)

  # Pre-calculate all the required data
  all_access <- df[, .(count = .N), by = .(SnapshotDate, year, month, `Country Code`, `All Access Goal Met`)]

  all_access <- dcast(all_access, SnapshotDate + year + month + `Country Code` ~ `All Access Goal Met`,
                 value.var = "count", fun.aggregate = sum, fill = 0)

  setDT(all_access)

  # Order the data by date
  setorder(all_access, SnapshotDate)

  # Convert SnapshotDate to Date format in all data.tables
  df[, SnapshotDate := as.Date(SnapshotDate)]
  all_access[, SnapshotDate := as.Date(SnapshotDate)]

  # Calculate 'Net goals met this month'
  all_access[, `Net goals met this month` := shift(Yes, type = "lead") - Yes, by = `Country Code`]
  # all_access[, `Net goals met this month` := Yes - shift(Yes, type = "lead"), by = `Country Code`]

  # carry back net starts from January into December of the prior year except for latest month/year
  max_year <- all_access$year |> unique() |> max()
  all_access[, `Net goals met this month` := ifelse(is.na(`Net goals met this month`),
                                            ifelse((year + 1) > max_year,
                                                   # 9999,
                                                   NA_integer_,
                                                   Yes - all_access[year == .BY$year + 1, first(Yes)]),
                                            `Net goals met this month`),
        by = year]

  # Step 1: Create a table of current languages
  current_langs_dt <- df[`All Access Goal Met` == "Yes",
                         .(current_langs = list(`Language Code`)),
                         by = .(SnapshotDate, `Country Code`)]

  # Step 1a: Create a table of current languages
  current_no_langs_dt <- df[`All Access Goal Met` == "No",
                         .(current_no_langs = list(`Language Code`)),
                         by = .(SnapshotDate, `Country Code`)]

  # Step 1b: Join the current 'yes' and 'no' language tables
  current_langs_dt <- merge(current_langs_dt, current_no_langs_dt,
                     by = c("SnapshotDate", "Country Code"), all.x = TRUE)

  # Step 2: Create a table of next languages
  next_langs_dt <- df[`All Access Goal Met` == "Yes",
                      .(next_langs = list(`Language Code`)),
                      by = .(SnapshotDate = SnapshotDate %m-% months(1), `Country Code`)]

  # Step 3: Join the current and next language tables
  joined_dt <- merge(current_langs_dt, next_langs_dt,
                     by = c("SnapshotDate", "Country Code"), all.x = TRUE)

  # Step 4: Calculate differences
  result <- joined_dt[, c("Removed", "Added", "Removed_Langs", "Added_Langs") := {
    if (!is.null(next_langs)) {
      removed_langs <- setdiff(current_langs[[1]], next_langs[[1]])
      added_langs <- setdiff(next_langs[[1]], current_langs[[1]])
      list(length(removed_langs), length(added_langs), list(removed_langs), list(added_langs))
    } else {
      list(NA_integer_, NA_integer_, list(character(0)), list(character(0)))
    }
  }, by = .(SnapshotDate, `Country Code`)]

  result2 <- result |>
    # select(-next_langs) |>
    mutate(across(ends_with("_Langs"), ~ map_chr(., \(x) str_c(x, collapse = ", ")))) |>
    rename(GoalMet_Langs = current_langs,
           GoalNotMet_Langs = current_no_langs)


  # Step 5: Merge results back to all_access
  result3 <- all_access[result2, on = .(SnapshotDate, `Country Code`)]

  # Step 6: Format reactable data table
  aa_hist_table_df <- result3 |>
    group_by(SnapshotDate) |>
    summarise(
      CountryCodes = paste(unique(`Country Code`), collapse = ", "),
      year = first(year),
      month = first(month),
      `No/other` = sum(No),
      Yes = sum(Yes),
      `Net goals met this month` = if_else(all(is.na(`Net goals met this month`)), NA_integer_, sum(Added, na.rm = TRUE) - sum(Removed, na.rm = TRUE)),
      # `Net goals met this month` = if_else(all(is.na(`Net goals met this month`)), NA_integer_, sum(Removed, na.rm = TRUE) - sum(Added, na.rm = TRUE)),
      Removed = if_else(all(is.na(`Net goals met this month`)), NA_integer_, sum(Removed, na.rm = TRUE)),
      Added = if_else(all(is.na(`Net goals met this month`)), NA_integer_, sum(Added, na.rm = TRUE)),
      GoalMet_Langs = paste(na.omit(unique(GoalMet_Langs)), collapse = ", "),
      GoalNotMet_Langs = paste(na.omit(unique(GoalNotMet_Langs)), collapse = ", "),
      Removed_Langs = paste(na.omit(unique(Removed_Langs)), collapse = ", "),
      Added_Langs = paste(na.omit(unique(Added_Langs)), collapse = ", ")
    ) |>
    mutate(across(c(GoalMet_Langs, GoalNotMet_Langs, Removed_Langs, Added_Langs), clean_string)) |>
    # mutate(across(c(GoalMet_Langs, Removed_Langs, Added_Langs), clean_string)) |>
    arrange(SnapshotDate)

  # print(names(aa_hist_table_df))

  return(aa_hist_table_df)
}

######################################################################

get_aa_chapters_df <- function(df) {

  setDT(df)

  # Perform the aggregation
  all_access <- df[, .(remaining_chapters = sum(`Chapters Remaining`)),
  # all_access <- df[, .(remaining_chapters = sum(`AAG chapters remaining`)),
                   by = .(SnapshotDate, year, month)]

  # print("first str for all_access")
  # str(all_access)

  # Reshape the data
  #   all_access <- dcast(all_access, SnapshotDate + year + month ~ `Country Code`,
  #                       value.var = "remaining_chapters", fun.aggregate = sum, fill = 0)
  #   # all_access <- dcast(all_access, SnapshotDate + year + month ~ `Country Code`,
  #   #                     value.var = "remaining_chapters", fun.aggregate = sum, fill = 0)
  #
  # print("second str for all_access")
  # str(all_access)

  setDT(all_access)

  # Order the data by date
  setorder(all_access, SnapshotDate)

  # Convert SnapshotDate to Date format in all data.tables
  df[, SnapshotDate := as.Date(SnapshotDate)]
  all_access[, SnapshotDate := as.Date(SnapshotDate)]

  # Calculate 'Chapters completed this month'
  all_access[, `Chapters completed this month` := `remaining_chapters` - shift(`remaining_chapters`, type = "lead"), by = `SnapshotDate`]

  # carry back net starts from January into December of the prior year except for latest month/year
  max_year <- all_access$year |> unique() |> max()
  all_access[, `Chapters completed this month` := ifelse(is.na(`Chapters completed this month`),
                                                         ifelse((year + 1) > max_year,
                                                                # 9999,
                                                                NA_integer_,
                                                                remaining_chapters - all_access[year == .BY$year + 1, first(remaining_chapters)]),
                                                         `Chapters completed this month`),
             by = year]

  # Step 6: Format reactable data table
  aa_hist_table_df <- all_access |>
    arrange(SnapshotDate)

  return(aa_hist_table_df)
}

clean_string <- function(x) {
  x |>
    str_trim() |>
    str_remove_all("^,\\s*|,\\s*$") |>
    str_replace_all(",\\s*,", ",") |>
    str_trim()
}

# Custom rolling mean function
rollmean_custom <- function(x, k) {
  n <- length(x)
  result <- rep(NA, n)
  for (i in k:n) {
    result[i] <- mean(x[(i-k+1):i], na.rm = TRUE)
  }
  return(result)
}
