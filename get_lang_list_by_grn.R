library(dplyr)
library(stringr)

expand_grn_numbers <- function(data) {
  # Check if data is empty or has no rows
  if (nrow(data) == 0 || ncol(data) == 0) {
    # Return an empty tibble with the expected column structure
    return(tibble(
      `Language Name` = character(0),
      `GRN Numbers` = character(0),
      `Varieties (ROLV)` = character(0),
      Language = character(0),
      Pseudonym = character(0),
      `Pronunciation Guide` = character(0),
      Population = character(0),
      `First Scripture?` = character(0),
      `All Access Goal?` = character(0),
      `Language Vitality` = character(0)
    ))
  }

  # print("*** str for data input to expand_grn_numbers ***")
  # str(data)
  # print(paste("*** name for data input to expand_grn_numbers: ", str_c(names(data), sep = ", ")))

  # Function to split GRN Numbers and create expanded rows
  expand_row <- function(row) {
    # print("*** str for 'row' in 'expand_row' ***")
    # str(row)
    # Extract the GRN and ROLV Numbers column
    grn_numbers <- row$`GRN Numbers`
    rolv_numbers <- row$`Varieties (ROLV)`

    # print(paste0("grn_numbers: ", grn_numbers))
    # print(paste0("rolv_numbers: ", rolv_numbers))
    # str(grn_numbers)
    # str(rolv_numbers)

    grn_numbers_only <- if_else(!is.na(grn_numbers),
                                str_extract_all(grn_numbers, "(?<=\\>)\\d+(?=<\\/)"),
                                list("none"))

    # print(paste0("grn_numbers_only: ", grn_numbers_only))
    # print("str for grn_numbers_only")
    # str(grn_numbers_only)

    rolv_numbers_only <- if_else(!is.na(rolv_numbers),
                                 str_extract_all(rolv_numbers, "(?<=\\()\\d+(?=\\))"),
                                 list("none"))

    # print(paste0("rolv_numbers_only: ", rolv_numbers_only))
    # print("str for rolv_numbers_only")
    # str(rolv_numbers_only)

    numbers_only <- c(grn_numbers_only, rolv_numbers_only, recursive = TRUE)
    numbers_only <- numbers_only[numbers_only != "none"]
    # numbers_only <- numbers_only[!duplicated(numbers_only)]
    # numbers_only <- str_c(grn_numbers_only, rolv_numbers_only)

    # print(paste0("numbers_only: ", numbers_only))
    # print("str for numbers_only")
    # str(numbers_only)

    grn_numbers <- if_else(!is.na(rolv_numbers),
                           str_c(grn_numbers, rolv_numbers, sep = "<br>"),
                           if_else(!is.na(grn_numbers),
                                   grn_numbers,
                                   "none"))

    # numbers_only <- grn_numbers |> str_extract_all("(?<=\\>)\\d*(?=<)")

    # numbers_only <- if_else(grn_numbers != "none",
    #                         str_extract_all(grn_numbers, "(?<=\\>)\\d*(?=<)"),
    #                         "none")

    # print(paste0("*** grn_numbers: ", grn_numbers))

    iso_code <- row$`Language Name (ISO)` |> str_extract("(?<=\\[)\\w{3}")
    # print(paste0("*** iso_code: ", iso_code))

    if(grn_numbers != "none") {
      # Split by <br> tags instead of closing brackets
      grn_entries <- str_split(grn_numbers, "<br>")[[1]]

      # print("*** length of grn_entries ***")
      # length(grn_entries)
      # print("*** str for grn_entries ***")
      # str(grn_entries)

      # Clean up entries - remove empty ones and trim whitespace
      grn_entries <- grn_entries[grn_entries != "" & str_trim(grn_entries) != ""]
      grn_entries <- str_trim(grn_entries)
      # grn_entries <- grn_entries |> remove_duplicates_by_id()

      # pseudonyms <- generate_pseudonym(numbers_only, row$Region)
      pseudonyms <- generate_pseudonym(grn_entries, row$Region)
      # print(paste0("Pseudonyms: ", pseudonyms))
      # print("str for pseudonyms")
      # str(pseudonyms)

      # print(length(grn_entries))

      # Create a data frame with one row per GRN entry
      expanded_rows <- data.frame(
        `Language Name (ISO)` = rep(row$`Language Name (ISO)`, length(grn_entries)),
        `GRN Numbers` = grn_entries,
        Language = rep(row$Language, length(grn_entries)),
        Code = rep(iso_code, length(grn_entries)),
        Pseudonym = pseudonyms,
        `GRN/ROLV Number` = numbers_only,
        `Pronunciation Guide` = rep(row$`Pronunciation Guide`, length(grn_entries)),
        `Number of speakers` = rep(row$`Number of speakers`, length(grn_entries)),
        # Population = rep(row$Population, length(grn_entries)),
        `First Scripture?` = rep(row$`First Scripture?`, length(grn_entries)),
        `All Access Goals` = rep(row$`All Access Goals`, length(grn_entries)),
        # `All Access Goal?` = rep(row$`All Access Goal?`, length(grn_entries)),
        `Language Vitality (EGIDS)` = rep(row$`Language Vitality (EGIDS)`, length(grn_entries)),
        # `Language Vitality` = rep(row$`Language Vitality`, length(grn_entries)),
        stringsAsFactors = FALSE,
        check.names = FALSE)
      } else {
        expanded_rows <- row
      }

    # expanded_rows2 <- as.data.table(expanded_rows)

    expanded_rows <- expanded_rows |>
      filter(`GRN/ROLV Number` != "none")

    # str(expanded_rows)

    return(expanded_rows)
  }

  expanded_data <- data %>%
    base::split(seq_len(nrow(.))) %>%
    purrr::map(expand_row) %>%
    purrr::list_rbind()

  # print("str for expanded_data")
  # str(expanded_data)

  # return(expanded_data)
  languages_by_grn <- expanded_data %>%
    mutate(
      # Extract GRN language name (text before first opening bracket)
      grn_lang_raw = str_extract(`GRN Numbers`, "^[^(]+")) |>
    mutate(
      # Remove text before colon if present, then trim whitespace
      Language = case_when(
        is.na(grn_lang_raw) ~ str_trim(str_extract(`Language Name (ISO)`, "^.*(?=\\w*\\[)"), side = "both"),
        str_detect(grn_lang_raw, ":") ~ str_trim(str_extract(grn_lang_raw, ":[^:]*$") %>% str_remove("^:"), side = "both"),
        TRUE ~ str_trim(grn_lang_raw, side = "both")
      )) |>
    # mutate(Pseudonym = generate_pseudonym(Language, row$Region)) |>
    mutate(
      `Pronunciation Guide` = if_else(!str_detect(Language, "Sign Language"),
                                      generate_pronunciation_guide(Language, region = "Pacific"),
                                      "n/a"),
      .after = `Language`) |>
      # .after = `GRN Number`) |>
    mutate(
      # Remove the temporary column
      grn_lang_raw = NULL) |>
    rename(`GRN/ROLV Numbers` = `GRN Numbers`)
    # rename(`Language Name (ISO)` = `Language Name`,
    #        `GRN/ROLV Numbers` = `GRN Numbers`)

  languages_by_grn <- languages_by_grn |>
    dplyr::distinct(Language, .keep_all = TRUE)

  return(languages_by_grn)
}

# Example usage:
# Assuming your data is in a data frame called 'language_data'
# expanded_data <- expand_grn_numbers(language_data)

# If you need to read the data from the file first:
# language_data <- read.csv("data/assets/Mesa export - test data.csv", header = TRUE, check.names = FALSE)

# expanded_data <- expand_grn_numbers(language_data)

# Fix any empty column names
# names(expanded_data)[names(expanded_data) == ""] <- "Index"
#
# # Transform the dataframe using dplyr mutate
# languages_by_grn <- expanded_data %>%
#   mutate(
#     # Extract GRN language name (text before first opening bracket)
#     grn_lang_raw = str_extract(`GRN Numbers`, "^[^(]+")) |>
#   mutate(
#     # Remove text before colon if present, then trim whitespace
#     Language = case_when(
#       str_detect(grn_lang_raw, ":") ~ str_trim(str_extract(grn_lang_raw, ":[^:]*$") %>% str_remove("^:"), side = "both"),
#       TRUE ~ str_trim(grn_lang_raw, side = "both")
#     )) |>
#   mutate(
#     # Extract GRN number (1 to 5 digits after first opening bracket)
#     `GRN Number` = str_extract(`GRN Numbers`, "(?<=\\()[0-9]{1,5}"),
#     .after = Language) |>
#   mutate(
#     `Pronunciation Guide` = generate_pronunciation_guide(Language, region = "Pacific"),
#     .after = `GRN Number`) |>
#   mutate(
#     # Remove the temporary column
#     grn_lang_raw = NULL)
#
# # Display the first few rows to verify the transformation
# head(languages_by_grn[, c("GRN Numbers", "Language", "GRN Number", "Pronunciation Guide")], 10)

remove_duplicates_by_id <- function(char_vector) {
  # Split the character vector by <br> to get individual elements
  elements <- unlist(strsplit(char_vector, "<br>"))

  # Remove leading/trailing whitespace
  elements <- trimws(elements)

  # Extract ID numbers from elements using regex
  # Look for numbers in parentheses at the end or within href links
  extract_id <- function(element) {
    # First try to find ID in href link (e.g., target='_blank'>24196</a>)
    href_match <- regmatches(element, regexpr("target='_blank'>([0-9]+)</a>", element))
    if (length(href_match) > 0) {
      return(gsub(".*target='_blank'>([0-9]+)</a>.*", "\\1", href_match))
    }

    # If no href link, look for ID in simple parentheses at the end
    paren_match <- regmatches(element, regexpr("\\(([0-9]+)\\)$", element))
    if (length(paren_match) > 0) {
      return(gsub(".*\\(([0-9]+)\\)$", "\\1", paren_match))
    }

    # If no ID found, return NA
    return(NA)
  }

  # Extract IDs for all elements
  ids <- sapply(elements, extract_id, USE.NAMES = FALSE)

  # Create a data frame to track elements and their IDs
  df <- data.frame(
    element = elements,
    id = ids,
    stringsAsFactors = FALSE
  )

  # Remove elements with NA IDs (elements without ID numbers)
  df <- df[!is.na(df$id), ]

  # Keep only the first occurrence of each ID
  df_unique <- df[!duplicated(df$id), ]

  # Return the unique elements as a character vector
  return(df_unique$element)
}

# Test the function with your example data
# test_data <- "<b>Abu</b> (<a href='https://globalrecordings.net/en/language/3208' target='_blank'>3208</a>, GRN rec: No)<br>Abu: Auwa (<a href='https://globalrecordings.net/en/language/24196' target='_blank'>24196</a>, GRN rec: No)<br>Abu: Sabu (<a href='https://globalrecordings.net/en/language/24197' target='_blank'>24197</a>, GRN rec: No)<br>Auwa (24196)<br>Sabu (24197)"
#
# result <- remove_duplicates_by_id(test_data)
# print("Original elements:")
# print(unlist(strsplit(test_data, "<br>")))
# print("\nUnique elements (duplicates removed):")
# print(result)
