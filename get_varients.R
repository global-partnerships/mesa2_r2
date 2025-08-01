library(data.table)
library(stringr)

#' Extract and Deduplicate Language Variants
#'
#' This function processes language data to extract variants from GRN Numbers,
#' Varieties (ROLV), and Alternate Names fields, removing duplicates according to
#' a priority system: GRN > ROLV > Alternate Names.
#'
#' @param dt A data.table with columns: Language.Name, Language.Code,
#'           Alternate.Names, GRN.Numbers, Varieties..ROLV.
#' @return A data.table with columns: iso_language_name, variant_name, variant_type
#'
#' @examples
#' # Assuming your data is in a data.table called 'language_data'
#' variants <- extract_language_variants(language_data)
#' head(variants)
extract_language_variants <- function(dt) {

  # Helper function to extract names from GRN Numbers field
  extract_grn_variants <- function(grn_text) {
    if (is.na(grn_text) || grn_text == "") return(character(0))

    # Pattern to match: "Name (code, GRN rec: No)" or "Parent: Child (code, GRN rec: No)"
    patterns <- str_extract_all(grn_text, "([^:()]+?)(?:\\s*\\(\\d+,\\s*GRN\\s*rec:\\s*No\\))")[[1]]

    # Clean up the extracted names
    variants <- str_trim(str_remove_all(patterns, "\\s*\\(\\d+,\\s*GRN\\s*rec:\\s*No\\)"))

    # Handle parent:child format - extract just the child name
    variants <- sapply(variants, function(x) {
      if (str_detect(x, ":")) {
        parts <- str_split(x, ":")[[1]]
        return(str_trim(parts[length(parts)]))  # Take the part after the last colon
      } else {
        return(str_trim(x))
      }
    })

    return(unique(variants[variants != ""]))
  }

  # Helper function to extract names from Varieties (ROLV) field
  extract_rolv_variants <- function(rolv_text) {
    if (is.na(rolv_text) || rolv_text == "") return(character(0))

    # Pattern to match: "Name (code)" format
    patterns <- str_extract_all(rolv_text, "([^()]+?)\\s*\\(\\d+\\)")[[1]]
    variants <- str_trim(str_remove_all(patterns, "\\s*\\(\\d+\\)"))

    return(unique(variants[variants != ""]))
  }

  # Helper function to extract names from Alternate Names field
  extract_alt_variants <- function(alt_text) {
    if (is.na(alt_text) || alt_text == "") return(character(0))

    # Pattern to match: "Name (source, type)" format
    patterns <- str_extract_all(alt_text, "([^()]+?)\\s*\\([^)]+\\)")[[1]]
    variants <- str_trim(str_remove_all(patterns, "\\s*\\([^)]+\\)"))

    return(unique(variants[variants != ""]))
  }

  # Helper function to extract GRN codes for duplicate detection
  extract_grn_codes <- function(grn_text) {
    if (is.na(grn_text) || grn_text == "") return(character(0))

    codes <- str_extract_all(grn_text, "\\((\\d+),\\s*GRN\\s*rec:\\s*No\\)")[[1]]
    codes <- str_extract(codes, "\\d+")
    return(codes[!is.na(codes)])
  }

  # Helper function to extract ROLV codes for duplicate detection
  extract_rolv_codes <- function(rolv_text) {
    if (is.na(rolv_text) || rolv_text == "") return(character(0))

    codes <- str_extract_all(rolv_text, "\\((0?\\d+)\\)")[[1]]
    codes <- str_extract(codes, "0?(\\d+)")
    codes <- str_remove(codes, "^0+")  # Remove leading zeros
    return(codes[!is.na(codes) & codes != ""])
  }

  # Initialize result data.table
  result <- data.table(
    iso_language_name = character(0),
    variant_name = character(0),
    variant_type = character(0)
  )

  # Process each language row
  for (i in 1:nrow(dt)) {
    row <- dt[i, ]
    language_name <- row$Language.Name

    # Extract variants from each field
    grn_variants <- extract_grn_variants(row$GRN.Numbers)
    rolv_variants <- extract_rolv_variants(row$Varieties..ROLV.)
    alt_variants <- extract_alt_variants(row$Alternate.Names)

    # Extract codes for duplicate detection
    grn_codes <- extract_grn_codes(row$GRN.Numbers)
    rolv_codes <- extract_rolv_codes(row$Varieties..ROLV.)

    # Step 1: Add all GRN variants (highest priority)
    if (length(grn_variants) > 0) {
      grn_dt <- data.table(
        iso_language_name = language_name,
        variant_name = grn_variants,
        variant_type = "GRN"
      )
      result <- rbind(result, grn_dt)
    }

    # Step 2: Add ROLV variants that don't duplicate GRN codes
    if (length(rolv_variants) > 0) {
      # Remove ROLV variants whose codes match GRN codes
      valid_rolv <- character(0)
      rolv_variant_codes <- extract_rolv_codes(row$Varieties..ROLV.)

      for (j in seq_along(rolv_variants)) {
        if (j <= length(rolv_variant_codes)) {
          if (!rolv_variant_codes[j] %in% grn_codes) {
            valid_rolv <- c(valid_rolv, rolv_variants[j])
          }
        }
      }

      if (length(valid_rolv) > 0) {
        rolv_dt <- data.table(
          iso_language_name = language_name,
          variant_name = valid_rolv,
          variant_type = "ROLV"
        )
        result <- rbind(result, rolv_dt)
      }
    }

    # Step 3: Add Alternate Names that don't duplicate GRN or valid ROLV variants
    if (length(alt_variants) > 0) {
      existing_variants <- c(grn_variants, rolv_variants)

      # Remove duplicates from alternate names
      unique_alt_variants <- setdiff(alt_variants, existing_variants)

      # Remove within-field duplicates
      unique_alt_variants <- unique(unique_alt_variants)

      if (length(unique_alt_variants) > 0) {
        alt_dt <- data.table(
          iso_language_name = language_name,
          variant_name = unique_alt_variants,
          variant_type = "Alternate"
        )
        result <- rbind(result, alt_dt)
      }
    }
  }

  # Final deduplication: remove any remaining duplicates within each language
  # keeping the highest priority variant type
  result[, priority := case_when(
    variant_type == "GRN" ~ 1,
    variant_type == "ROLV" ~ 2,
    variant_type == "Alternate" ~ 3,
    TRUE ~ 4
  )]

  # Remove duplicates, keeping the highest priority (lowest number)
  result <- result[order(iso_language_name, variant_name, priority)]
  result <- result[!duplicated(paste(iso_language_name, variant_name)), ]

  # Remove the priority column
  result[, priority := NULL]

  return(result)
}

# Example usage function to demonstrate the expected input format
create_sample_data <- function() {
  # This shows the expected column structure for the input data.table
  sample_dt <- data.table(
    Country = "Papua New Guinea",
    Language.Name = c(
      "Abu' [aah]",
      "Miniafia Oyan [aai]",
      "Ankave [aak]"
    ),
    Language.Code = c("aah", "aai", "aak"),
    Alternate.Names = c(
      "Matapau (PG, L)Ounibisima (PG, L)Womenika (PG, L)Abu' Arapesh (PG, L)Ua (PG, L)",
      "Miniafia (PG, L)Arifama (PG, L)Miniafia (PG, L)Arifama-Miniafia (PG, L)Miniafia (PG, L)Miniafia-Arifama (PG, L)",
      "Aga pɨ'ne' (PG, L)Ankai (PG, L)Bu'u (PG, L)Miyatnu (PG, L)Sawuve (PG, L)Wiyagwa (PG, L)Wunavai (PG, L)Aga pɨ'ne' (PG, L)Angave (PG, L)Ankave-Anga (PG, L)"
    ),
    GRN.Numbers = c(
      "Abu' (20310, GRN rec: No)Arapesh, Abu': Matapau (24175, GRN rec: No)Arapesh, Abu': Ounibisima (24176, GRN rec: No)Arapesh, Abu': Womenika (24177, GRN rec: No)",
      "Miniafia Oyan: Miniafia (2416, GRN rec: No)Miniafia Oyan (2434, GRN rec: No)Karoto (2436, GRN rec: No)",
      "Ankave-Anga (7019, GRN rec: No)Ankave: Ankai (7020, GRN rec: No)Ankave: Bu'u (7021, GRN rec: No)Ankave: Miyatnu (7022, GRN rec: No)Ankave: Sawuve (7023, GRN rec: No)Ankave: Wiyagwa (7024, GRN rec: No)Ankave: Wunavai (7025, GRN rec: No)"
    ),
    Varieties..ROLV. = c(
      NA_character_,
      "Miniafia (02416)Karoto (02436)",
      "Ankai (07020)Bu'u (07021)Miyatnu (07022)Sawuve (07023)Wiyagwa (07024)Wunavai (07025)"
    )
  )
  return(sample_dt)
}

# Demonstration of the function
demo_extraction <- function() {
  # Create sample data
  sample_data <- create_sample_data()

  cat("Input data structure:\n")
  print(sample_data)

  cat("\n\nProcessing variants...\n")

  # Extract variants
  variants <- extract_language_variants(sample_data)

  cat("\nExtracted variants:\n")
  print(variants)

  cat("\nSummary by language and type:\n")
  print(variants[, .N, by = .(iso_language_name, variant_type)])

  return(variants)
}
