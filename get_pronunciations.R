# Regional Pronunciation Guide Generator for Language Names
# Converts language names to phonetic spelling for American English speakers
# Supports different regional pronunciation patterns

generate_pronunciation_guide <- function(language_names, region = "Pacific") {

  # Validate inputs
  if (!is.character(language_names) || length(language_names) == 0) {
    stop("language_names must be a non-empty character vector")
  }

  # region <- tolower(trimws(region))
  valid_regions <- c("Pacific", "Africa")

  if (!region %in% valid_regions) {
    stop(paste("Region must be one of:", paste(valid_regions, collapse = ", ")))
  }

  # Initialize results
  results <- character(length(language_names))

  # Process each language name
  for (i in seq_along(language_names)) {
    name <- trimws(language_names[i])

    if (nchar(name) == 0) {
      results[i] <- ""
      next
    }

    # Apply regional transformation
    if (region == "Pacific") {
      results[i] <- apply_Pacific_rules(name)
    } else if (region == "Africa") {
      results[i] <- apply_Africa_rules(name)
    }
  }

  return(results)
}

# Pacific pronunciation rules (corrected sequence)
apply_Pacific_rules <- function(name) {
  # Convert to lowercase for processing
  name_lower <- tolower(name)

  # FIRST: Apply syllabification to the original name
  syllables <- syllabify_Pacific(name_lower)

  # SECOND: Apply vowel transformations to each syllable
  transformed_syllables <- sapply(syllables, function(syllable) {
    # Basic vowel transformations for Pacific
    syllable <- gsub("a(?=[^aeiou]|$)", "ah", syllable, perl = TRUE)
    syllable <- gsub("e(?=[^aeiou]|$)", "eh", syllable, perl = TRUE)
    syllable <- gsub("i(?=[^aeiou]|$)", "ee", syllable, perl = TRUE)
    syllable <- gsub("o(?=[^aeiou]|$)", "oh", syllable, perl = TRUE)
    syllable <- gsub("u(?=[^aeiou]|$)", "oo", syllable, perl = TRUE)

    # Handle consonant clusters
    syllable <- gsub("ng", "ng", syllable)
    syllable <- gsub("ch", "ch", syllable)

    return(syllable)
  })

  # Apply stress (typically penultimate for Pacific)
  stressed_syllables <- apply_Pacific_stress(transformed_syllables)

  # Join with hyphens
  result <- paste(stressed_syllables, collapse = "-")

  return(result)
}

# Africa pronunciation rules (corrected sequence)
apply_Africa_rules <- function(name) {
  # Convert to lowercase for processing
  name_lower <- tolower(name)

  # FIRST: Apply syllabification to the original name
  syllables <- syllabify_Africa(name_lower)

  # SECOND: Apply transformations to each syllable
  transformed_syllables <- sapply(syllables, function(syllable) {
    # Africa-specific vowel transformations (more lengthening)
    syllable <- gsub("a", "ah", syllable)
    syllable <- gsub("e", "ee", syllable)
    syllable <- gsub("i", "ee", syllable)
    syllable <- gsub("o", "oh", syllable)
    syllable <- gsub("u", "oo", syllable)

    # Handle double vowels (prevent double transformation)
    syllable <- gsub("ahah", "ah", syllable)
    syllable <- gsub("eeee", "ee", syllable)
    syllable <- gsub("ohoh", "oh", syllable)
    syllable <- gsub("oooo", "oo", syllable)

    # Africa consonant emphasis
    syllable <- gsub("^j", "jee", syllable)
    syllable <- gsub("^k", "kah", syllable)
    syllable <- gsub("^g", "gee", syllable)

    # Handle nasal consonant clusters (Africa characteristic)
    syllable <- gsub("mb", "mm-b", syllable)
    syllable <- gsub("ng", "n-g", syllable)
    syllable <- gsub("nw", "n-w", syllable)
    syllable <- gsub("kw", "k-w", syllable)
    syllable <- gsub("gw", "g-w", syllable)

    # Handle consonant clusters at syllable boundaries
    syllable <- gsub("([bcdfghjklmnpqrstvwxyz])([bcdfghjklmnpqrstvwxyz])", "\\1-\\2", syllable, perl = TRUE)

    return(syllable)
  })

  # Apply stress (predominantly initial for Africa)
  stressed_syllables <- apply_Africa_stress(transformed_syllables)

  # Join with hyphens
  result <- paste(stressed_syllables, collapse = "-")

  # Clean up double hyphens
  result <- gsub("--+", "-", result)

  return(result)
}

# Pacific syllabification
syllabify_Pacific <- function(name) {
  # Simple syllabification - split on likely syllable boundaries
  # This is a simplified approach - real syllabification is more complex

  # Split on consonant clusters
  parts <- strsplit(name, "(?<=[aeiou])(?=[^aeiou][aeiou])", perl = TRUE)[[1]]

  # If no splits, try other patterns
  if (length(parts) == 1) {
    parts <- strsplit(name, "(?<=[aeiou])(?=[^aeiou]{2,})", perl = TRUE)[[1]]
  }

  # If still no splits, split roughly in middle for long names
  if (length(parts) == 1 && nchar(name) > 4) {
    mid <- ceiling(nchar(name) / 2)
    parts <- c(substr(name, 1, mid), substr(name, mid + 1, nchar(name)))
  }

  # Clean empty parts
  parts <- parts[nchar(parts) > 0]

  return(parts)
}

# Africa syllabification
syllabify_Africa <- function(name) {
  # More complex syllabification reflecting Africa language patterns

  # Split on vowel-consonant-vowel patterns
  parts <- strsplit(name, "(?<=[aeiou])(?=[^aeiou][aeiou])", perl = TRUE)[[1]]

  # If no splits, try splitting after vowel before consonant cluster
  if (length(parts) == 1) {
    parts <- strsplit(name, "(?<=[aeiou])(?=[^aeiou]{2,})", perl = TRUE)[[1]]
  }

  # Handle special Africa patterns
  if (length(parts) == 1 && nchar(name) > 3) {
    # Split on likely syllable boundaries
    mid <- ceiling(nchar(name) / 2)
    parts <- c(substr(name, 1, mid), substr(name, mid + 1, nchar(name)))
  }

  # Clean empty parts
  parts <- parts[nchar(parts) > 0]

  return(parts)
}

# Apply Pacific stress patterns (typically penultimate)
apply_Pacific_stress <- function(syllables) {
  if (length(syllables) == 0) return(syllables)

  # Stress penultimate syllable (second to last)
  stress_pos <- max(1, length(syllables) - 1)

  syllables[stress_pos] <- toupper(syllables[stress_pos])

  return(syllables)
}

# Apply Africa stress patterns (predominantly initial)
apply_Africa_stress <- function(syllables) {
  if (length(syllables) == 0) return(syllables)

  # Africa languages typically have initial stress
  # Based on analysis: 25/39 words have stress on syllable 1

  # Primary stress on first syllable
  if (length(syllables) >= 1) {
    syllables[1] <- toupper(syllables[1])
  }

  # Secondary stress on third syllable for longer words (4+ syllables)
  if (length(syllables) >= 4) {
    syllables[3] <- toupper(syllables[3])
  }

  return(syllables)
}

# Convenience functions for specific regions
Pacific_pronunciation <- function(language_names) {
  return(generate_pronunciation_guide(language_names, region = "Pacific"))
}

Africa_pronunciation <- function(language_names) {
  return(generate_pronunciation_guide(language_names, region = "Africa"))
}

# Example usage and testing
if (FALSE) {
  # Test with Pacific names
  Pacific_names <- c("Fijian", "Samoan", "Tongan", "Kiribati")
  Pacific_results <- generate_pronunciation_guide(Pacific_names, "Pacific")
  print("Pacific pronunciations: ")
  print(data.frame(Language = Pacific_names, Pronunciation = Pacific_results))

  # Test with Africa names
  Africa_names <- c("Jibana", "Kambe", "Tigania", "Arigidi", "Ulukwumi")
  Africa_results <- generate_pronunciation_guide(Africa_names, "Africa")
  print("Africa pronunciations:")
  print(data.frame(Language = Africa_names, Pronunciation = Africa_results))

  # Compare regions
  test_name <- "Kikuyu"
  print(paste("Pacific style:", generate_pronunciation_guide(test_name, "Pacific")))
  print(paste("Africa style:", generate_pronunciation_guide(test_name, "Africa")))
}
