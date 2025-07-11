# Regional Pronunciation Guide Generator for Language Names
# Converts language names to phonetic spelling for American English speakers
# Supports different regional pronunciation patterns

generate_pronunciation_guide <- function(language_names, region = "oceanic") {

  # Validate inputs
  if (!is.character(language_names) || length(language_names) == 0) {
    stop("language_names must be a non-empty character vector")
  }

  region <- tolower(trimws(region))
  valid_regions <- c("oceanic", "african")

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
    if (region == "oceanic") {
      results[i] <- apply_oceanic_rules(name)
    } else if (region == "african") {
      results[i] <- apply_african_rules(name)
    }
  }

  return(results)
}

# Oceanic pronunciation rules (original patterns)
apply_oceanic_rules <- function(name) {
  # Convert to lowercase for processing
  name_lower <- tolower(name)

  # Basic vowel transformations for Oceanic
  name_lower <- gsub("a(?=[^aeiou]|$)", "ah", name_lower, perl = TRUE)
  name_lower <- gsub("e(?=[^aeiou]|$)", "eh", name_lower, perl = TRUE)
  name_lower <- gsub("i(?=[^aeiou]|$)", "ee", name_lower, perl = TRUE)
  name_lower <- gsub("o(?=[^aeiou]|$)", "oh", name_lower, perl = TRUE)
  name_lower <- gsub("u(?=[^aeiou]|$)", "oo", name_lower, perl = TRUE)

  # Handle consonant clusters
  name_lower <- gsub("ng", "ng", name_lower)
  name_lower <- gsub("ch", "ch", name_lower)

  # Basic syllabification (simple approach)
  syllables <- syllabify_oceanic(name_lower)

  # Apply stress (typically penultimate for Oceanic)
  stressed_syllables <- apply_oceanic_stress(syllables)

  # Join with hyphens
  result <- paste(stressed_syllables, collapse = "-")

  return(result)
}

# African pronunciation rules (based on analysis)
apply_african_rules <- function(name) {
  # Convert to lowercase for processing
  name_lower <- tolower(name)

  # African-specific vowel transformations (more lengthening)
  name_lower <- gsub("a", "ah", name_lower)
  name_lower <- gsub("e", "ee", name_lower)
  name_lower <- gsub("i", "ee", name_lower)
  name_lower <- gsub("o", "oh", name_lower)
  name_lower <- gsub("u", "oo", name_lower)

  # Handle double vowels (prevent double transformation)
  name_lower <- gsub("ahah", "ah", name_lower)
  name_lower <- gsub("eeee", "ee", name_lower)
  name_lower <- gsub("ohoh", "oh", name_lower)
  name_lower <- gsub("oooo", "oo", name_lower)

  # African consonant emphasis
  name_lower <- gsub("^j", "jee", name_lower)
  name_lower <- gsub("^k", "kah", name_lower)
  name_lower <- gsub("^g", "gee", name_lower)

  # Handle nasal consonant clusters (African characteristic)
  name_lower <- gsub("mb", "mm-b", name_lower)
  name_lower <- gsub("ng", "n-g", name_lower)
  name_lower <- gsub("nw", "n-w", name_lower)
  name_lower <- gsub("kw", "k-w", name_lower)
  name_lower <- gsub("gw", "g-w", name_lower)

  # Handle consonant clusters at syllable boundaries
  name_lower <- gsub("([bcdfghjklmnpqrstvwxyz])([bcdfghjklmnpqrstvwxyz])", "\\1-\\2", name_lower, perl = TRUE)

  # Basic syllabification for African languages
  syllables <- syllabify_african(name_lower)

  # Apply stress (predominantly initial for African)
  stressed_syllables <- apply_african_stress(syllables)

  # Join with hyphens
  result <- paste(stressed_syllables, collapse = "-")

  # Clean up double hyphens
  result <- gsub("--+", "-", result)

  return(result)
}

# Oceanic syllabification
syllabify_oceanic <- function(name) {
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

# African syllabification
syllabify_african <- function(name) {
  # More complex syllabification reflecting African language patterns

  # Handle pre-split consonant clusters
  name <- gsub("-", "|", name)  # Mark existing breaks

  # Split on vowel-consonant-vowel patterns
  parts <- strsplit(name, "(?<=[aeiou])(?=[^aeiou][aeiou])", perl = TRUE)[[1]]

  # If no splits, try splitting after vowel before consonant cluster
  if (length(parts) == 1) {
    parts <- strsplit(name, "(?<=[aeiou])(?=[^aeiou]{2,})", perl = TRUE)[[1]]
  }

  # Handle special African patterns
  if (length(parts) == 1 && nchar(name) > 3) {
    # Split on likely syllable boundaries
    mid <- ceiling(nchar(name) / 2)
    parts <- c(substr(name, 1, mid), substr(name, mid + 1, nchar(name)))
  }

  # Restore cluster breaks
  parts <- unlist(strsplit(parts, "\\|"))

  # Clean empty parts
  parts <- parts[nchar(parts) > 0]

  return(parts)
}

# Apply Oceanic stress patterns (typically penultimate)
apply_oceanic_stress <- function(syllables) {
  if (length(syllables) == 0) return(syllables)

  # Stress penultimate syllable (second to last)
  stress_pos <- max(1, length(syllables) - 1)

  syllables[stress_pos] <- toupper(syllables[stress_pos])

  return(syllables)
}

# Apply African stress patterns (predominantly initial)
apply_african_stress <- function(syllables) {
  if (length(syllables) == 0) return(syllables)

  # African languages typically have initial stress
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
oceanic_pronunciation <- function(language_names) {
  return(generate_pronunciation_guide(language_names, region = "oceanic"))
}

african_pronunciation <- function(language_names) {
  return(generate_pronunciation_guide(language_names, region = "african"))
}

# Example usage and testing
if (FALSE) {
  # Test with Oceanic names
  oceanic_names <- c("Fijian", "Samoan", "Tongan", "Kiribati")
  oceanic_results <- generate_pronunciation_guide(oceanic_names, "oceanic")
  print("Oceanic pronunciations:")
  print(data.frame(Language = oceanic_names, Pronunciation = oceanic_results))

  # Test with African names
  african_names <- c("Jibana", "Kambe", "Tigania", "Arigidi", "Ulukwumi")
  african_results <- generate_pronunciation_guide(african_names, "african")
  print("African pronunciations:")
  print(data.frame(Language = african_names, Pronunciation = african_results))

  # Compare regions
  test_name <- "Kikuyu"
  print(paste("Oceanic style:", generate_pronunciation_guide(test_name, "oceanic")))
  print(paste("African style:", generate_pronunciation_guide(test_name, "african")))
}
