# Pre-define valid regions as a named vector for faster lookup
VALID_REGIONS <- c(
  "australia and new zealand" = TRUE, "caribbean" = TRUE, "central america" = TRUE,
  "central asia" = TRUE, "eastern africa" = TRUE, "eastern asia" = TRUE,
  "eastern europe" = TRUE, "melanesia" = TRUE, "micronesia" = TRUE,
  "middle africa" = TRUE, "northern africa" = TRUE, "northern america" = TRUE,
  "northern europe" = TRUE, "polynesia" = TRUE, "south america" = TRUE,
  "south-eastern asia" = TRUE, "southern africa" = TRUE, "southern asia" = TRUE,
  "southern europe" = TRUE, "western africa" = TRUE, "western asia" = TRUE,
  "western europe" = TRUE
)

# Global variable to track used pseudonyms across function calls
.used_pseudonyms <- new.env(parent = emptyenv())

# Function to reset the uniqueness tracker
reset_pseudonym_tracker <- function() {
  rm(list = ls(.used_pseudonyms), envir = .used_pseudonyms)
}

generate_pseudonym <- function(language, region, return_full = FALSE, max_attempts = 1000) {
  # Vectorized function with guaranteed uniqueness

  # print(paste0("language: ", language))
  # print(paste0("region: ", region))


  # Input validation for vector inputs
  if (!is.character(language) || !is.character(region)) {
    stop("Both language and region must be character strings")
  }

  # Ensure same length
  max_len <- max(length(language), length(region))
  if (length(language) == 1) language <- rep(language, max_len)
  if (length(region) == 1) region <- rep(region, max_len)
  if (length(language) != length(region)) {
    stop("Language and region vectors must be the same length")
  }

  # Convert to lowercase for consistency
  language <- tolower(language)
  region <- tolower(region)

  # Validate all regions at once
  invalid_regions <- is.na(VALID_REGIONS[region])
  if (any(invalid_regions)) {
    stop(paste("Invalid region(s):", paste(region[invalid_regions], collapse = ", ")))
  }

  # Base adjectives and nouns - EXPANDED for more combinations
  base_adjectives <- c(
    "bright", "calm", "clear", "cool", "deep", "distant", "early", "flowing",
    "gentle", "golden", "hidden", "quiet", "rapid", "serene", "silent", "smooth",
    "soft", "steady", "strong", "swift", "tall", "warm", "wide", "wild",
    "bold", "crystal", "dancing", "elegant", "fresh", "glowing",
    "graceful", "humble", "kind", "noble", "peaceful", "pure", "radiant",
    "rising", "shiny", "silver", "sparkling", "tender", "tranquil", "vibrant",
    "blazing", "cascading", "drifting", "echoing", "flickering", "gleaming",
    "hovering", "laughing", "murmuring", "rippling", "soaring", "twinkling",
    "whispering", "gleaming", "pristine",
    "stellar", "cosmic", "lunar", "radiant", "brilliant", "dazzling",
    "shimmering", "glistening"
  )

  base_nouns <- c(
    "branch", "brook", "cloud", "dawn", "field", "hill", "lake", "leaf",
    "moon", "path", "peak", "river", "rock", "shadow", "sky", "star",
    "stone", "stream", "sun", "tree", "valley", "wave", "wind", "wood",
    "bridge", "canyon", "coast", "creek", "dune", "echo", "flame", "forest",
    "garden", "grove", "harbor", "island", "meadow", "mountain", "ocean",
    "plateau", "pond", "ridge", "shore", "spring", "summit", "tide", "trail",
    "aurora", "beacon", "ember", "feather", "glacier", "horizon",
    "jewel", "lagoon", "mirage", "nebula", "opal", "prism", "quartz", "rainbow",
    "tempest", "universe", "vortex", "waterfall", "zenith",
    "artifact", "bastion", "doorway", "essence", "fountain",
    "gateway", "haven", "ivory", "keystone", "labyrinth", "monument"
  )

  # Region-specific nature elements - EXPANDED
  regional_elements <- list(
    "australia and new zealand" = list(
      adjectives = c("southern", "emerald", "crimson", "azure", "coral", "misty", "windswept", "rugged", "pristine", "outback"),
      nouns = c("eucalyptus", "reef", "outback", "creek", "billabong", "fjord", "geyser", "glacier", "kiwi", "wallaby", "boomerang", "didgeridoo")
    ),

    "caribbean" = list(
      adjectives = c("tropical", "coral", "turquoise", "sunny", "breezy", "emerald", "crystal", "island"),
      nouns = c("bay", "cove", "palm", "lagoon", "reef", "isle", "beach", "conch", "mango", "hurricane", "steel", "drum")
    ),

    "central america" = list(
      adjectives = c("volcanic", "jade", "misty", "lush", "tropical", "verdant", "humid", "emerald"),
      nouns = c("volcano", "rainforest", "canopy", "quetzal", "cenote", "plateau", "orchid", "jaguar", "pyramid", "cacao", "iguana", "toucan")
    ),

    "central asia" = list(
      adjectives = c("high", "dry", "desert", "nomadic", "vast", "rocky", "barren", "silk", "ancient", "windswept"),
      nouns = c("steppe", "yurt", "camel", "oasis", "dune", "caravan", "silk", "eagle", "bazaar")
    ),

    "eastern africa" = list(
      adjectives = c("red", "dusty", "wide", "endless", "acacia", "golden", "vast", "safari", "ancient"),
      nouns = c("savanna", "baobab", "rift", "zebra", "acacia", "termite", "elephant", "wildebeest", "masai", "kilimanjaro", "rhinoceros", "gazelle")
    ),

    "eastern asia" = list(
      adjectives = c("bamboo", "misty", "jade", "cherry", "silken", "peaceful", "flowing"),
      nouns = c("bamboo", "lotus", "crane", "dragon", "garden", "mountain", "tea", "silk", "panda", "phoenix")
    ),

    "eastern europe" = list(
      adjectives = c("birch", "snowy", "frozen", "dark", "pine", "silver", "cold", "slavic"),
      nouns = c("birch", "pine", "wolf", "raven", "frost", "taiga", "bear", "steppe", "vodka", "balalaika")
    ),

    "melanesia" = list(
      adjectives = c("coral", "volcanic", "tropical", "azure", "lush", "humid", "green"),
      nouns = c("atoll", "coconut", "reef", "volcano", "orchid", "parrot", "canoe", "lagoon", "crocodile", "cassowary", "sago", "taro")
    ),

    "micronesia" = list(
      adjectives = c("tiny", "coral", "blue", "scattered", "peaceful", "remote", "pristine", "oceanic", "atoll", "pacific"),
      nouns = c("atoll", "pearl", "turtle", "reef", "island", "shell", "lagoon", "breeze", "navigator", "outrigger", "frigate", "dolphin")
    ),

    "middle africa" = list(
      adjectives = c("dense", "humid", "green", "thick", "vast", "tropical", "equatorial"),
      nouns = c("rainforest", "mahogany", "vine", "canopy", "leopard", "antelope", "river", "chimpanzee", "okapi", "drum", "mask")
    ),

    "northern africa" = list(
      adjectives = c("golden", "endless", "hot", "dry", "ancient", "vast", "sandy", "desert", "berber"),
      nouns = c("dune", "oasis", "camel", "ibis", "falcon", "desert", "papyrus")
    ),

    "northern america" = list(
      adjectives = c("maple", "snowy", "great", "wide", "prairie", "mighty", "northern", "frontier"),
      nouns = c("maple", "moose", "beaver", "eagle", "buffalo", "prairie", "canyon", "redwood", "tepee", "mustang", "coyote")
    ),

    "northern europe" = list(
      adjectives = c("fjord", "icy", "aurora", "pine", "arctic", "midnight", "crystalline", "viking", "nordic", "runic"),
      nouns = c("fjord", "aurora", "reindeer", "pine", "glacier", "tundra", "seal", "whale", "rune", "longship")
    ),

    "polynesia" = list(
      adjectives = c("tropical", "volcanic", "pearl", "hibiscus", "ocean", "trade", "peaceful", "tiki", "hula", "aloha"),
      nouns = c("volcano", "hibiscus", "pearl", "outrigger", "tiki", "breadfruit", "reef", "manta", "ukulele", "lei", "luau", "kahuna")
    ),

    "south america" = list(
      adjectives = c("amazon", "condor", "high", "misty", "emerald", "ancient", "mystical", "inca", "gaucho", "carnival"),
      nouns = c("condor", "llama", "jaguar", "toucan", "orchid", "capybara", "piranha", "macaw", "alpaca", "quinoa", "samba", "tango")
    ),

    "south-eastern asia" = list(
      adjectives = c("monsoon", "spice", "golden", "humid", "lush", "tropical", "royal", "exotic"),
      nouns = c("elephant", "tiger", "orchid", "monsoon", "spice", "gecko", "mangrove", "durian", "sarong", "gamelan", "batik")
    ),

    "southern africa" = list(
      adjectives = c("diamond", "springbok", "vast", "red", "dry", "endless", "golden"),
      nouns = c("springbok", "diamond", "meerkat", "baobab", "ostrich", "protea", "zebra", "cheetah", "veld", "kraal", "assegai", "ubuntu")
    ),

    "southern asia" = list(
      adjectives = c("monsoon", "sacred", "spice", "lotus", "golden", "ancient", "mystical", "himalayan", "ganges"),
      nouns = c("lotus", "tiger", "peacock", "cobra", "banyan", "monsoon", "sitar", "elephant")
    ),

    "southern europe" = list(
      adjectives = c("olive", "mediterranean", "warm", "ancient", "golden", "sunny", "coastal", "roman", "classical", "renaissance"),
      nouns = c("olive", "cypress", "vine", "dolphin", "marble", "amphora", "bay", "lavender", "colosseum", "fresco", "gondola", "siesta")
    ),

    "western africa" = list(
      adjectives = c("golden", "red", "dusty", "wide", "ancient", "rhythmic", "warm"),
      nouns = c("baobab", "elephant", "drum", "gold", "millet", "hippo", "crocodile", "antelope", "djembe", "kola", "griot", "sahel")
    ),

    "western asia" = list(
      adjectives = c("cedar", "ancient", "desert", "golden", "frankincense", "mystical"),
      nouns = c("cedar", "olive", "frankincense", "camel", "falcon", "oasis", "bazaar")
    ),

    "western europe" = list(
      adjectives = c("castle", "misty", "rolling", "ancient", "oak", "heather", "cobbled", "gothic", "renaissance", "romantic"),
      nouns = c("oak", "castle", "heather", "moor", "thistle", "raven", "stag", "heath", "knight", "troubadour", "vineyard")
    )
  )

  # Language-specific adjectives - EXPANDED
  language_adjectives <- list(
    english = c("whispering", "wandering", "dreaming", "laughing", "singing", "charming", "witty", "clever"),
    spanish = c("dancing", "passionate", "fiery", "spirited", "vibrant", "festive", "romantic", "melodic"),
    french = c("elegant", "delicate", "refined", "graceful", "charming", "sophisticated", "artistic", "poetic"),
    german = c("sturdy", "reliable", "enduring", "steadfast", "resolute", "precise", "methodical", "thorough"),
    italian = c("melodic", "artistic", "romantic", "expressive", "lyrical", "operatic", "passionate", "creative"),
    portuguese = c("gentle", "melodious", "harmonious", "poetic", "soulful", "lyrical", "rhythmic", "flowing"),
    dutch = c("practical", "thoughtful", "measured", "balanced", "grounded", "windmill", "tulip", "dike"),
    japanese = c("harmonious", "balanced", "contemplative", "mindful", "serene", "zen", "honorable", "respectful"),
    chinese = c("balanced", "harmonious", "wise", "patient", "enduring", "ancient", "philosophical", "celestial"),
    korean = c("respectful", "diligent", "persistent", "thoughtful", "careful", "hanbok", "kimchi", "taekwondo"),
    arabic = c("generous", "hospitable", "noble", "dignified", "gracious", "bedouin", "desert", "mystical"),
    russian = c("resilient", "enduring", "deep", "soulful", "contemplative", "siberian", "cossack", "revolutionary"),
    hindi = c("spiritual", "colorful", "diverse", "festive", "sacred", "bollywood", "curry", "monsoon"),
    scandinavian = c("resilient", "hardy", "pure", "pristine", "enduring", "viking", "midnight", "aurora")
  )

  # Calculate total possible combinations to warn if insufficient
  total_combinations <- 0
  unique_regions <- unique(region)
  unique_languages <- unique(language)

  for (r in unique_regions) {
    for (l in unique_languages) {
      adj_count <- length(base_adjectives)
      noun_count <- length(base_nouns)

      regional <- regional_elements[[r]]
      if (!is.null(regional)) {
        adj_count <- adj_count + length(regional$adjectives)
        noun_count <- noun_count + length(regional$nouns)
      }

      lang_adj <- language_adjectives[[l]]
      if (!is.null(lang_adj)) {
        adj_count <- adj_count + length(lang_adj)
      }

      total_combinations <- total_combinations + (adj_count * noun_count)
    }
  }

  # Average combinations per language-region pair
  avg_combinations <- total_combinations / (length(unique_regions) * length(unique_languages))

  # print(paste0("max_len: ", max_len))
  # print(paste0("avg_combination: ", avg_combinations))

  req(max_len, avg_combinations)
  req(max_len > 0, avg_combinations > 0, !is.nan(avg_combinations))

  if (max_len > avg_combinations * 0.8) {
    warning(paste("Requesting", max_len, "pseudonyms but only ~", round(avg_combinations),
                  "average combinations available per language-region pair. Consider adding more words or expect potential failures."))
  }

  # Initialize used set for this session if it doesn't exist
  if (!"used" %in% ls(.used_pseudonyms)) {
    .used_pseudonyms$used <- character(0)
  }

  # Generate unique pseudonyms
  results <- vector("list", length = max_len)

  for (i in seq_len(max_len)) {
    # Build word pools for this iteration
    adjectives <- base_adjectives
    nouns <- base_nouns

    # Add regional elements
    regional <- regional_elements[[region[i]]]
    if (!is.null(regional)) {
      adjectives <- c(adjectives, regional$adjectives)
      nouns <- c(nouns, regional$nouns)
    }

    # Add language-specific adjectives
    lang_adj <- language_adjectives[[language[i]]]
    if (!is.null(lang_adj)) {
      adjectives <- c(adjectives, lang_adj)
    }

    # Generate unique pseudonym with retry logic
    attempts <- 0
    repeat {
      attempts <- attempts + 1

      # Generate pseudonym
      adj_idx <- sample.int(length(adjectives), 1)
      noun_idx <- sample.int(length(nouns), 1)

      selected_adjective <- adjectives[adj_idx]
      selected_noun <- nouns[noun_idx]

      # Capitalize first letters
      selected_adjective_cap <- selected_adjective
      selected_noun_cap <- selected_noun
      substr(selected_adjective_cap, 1, 1) <- toupper(substr(selected_adjective_cap, 1, 1))
      substr(selected_noun_cap, 1, 1) <- toupper(substr(selected_noun_cap, 1, 1))

      pseudonym <- paste(selected_adjective_cap, selected_noun_cap)

      # Check if unique
      if (!pseudonym %in% .used_pseudonyms$used) {
        .used_pseudonyms$used <- c(.used_pseudonyms$used, pseudonym)
        break
      }

      if (attempts >= max_attempts) {
        # Try adding a number suffix as last resort
        for (suffix in 1:999) {
          numbered_pseudonym <- paste0(pseudonym, " ", suffix)
          if (!numbered_pseudonym %in% .used_pseudonyms$used) {
            .used_pseudonyms$used <- c(.used_pseudonyms$used, numbered_pseudonym)
            pseudonym <- numbered_pseudonym
            warning(paste("Had to add numeric suffix to ensure uniqueness:", pseudonym))
            break
          }
        }
        break
      }
    }

    # Store result
    if (return_full) {
      results[[i]] <- list(
        pseudonym = pseudonym,
        adjective = selected_adjective_cap,
        noun = selected_noun_cap,
        language = language[i],
        region = region[i]
      )
    } else {
      results[[i]] <- pseudonym
    }
  }

  # Return appropriate format
  if (return_full) {
    return(results)
  } else {
    # print(paste0("results: ", unlist(results)))
    return(unlist(results))
  }
}

# Example usage:
# reset_pseudonym_tracker()  # Call this to start fresh
# generate_pseudonym("english", "northern america")
# generate_pseudonym(rep("spanish", 100), rep("caribbean", 100))  # Test uniqueness
