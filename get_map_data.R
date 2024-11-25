library(tidyverse)
library(leaflet)
library(leaflet.extras)

get_map_data <- function(df, dashboard, countries){

    title_text <- paste0(dashboard, " summary: ", countries)

    if(dashboard == "All Access"){
      df_map <- df %>%
          filter(.$`On All Access List` == "Yes")
    } else if (dashboard == "Vision 2025") {
      df_map <- df %>%
        filter(.$`Is Remaining V2025 Need` == "Yes")
    } else if (dashboard == "Sign Language") {
      df_map <- df %>%
        filter(.$`Is Sign Language` == "Yes")
    } else if (dashboard == "Engagements") {
      df_map <- df %>%
        filter(.$`Engagement Status` == "Current")
    } else {
      df_map <- df
    }


    return(df_map)
}

# calculate_map_bounds <- function(countries) {
#   world_map <- maps::map(database = "world",
#                          regions = as.character(countries),
#                          fill = TRUE,
#                          plot = FALSE,
#                          exact = FALSE)
#
#   bounds <- list(
#     lng1 = min(world_map$x, na.rm = TRUE),
#     lng2 = max(world_map$x, na.rm = TRUE),
#     lat1 = min(world_map$y, na.rm = TRUE),
#     lat2 = max(world_map$y, na.rm = TRUE)
#   )
#
#   # If bounds cross the international date line, adjust
#   if (bounds$lng2 - bounds$lng1 > 180) {
#     # Adjust coordinates for Pacific-centered view, handling NAs
#     is_negative <- !is.na(world_map$x) & world_map$x < 0
#     world_map$x[is_negative] <- world_map$x[is_negative] + 360
#
#     bounds$lng1 <- min(world_map$x, na.rm = TRUE)
#     bounds$lng2 <- max(world_map$x, na.rm = TRUE)
#   }
#
#   list(
#     bounds = bounds,
#     center = list(
#       lng = mean(c(bounds$lng1, bounds$lng2)),
#       lat = mean(c(bounds$lat1, bounds$lat2))
#     )
#   )
# }
#
# calculate_zoom_level <- function(bounds, padding_levels = 0) {
#   # Calculate the span of coordinates
#   lng_span <- abs(bounds$lng2 - bounds$lng1)
#   lat_span <- abs(bounds$lat2 - bounds$lat1)
#
#   # Use the larger span to determine zoom
#   max_span <- max(lng_span, lat_span)
#
#   # Calculate base zoom level
#   base_zoom <- floor(log2(360/max_span))
#
#   # Add padding by reducing zoom level
#   padded_zoom <- base_zoom - padding_levels
#
#   # Ensure zoom doesn't go below 0 or above 18 (typical max zoom)
#   final_zoom <- max(min(padded_zoom, 18), 0)
#
#   return(final_zoom)
# }

calculate_map_bounds <- function(countries) {
  world_map <- maps::map(database = "world",
                         regions = as.character(countries),
                         fill = TRUE,
                         plot = FALSE,
                         exact = FALSE)

  # Initial bounds calculation
  bounds <- list(
    lng1 = min(world_map$x, na.rm = TRUE),
    lng2 = max(world_map$x, na.rm = TRUE),
    lat1 = min(world_map$y, na.rm = TRUE),
    lat2 = max(world_map$y, na.rm = TRUE)
  )

  # Handle date line crossing
  if (bounds$lng2 - bounds$lng1 > 180) {
    # For Pacific/Oceania region, we want to center around the Pacific
    pacific_center <- 150  # Roughly centers on Pacific region

    # Normalize longitudes relative to Pacific center
    shifted_x <- world_map$x
    shifted_x[!is.na(shifted_x) & shifted_x < (pacific_center - 180)] <-
      shifted_x[!is.na(shifted_x) & shifted_x < (pacific_center - 180)] + 360

    bounds$lng1 <- min(shifted_x, na.rm = TRUE)
    bounds$lng2 <- max(shifted_x, na.rm = TRUE)
  }

  # Prevent excessive span
  max_span <- 360  # Maximum possible longitude span
  current_span <- bounds$lng2 - bounds$lng1
  if (current_span > max_span) {
    # If span is too large, center on mean and limit to max_span
    center_lng <- mean(c(bounds$lng1, bounds$lng2))
    half_span <- max_span / 2
    bounds$lng1 <- center_lng - half_span
    bounds$lng2 <- center_lng + half_span
  }

  list(
    bounds = bounds,
    center = list(
      lng = mean(c(bounds$lng1, bounds$lng2)),
      lat = mean(c(bounds$lat1, bounds$lat2))
    )
  )
}

calculate_zoom_level <- function(bounds, padding_levels = 0) {
  # Calculate spans
  lng_span <- abs(bounds$lng2 - bounds$lng1)
  lat_span <- abs(bounds$lat2 - bounds$lat1)

  # Adjust span calculation based on selection size
  max_span <- max(lng_span, lat_span)

  # For very large spans, ensure we don't zoom out too far
  if (max_span > 180) {
    max_span <- 180
  }

  # Calculate zoom level
  base_zoom <- floor(log2(360/max_span))
  padded_zoom <- base_zoom - padding_levels

  # For single country or small region selections, ensure minimum zoom
  if (lng_span < 45 && lat_span < 45) {  # roughly country-sized
    return(max(padded_zoom, 4))  # don't zoom out past level 4
  }

  # For larger selections, allow more zoom out but prevent excessive
  return(max(min(padded_zoom, 8), 2))  # keep between 2 and 8
}

# Usage examples:
# Less padding (tighter view):
# zoom_level <- calculate_zoom_level(bounds, padding_levels = 0.5)

# More padding (wider view):
# zoom_level <- calculate_zoom_level(bounds, padding_levels = 2)

