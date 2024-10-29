
#' Return only the points located inside a polygon, square, or circle drawn with leaflet.extras drawing tools on a Shiny Leaflet map.
#'
#' @param shape Shiny input (input$MAPID_draw_new_feature), representing shape drawn on the map by the user.
#' @param lang_coords_df A dataframe that includes coordinates and ids for all relevant map locations.
#' @param location_id_colname Column name from lang_coords_df containing desired names or ids for set of locations returned.
#' @return A vector of location ids.
#' @examples
#' mock_input.map_feature <- list(type = "Feature"
#'                          , properties = list(`_leaflet_id`= 13477, feature_type = "rectangle")
#'                          , geometry = list(type = "Polygon"
#'                          , coordinates = list(list(list(-76.15723, 39.51252)
#'                          , list(-76.15723,  40.30467), list(-74.73999, 40.30467)
#'                          , list(-74.73999, 39.51252), list(-76.15723, 39.51252)))))
#' airports <- data.frame('locationID' = c('PHL', 'DTW')
#'                       , 'Longitude' = c(-75.2408, -83.3533)
#'                       , 'Latitude' = c(39.8722, 42.2125))
#' coords = sp::SpatialPointsDataFrame(airports[,c('Longitude', 'Latitude')], airports)
#' findIntersects(shape = mock_input.map_feature
#'                      , lang_coords_df = coords
#'                      , location_id_colname = "locationID")


findIntersects <- function(shape, lang_coords_df, location_id_colname) {
  
  lang_pts_sf <- lang_coords_df |> 
    sf::st_as_sf(coords = c("Longitude", "Latitude"))

  # derive polygon coordinates and feature_type from shape input
  polygon_coordinates <- shape$geometry$coordinates
  feature_type <- shape$properties$feature_type
  
  if(feature_type %in% c("rectangle","polygon")) {
    
    # transform into a spatial polygon
    coords_list <- shape$geometry$coordinates |>
      list_flatten() |>
      list_flatten() |>
      list_c() |>
      matrix(ncol = 2, byrow = TRUE) |> 
      list()
    
    drawn_polygon <- sf::st_polygon(coords_list)
    
    intersects <- sf::st_intersects(lang_pts_sf, drawn_polygon, sparse = FALSE)
    
    selected_loc_ids <- lang_coords_df |> 
      mutate(selected = intersects) |> 
      filter(selected == TRUE) |> 
      pull(all_of(location_id_colname))

    return(selected_loc_ids)

  } else if (feature_type == "circle") {
    
    center_coords <- matrix(c(polygon_coordinates[[1]], polygon_coordinates[[2]])
                            , ncol = 2)
    
    center_coords_sf <- st_as_sf(center_coords)
    
    # get distances to center of drawn circle for all locations in location_coordinates
    # distance is in kilometers
    dist_to_center <- sf::st_distance(lang_pts_sf, 
                                      center_coords, 
                                      # center_coords_sf, 
                                      by_element = TRUE) 
    
    # dist_to_center <- sp::spDistsN1(lang_pts_sf, center_coords, longlat = TRUE)
    
    # get location ids
    # radius is in meters
    intersects <- lang_pts_sf[dist_to_center < shape$properties$radius/1000, location_id_colname]
    
    selected_loc_ids <- lang_coords_df |> 
      mutate(selected = intersects) |> 
      filter(selected == TRUE) |> 
      pull(all_of(location_id_colname))
    
    return(selected_loc_ids)
  }
}