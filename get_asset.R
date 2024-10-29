get_asset <- function(filename, ext, dribble) {
  file_id <- dribble %>% 
    filter(str_detect(.$name, filename)) %>% pull(id)
  
  local_file_name <- paste0(make_clean_names(filename), "_local_")
  local_path <- tempfile(pattern = local_file_name, fileext = ext)
  
  # str(file_id)
  # str(local_file_name)
  # str(local_path)
  
  drive_download(file_id, path = local_path, overwrite = TRUE)
  
  if (ext == ".doc") {
    asset <- htmlTemplate(local_path)
  } else if (ext == ".geojson") {
    asset <- geojsonio::geojson_read(local_path, 
                                     parse = TRUE, 
                                     what = "sp",
                                     stringsAsFactors = TRUE)
    
    # asset <- rgdal::readOGR(local_path)
    asset2 <- sf::read_sf(
      dsn = local_path,
      type = 6)
    
    # asset2$area <- sf::st_area(asset2$geometry)
    
    # print(paste("class: ", class(asset2)))
    # print(paste("names: ", names(asset2)))
    # str(asset2)
    # asset2 <- arrange(asset2, -area)
    
    # if (filename == "StratLangShapes_v2") {
    #   asset <- readLines(local_path) %>% paste(collapse = "\n")
    # } else {
    #   asset <- sf::read_sf(local_path)
    # }
  } else if (ext == ".csv") {
    asset <- read.csv(local_path)
  }
  return(asset)
  rm(asset)
  file.remove(local_path)
}
