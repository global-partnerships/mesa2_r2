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


