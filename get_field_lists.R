

# *** etl for field hierarchies list

get_field_lists <- function() {

  # raw_field_list <- read.csv("R/data/assets/combined_field_list.csv")
  raw_field_list <- read.csv("data/assets/combined_field_list.csv")
  # field_list_ss <- "1hMz9wLWj-etek40T5c0JNkkZ0MM0fUuqdAo9KmOoPSE"
  # raw_field_list <- read_sheet(field_list_ss, sheet = "field_list")

  df_temp <- raw_field_list %>% # initial wrangling of combined_field_list
    pivot_longer(cols = !field_name:Topic,
                 names_to = "Source",
                 values_to = "included") %>%
    filter(included == TRUE) %>%
    select(-included)

  df_dashboards <- df_temp %>% # extract dashboard fields
    filter(str_detect(.$Source, "DB")) |>
    mutate(Dashboard = str_extract(Source, "(?<=DB\\.\\.).+")) |>
    select(field_name, Dashboard) |>
    mutate(Dashboard = str_replace_all(Dashboard, "Country", "Status by Country")) |>
    mutate(Dashboard = str_replace_all(Dashboard, "\\.", " "))

  df_sources <- df_temp %>% # extract api fields
    filter(!str_detect(.$Source, "DB")) %>%
    mutate(Source = str_extract(.$Source, "(?<=PB\\.\\.).+")) %>%
    mutate(Source = str_replace_all(.$Source, "\\.", " ")) %>%
    filter(!is.na(Source)) |>
    select(field_name, Topic, Source)

  field_dfs <- list(
    dashboards = df_dashboards,
    sources = df_sources
  )

  return(field_dfs)

  # rm(df_temp)
  # rm(raw_field_list)
}
