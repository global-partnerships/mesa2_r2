
# default_collections <- tibble(
#     name = as.character(),
#     user_id = as.character(),
#     user_full_name = as.character(),
#     languages = as.character(),
#     sharing_option = as.character(),
#     shares = as.character(),
#     date = as.character()
#   )

# default_collections <- tibble(
#     name = "xyz",
#     user_id = "xyz",
#     user_full_name = "xyz",
#     languages = "xyz",
#     sharing_option = "xyz",
#     shares = "xyz",
#     date = "xyz"
#   )

default_collections <- tibble(
    name = "test",
    description = "some text",
    user_id = "morris_johnson@wycliffe.org",
    user_full_name = "Morris Johnson",
    languages = "none",
    sharing_option = "none",
    shares = "none",
    date = as.character(Sys.Date())
  )

append_collection <- function(row, df) {
  df <- df |> 
  bind_rows(row)
}

replace_collection <- function(name, row, df) {
  df <- df |> 
    filter(name != {{name}}) |> 
    bind_rows(row)
}

# *** import collection file from GP Data > Mesa > data > assets
read_collections <- function() {
  
  file_list <- drive_ls(as_id("1b-hVycRS6EjGUQwZ5sbvb_T9ll1dgPni"))
  
  file_id <- file_list |> 
    filter(name == "collections.rds") |> 
    pull(id)
  
  # df <- drive_read_raw(file_id) %>% 
  #   rawConnection() %>% 
  #   readRDS()
  
  conn <- drive_read_raw(file_id) %>% 
    rawConnection()
  
  df <- conn |>  
    readRDS()
  
  close(conn)
  
  return(df)
}

write_collections <- function(df) {
  
  df <- df |> arrange(desc(date))
# print(paste0("write_collections df"))
# str(df)
  
  filename <- "collections.rds"
  
  folder_id <- as_id("1b-hVycRS6EjGUQwZ5sbvb_T9ll1dgPni")
  
  local_path <- tempfile(pattern = "collections", tmpdir = tempdir(), fileext = ".rds")
  
  write_rds(df, local_path, 
            compress = c("none"),
            text = TRUE)
  
  # upload new cache file to drive (i.e., overwrite old cache)
  drive_upload(media = local_path,
               path = folder_id,
               name = filename,
               overwrite = TRUE)
  
  file.remove(local_path) 
}

write_out_prof_html <- function() {
  
  folder_id <- as_id("1DeZ6NmgFxpr7kxupPeyKdpRGXQeH2CNZ") # GP Data > Mesa
  
  local_path <- "www/prof.html"
  
  # upload prof.html file to drive (i.e., overwrite old cache)
  drive_upload(media = local_path,
               path = folder_id,
               name = "prof.html",
               overwrite = TRUE)
  
  file.remove(local_path) 
}

# *** use to update collections data model ***
# write_collections(default_collections)
