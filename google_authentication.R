# *** drive authentication  ***

library(googledrive)
# library(googlesheets4)

options(gargle_oauth_cache = ".secrets",
        gargle_oauth_email = TRUE)

drive_auth(scopes = "https://www.googleapis.com/auth/drive", # read and write
           email = "global-partnerships-analytics@partnerships.global")

gs4_auth(scopes = "https://www.googleapis.com/auth/spreadsheets", # read and write
         email = "global-partnerships-analytics@partnerships.global")