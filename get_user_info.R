get_gp_app_users <- function() {
  gp_app_users_ss <<- as_id("1m37eThcH2BbCjRNv1NIQVzJ7e0Fu7KveDhs1NXwDZls")
  gp_app_users_df <<- read_sheet(gp_app_users_ss, sheet = "gp_app_users")
}

get_mesa_users <- function(data) {
  mesa_users_df <<- data |> 
    mutate(email = tolower(email)) |> 
    filter(mesa_user == "yes")
}
  
get_user_info <- function(data, user) {
  if({{user}} == "Guest@partnerships.global") {
    user_info <- tibble(
      full_name = "Guest User",
      email = "Guest@partnerships.global",
      pb_org_name = "Wycliffe USA",
      first = "Guest",
      last = "User",
      mesa_user = "yes",
      home_user = "no",
      mesa_staging = "no",
      home_staging = "no",
      organization = "Wycliffe USA",
      role = "Field Coordinator",
      unrestricted = "yes",
      restricted = "yes",
      confidential = "yes",
      admin = "no"
    )
  } else {
    user_info <- mesa_users_df |> 
      filter(email == {{user}})
  }
  admin <<- user_info$admin
  return(user_info)
}
  
get_org_users <- function(data) {
  if(user_info$admin == "yes") {
    admin <<- "yes"
    org_users <<- mesa_users_df |> 
      filter(pb_org_name == user_info$pb_org_name) |> 
      select(full_name, email, unrestricted, restricted, confidential, admin)
      # select(full_name, email, pb_org_name, unrestricted, restricted, confidential, admin)
  }
  return(org_users)
}
  
  
  # rm(mesa_users_df)