get_WIP_rows <- function(rows, user_info) {
  req(user_info)
  req(user_info$confidential)
  # This function filters the rows based on the user's confidentiality and restriction level
  # and returns the filtered rows
  rows <- {{rows}} |>
    filter(`Organization Name` != "Global Partnerships") # remove duplicates of Wycliffe USA

  user_info <- {{user_info}}

  user_org <- user_info$pb_org_name
  col_names <- names(rows)
  cols2mask <- c("Organization Name", "Project Name", "Domains", "Activities", "Engagement Functions",
                 "Translation Type", "Translation Goals")

  if (user_info$confidential == "yes") {
    df <- rows
  } else {
    dfx <- rows |> filter(Sensitivity == "Confidential")
    dfy <- rows |> filter(Sensitivity != "Confidential")
    dfz <- dfx |>
        mutate(across(all_of(cols2mask), ~ str_replace(.,"^.*$", "Confidential")))
    df <- dfy |> rbind(dfz)
  }

  if (user_info$restricted == "no") {
    dfx <- df |> filter(Sensitivity == "Restricted")
    dfy <- df |> filter(Sensitivity != "Restricted")
    dfz <- dfx |>
      mutate(across(all_of(cols2mask), ~ str_replace(.,"^.*$", "Restricted")))
    df <- dfy |> rbind(dfz)
  }


  df <- df |>
    arrange(`Country Code`, `Language Code`)

  return(df)
}
