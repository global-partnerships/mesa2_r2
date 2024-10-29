get_db_chart <- function(df, dashboard, switch_cats) {
  title_text <- paste0(dashboard, " Summary")

  df_summary <- prepare_data(df, dashboard, switch_cats)

  if (dashboard == "Engagements") {
    n_plot <- create_engagements_plot(df_summary, switch_cats)
    # tooltip <- c("Engagement Status", "Count")
    if(switch_cats == 0){
      tooltip <- c("Engagement Status", "Count")
    } else {
      tooltip <- c("Country", "Count")
    }
  } else {
    # n_plot <- create_standard_plot(df_summary)
    n_plot <- create_standard_plot(df_summary, switch_cats)
    if(switch_cats == 0){
      tooltip <- c("Country", "Count")
    } else {
      tooltip <- c("Translation Status", "Count")
    }
  }

  p_plot <- ggplotly(
    n_plot,
    height = 400,
    tooltip = tooltip,
    source = "A"
  ) %>%
    plotly::config(displayModeBar = FALSE)
}

prepare_data <- function(df, dashboard, switch_cats) {
  out <- switch(
    dashboard,
    "All Access" = prepare_all_access_data(df, switch_cats),
    "Vision 2025" = prepare_vision_2025_data(df, switch_cats),
    "Sign Language" = prepare_sign_language_data(df, switch_cats),
    "Engagements" = prepare_engagements_data(df, switch_cats),
    df  # default case
  )

  y_cat = names(out)[2]
  fill_cat = names(out)[1]

  out2 <- out %>%
    group_by(across(2)) %>%
    mutate(total = sum(Count)) %>%
    mutate(lab_offset = sum(Count) * 0.05) %>%
    ungroup()

  # Apply custom ordering only if y_cat is 'Country'
  if (y_cat == "Country") {
    # For 'Country', order by total count descending
    country_order <- out2 %>%
      group_by(.data[[y_cat]]) %>%
      summarise(total = sum(Count)) %>%
      arrange(desc(total)) %>%
      pull(.data[[y_cat]])

    out2 <- out2 %>%
      mutate(!!y_cat := factor(.data[[y_cat]], levels = country_order))
  }

  return(out2)
}

prepare_all_access_data <- function(df, switch_cats) {
  df2 <- df |>
    filter(`On All Access List` == "Yes") |>
    distinct(`Country Code`, `Language Name`, .keep_all = TRUE)

  if (switch_cats == 1) {
    df <- df2 %>%
      group_by(`All Access Status`, Country) %>%
       summarise(Count = n(), .groups = "drop")
  } else {
    df <- df2 |>
      group_by(Country, `All Access Status`) |>
      summarise(Count = n(), .groups = "drop")
      # summarise(Count = n(), .groups = "drop") |>
      # arrange(desc(Count))
      # arrange(desc(Country))
  }

  return(df)
}

prepare_vision_2025_data <- function(df, switch_cats) {
  df2 <- df |>
    filter(`Is Remaining V2025 Need` == "Yes") |>
    distinct(`Country Code`, `Language Name`, .keep_all = TRUE)

  if (switch_cats == 1) {
    df <- df2 %>%
      group_by(`Translation Status`, Country) %>%
      summarise(Count = n(), .groups = "drop")
  } else {
    df <- df2 |>
      group_by(Country, `Translation Status`) |>
      summarise(Count = n(), .groups = "drop")
      # summarise(Count = n(), .groups = "drop") |>
      # arrange(desc(Count))
      # arrange(desc(Country))
  }

  return(df)
}

prepare_sign_language_data <- function(df, switch_cats) {
  df2 <- df |>
    filter(`On All Access List` == "Yes",
           `Is Sign Language` == "Yes") |>
    select(-`Is Sign Language`) %>%
    distinct(`Country Code`, `Language Name`, .keep_all = TRUE)

  if (switch_cats == 1) {
    df <- df2 %>%
      group_by(`All Access Status`, Country) %>%
      summarise(Count = n(), .groups = "drop")
  } else {
    df <- df2 |>
      group_by(Country, `All Access Status`) |>
      summarise(Count = n(), .groups = "drop")
  }

  return(df)
}

prepare_engagements_data <- function(df, switch_cats) {
  if(switch_cats == 1) {
    df2 <- df %>%
      group_by(`Engagement Status`, Country, `Organization Name`) %>%
      summarise(Count = n(), .groups = "drop")
  } else {
    df2 <- df %>%
      group_by(Country, `Engagement Status`, `Organization Name`) %>%
      summarise(Count = n(), .groups = "drop")
  }
  status_order <- c('Past', 'Inactive', 'Current', 'Future', 'Planned', 'Relationship', 'Intent')
  df2 <- df2 |>
    mutate(`Engagement Status` = `Engagement Status` |> factor(levels = status_order))

  # For 'Country', order by total count descending
  # country_order <- df2 %>%
  #   group_by(Country) %>%
  #   summarise(total = sum(Count)) %>%
  #   arrange(desc(total)) %>%
  #   pull(Country)
  #
  # df2 <- df2 %>%
  #   mutate(Country := factor(Country, levels = country_order))

  return(df2)
}

create_engagements_plot <- function(df_summary, switch_cats) {
  # Define custom order for Engagement Status

  y_cat <- names(df_summary)[2]
  fill_cat <- names(df_summary)[1]

  df <- df_summary %>%
    filter(!is.na(.data[[fill_cat]])) %>%
    group_by(across(2)) %>%
    mutate(total = sum(Count)) %>%
    mutate(lab_offset = sum(Count) * 0.08) %>%
    ungroup()

  country_order <- df$Country |> levels()

  # Apply custom ordering only if y_cat is 'Engagement Status'
  if (y_cat == "Engagement Status") {
    status_order <- c('Past', 'Inactive', 'Current', 'Future', 'Planned', 'Relationship', 'Intent')
    df <- df %>%
      mutate(!!y_cat := factor(.data[[y_cat]], levels = status_order))
  } else {
    # arrange(desc(Country))
    # For 'Country', order by total count descending
    # country_order <- df %>%
    #   group_by(.data[[y_cat]]) %>%
    #   summarise(total = sum(Count)) %>%
    #   arrange(desc(total)) %>%
    #   pull(.data[[y_cat]])
    #
    # df <- df %>%
    #   mutate(!!y_cat := factor(.data[[y_cat]], levels = country_order))
  }

  plot <- df %>%
    ggplot2::ggplot(aes(y = .data[[y_cat]], x = Count)) +
    geom_col(aes(fill = .data[[fill_cat]],
                 text = paste("Country:", `Country`,
                              "\nOrganization:", `Organization Name`,
                              "\nEngagement Status", `Engagement Status`,
                              "\nCount:", Count))) +
    geom_text(aes(label = total, x = total),
              nudge_x = df$lab_offset,
              size = 3,
              fontface = "bold") +
    scale_x_continuous(expand = expansion(mult = c(0, .2))) +
    theme(axis.text.y = element_text(size = 11)) +
    ylab("") + xlab("")

  # Apply reverse order for y-axis
  if (y_cat == "Engagement Status") {
    plot <- plot + scale_y_discrete(limits = rev(status_order))
  } else {
    plot <- plot + scale_y_discrete(limits = rev(country_order))
  }

  interactive_plot <- ggplotly(plot, tooltip = "text")

  return(interactive_plot)
}

# create_standard_plot <- function(df_summary) {
create_standard_plot <- function(df_summary, switch_cats) {
  y_cat = names(df_summary)[2]
  fill_cat = names(df_summary)[1]

  current_status_field <- ifelse(switch_cats == 0, y_cat, fill_cat)

  df <- df_summary %>%
    group_by(across(2)) %>%
    mutate(total = sum(Count)) %>%
    mutate(lab_offset = sum(Count) * 0.05) %>%
    ungroup()

  # Apply custom ordering only if y_cat is 'Country'
  if (y_cat == "Country") {
    # For 'Country', order by total count descending
    country_order <- df %>%
      group_by(.data[[y_cat]]) %>%
      summarise(total = sum(Count)) %>%
      arrange(desc(total)) %>%
      pull(.data[[y_cat]])

    df <- df %>%
      mutate(!!y_cat := factor(.data[[y_cat]], levels = country_order))
  }

  plot <- df |>
    ggplot2::ggplot(aes(y = .data[[y_cat]],
                        x = Count,
                        text = paste("Country:", `Country`,
                              "\nStatus:", .data[[current_status_field]],
                              "\nCount:", Count))) +
    geom_col(aes(fill = .data[[fill_cat]])) +
    geom_text(aes(label = total, x = total),
              nudge_x = df$lab_offset,
              size = 3.5,
              fontface = "bold") +
    scale_x_continuous(expand = expansion(mult = c(0, .2))) +  # Expand x-axis to make room for labels
    theme(axis.text.y = element_text(size = 11)) +
    ylab("") + xlab("")

  # Apply reverse order for y-axis
  if (y_cat == "Country") {
    plot <- plot + scale_y_discrete(limits = rev(country_order))
  }

  return(plot)
}
