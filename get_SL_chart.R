library(tidyverse)
library(ggplot2)
library(janitor)

# write.csv(df, "mesa_elt_data.csv")

# main_df <- read.csv("mesa_elt_data.csv") %>% select(-X) %>% janitor::clean_names()

get_SL_chart <- function(df) {

# df <- main_df %>% 
    df <- df %>% 
      filter(`Is Sign Language` == "Yes") %>% 
      select(
        `Language Name`,
        `Language Code`,
        `Is Sign Language`,
        `EGIDS (ELG)`,
        `All Access Status`,
        `All Access Goal`,
        `Translation Status`,
        `Is Remaining V2025 Need`,
        `On All Access List`,
        `Access through 2nd Language`) %>% 
      mutate(on_AA_list = if_else(`On All Access List` == "Yes", "Yes", "No")) %>% 
      mutate(low_vitality = if_else(`Translation Status` == "Low Language Vitality",
                                    "Low Vitality", 
                                    "n/a")) %>%
      mutate(second_language = if_else(`Access through 2nd Language` != "Not listed", 
                                       "Access via 2nd Language", 
                                       "n/a")) %>%
      mutate(`Mitigators` = paste0(low_vitality, " and/or ", second_language)) %>% 
      select(-low_vitality, -second_language)
    
      
      df$`Mitigators` <- df$`Mitigators` %>% 
        str_replace_all(" and/or n/a", "") %>% 
        str_replace_all("n/a and/or ", "")
      
    plot <- ggplot(df, aes(`All Access Status`, fill = `All Access Goal`)) +
      geom_histogram(position = "dodge",
                     stat = "count") +
      facet_grid(`Mitigators` ~ .)
    
    plot + theme(
      text = element_text(size = 12)
    )
    
    plot +
      xlab("All Access Status") +
      ylab("Count of Languages") +
      labs(title = "Sign Languages (Global) by All Access Status and Goal")
    
    # return(plot)
    
    
    # ggplot(df, aes(all_access_status, fill = Mitigators)) +
    #   geom_histogram(position = "dodge",
    #                  stat = "count") +
    #   # geom_bar(stat = "count",
    #   #          position = "dodge") +
    #   facet_grid(all_access_goal ~ .)
}
