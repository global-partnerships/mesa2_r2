get_summary_field <- function(df, head, detail, fld_name) {
  out <- df %>% 
    group_by(`Language Code`) %>%
    reframe(head = str_c( {{head}} ),
            detail = str_c( {{detail}} )) %>% 
    mutate(entry = paste0("<b>", head, "</b>", " (", detail, ")")) %>% 
    group_by(`Language Code`) %>% 
    summarise( {{fld_name}} := str_c(entry, collapse = ", "))
}