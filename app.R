# options(shiny.fullstacktrace = TRUE)
# options(shiny.maxRequestSize = 30*1024^2)  # Increase max request size to 30MB
# options(future.globals.maxSize = 3000 * 1024^2) # If using future package
message("Starting app initialization...")

message("Loading libraries...")
library(mgcv)
library(tidyverse)
library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyjqui)
library(shinydashboard)
# library(shinydashboardPlus)
# library(bs4Dash)
library(rclipboard)
library(arrow)
library(reactable)
library(reactablefmtr)
library(leaflet)
library(leaflet.extras)
library(sf)
library(DT)
library(htmltools)
library(markdown)
library(plotly)
library(openxlsx)
library(glue)
library(gt)
library(htmlTable)
library(googledrive)
library(googlesheets4)
library(sortable)
library(cookies)
# library(mongolite)
library(data.table)
library(RColorBrewer)
library(scales)
# library(geosphere)


# library(profvis)

message("Libraries loaded")

source("google_authentication.R")
source("get_map_data.R")
source("get_SL_chart.R")
source("get_db_chart.R")
source("get_field_lists.R")
source("findIntersects.R")
source("get_user_info.R")
source("get_summary_field.R")
source("get_WIP_rows.R")
source("get_collections.R")
source("utils.R")
source("get_history_functions.R")
# source("update_country_selectors.R")

# config <- config::get()


# **************** global options *****************


    # uncomment to debug app reactivity
# reactlog::reactlog_enable()


    # uncomment the desired line to experiment with shiny.sanitize.errors
# options(shiny.sanitize.errors = TRUE)
# options(shiny.sanitize.errors = FALSE)

# readRenviron(".Renviron")


# **************** global variables ****************

version <- "1.0.15"
# version <- "1.0.14"
# version <- "1.0.13"
# version <- "1.0.12"
# version <- "1.0.11"
# version <- "1.0.10"
# version <- "1.0.9"
# version <- "1.0.8"
# version <- "1.0.7"
# version <- "1.0.6"
# version <- "1.0.5"
# version <- "1.0.4"
# version <- "1.0.3"
# version <- "1.0.2"
# version <- "1.0.1"
# version <- "1.0.0"

main_sources <- c("Translation Status")

# ****** functions ******

csvDownloadButton <- function(id, filename = "data.csv", label = "Download as CSV") {
  tags$button(
    tagList(icon("download"), label),
    onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')", id, filename)
  )
}

get_DT_details_obj <- function(data) {
  table_names <- names(data)
  datatable(
    data = data,
    caption = "Tip: Select rows in main table above to display details.",
    filter = list(position = 'top', clear = TRUE, plain = TRUE),
    escape = FALSE,
    rownames = FALSE,
    width = NULL,
    # fillContainer = TRUE,
    class = 'display compact',
    # selection = list(mode = "single", target = "cell"),
    extensions = c('Buttons', 'ColReorder'),
    options = list(
                   scrollY = TRUE,
                   scrollX = TRUE,
                   scrollcollapse = TRUE,

                   # scrollY = '80vh',
                   # # scrollX = 'true',
                   # scrollX = 'false',
                   # scrollcollapse = FALSE,
                   dom = 'Blfrtip',
                   buttons = list('copy', 'print',
                                  list(extend = 'collection',
                                       buttons = c('csv', 'excel', 'pdf'),
                                       text = 'Download')
                   ),
                   lengthMenu = list(c(-1, 10, 25, 50), c("All", 10, 25, 50)),
                   colReorder = TRUE
            )
  ) %>%
    { if("1st Language Pop" %in% table_names)
        formatCurrency(., columns = "1st Language Pop",
                       currency = "",
                       digits = 0)
      else .
    }
}

get_DT_main_obj <- function(data) {
# get_DT_main_obj <- function(data, rows_selected) {
  table_names <- names(data)
  title <- "Mesa table export"
  top_message <- paste0("For internal use only. Please do not share publicly without permission. ",
                        "Data provided by ProgressBible™ on ", today())
  datatable(
    data = data,
    caption = "Tip: Use the search box and/or column filters to refine list. Select rows to show details in table below.",
    filter = list(position = 'top', clear = TRUE, plain = TRUE),
    escape = FALSE,
    rownames = FALSE,
    width = NULL,
    # fillContainer = TRUE,
    class = 'display compact',
    extensions = c('Buttons'),
    # extensions = c('Select', 'Buttons'),
    options = list(
      scrollY = TRUE,
      scrollX = TRUE,
      scrollcollapse = TRUE,
      # scrollY = '80vh',
      # scrollX = 'true',
      # scrollX = 'false',
      # scrollcollapse = FALSE,
      # select = list(
      #   style = 'multi+shift',
      #   items = 'row'),
      dom = 'Blfritip',
      rowId = 0,
      buttons = list(
        list(extend = "copy", title = NULL,
             text = "Copy to clipboard",
             messageTop = top_message),
        list(extend = "excel", title = NULL,
             filename = paste0("Mesa export - ", today()),
             text = "Export to Excel",
             messageTop = top_message,
             customize = DT::JS("function(xlsx) {
               var sheet = xlsx.xl.worksheets['sheet1.xml'];
               $('row:first c', sheet).attr( 's', '50' );}"))
        ),
      lengthMenu = list(c(-1, 10, 25, 50), c("All", 10, 25, 50))
    ),
    selection = list(
      mode = 'multiple',
      selected = NULL,
      # selected = rows_selected,
      target = 'row')
    # selection = list(
    #   selected = rows_selected
    # )
    # selection = 'none'
  ) %>%
    { if("1st Language Pop" %in% table_names)
      formatCurrency(., columns = "1st Language Pop",
                     currency = "",
                     digits = 0)
      else .
    }
}

get_details_data <- function(id, df) {
  df <- df %>%
    filter(id == {{id}}) %>%
    tidyr::unnest(cols = c(data)) %>%
    select(-id, -tab_label, -nrows)
}

# ********* cache file information **********

# cache_file_list <- list.files(pattern = "\\.feather",
#                               include.dirs = TRUE,
#                               recursive = TRUE) # for publication
#
# cache_file_info <- cache_file_list |>
#   purrr::map(file.info) |> purrr::list_rbind()
#
# max_date_accessed <- max(cache_file_info$mtime, na.rm = TRUE) |>
#   as.character() |> stringr::str_extract(".+(?=\\s)")

sqlite_file_list <- list.files(pattern = "\\.sqlite",
                              include.dirs = TRUE,
                              recursive = TRUE) # for publication

sqlite_file_info <- sqlite_file_list |>
  purrr::map(file.info) |> purrr::list_rbind()

max_date_accessed <- max(sqlite_file_info$mtime, na.rm = TRUE) |>
  as.character() |> stringr::str_extract(".+(?=\\s)")

attribution_part_1 <- "Data provided by the following sources: ProgressBible™, accessed on "
attribution_part_2 <- "; Eberhard, David M., Gary F. Simons, and Charles D. Fennig (eds.). 2023. Ethnologue: Languages of the World. Twenty-sixth edition. Dallas, Texas: SIL International."
attribution_part_3 <- "; Glottolog 4.4 edited by Hammarström, Harald & Forkel, Robert & Haspelmath, Martin & Bank, Sebastian; Alan Starling (ed.). 2021."
attribution_part_4 <- "; Registry of Language Varieties (ROLV). Online version (https://globalrecordings.net/en/rolv)."

attribution_text <- paste0(attribution_part_1, max_date_accessed, attribution_part_2, attribution_part_4)


# ********** color palettes *********
# for time zones in langMap
tz_pal <- colorNumeric(palette = "Blues",
                       domain = NULL,
                       # domain = timezones_sf$tz_count,
                       na.color = "white")

# for map markers, to match Progress Bible status color scheme
status_color_palettes <- tibble(status_type  = c("Translation Status",
                                                 "All Access Status",
                                                 "Is Remaining V2025 Need",
                                                 "On All Access List"),
                                status_value = list( c("Expressed Need",
                                                       "Limited or Old Scripture",
                                                       "Low Language Vitality",
                                                       "Potential Need",
                                                       "Recent Scripture",
                                                       "Work In Progress",
                                                       "Not available"),
                                                     c("Goal Met through L2",
                                                       "Goal Met through L1",
                                                       "Translation in Progress",
                                                       "Translation Not Started",
                                                       "Not Shown",
                                                       "Not available"),
                                                     c("No", "Yes"),
                                                     c("No", "Yes")),
                                color = list( c("#5E5E5E",
                                                "#00AD13",
                                                "#FF7A1E",
                                                "#ADB528",
                                                "#0084E9",
                                                "#782E2E",
                                                "#E50606"),
                                              c("#D9EEF1",
                                                "#B9DDF1",
                                                "#5E93B9",
                                                "#027ED7",
                                                "#555555",
                                                "#5E5E5E"),
                                              c("#5E93B9",
                                                "#782E2E"),
                                              c("#5E93B9",
                                                "#782E2E"))
)


# **************** Define UI for application *********************
# ui <- add_cookie_handlers(
ui <-
  dashboardPage(

    # useShinyjs(),
    # skin = "green",
    # skin = "blue",
    skin = "yellow",
    #
    title = paste0("Mesa ", version),

    # Application title
    dashboardHeader(
      title = paste0("Mesa ", version),
      tags$li(
        class = 'dropdown',
        style = "text-align:center",
        style = "font-color:white",
        h4(strong(htmlOutput("txt_V2025TimeRemaining"), style = 'color:white;'))
      ),
      dropdownMenuOutput("messageMenu"),
      dropdownMenuOutput("notificationMenu")
    ),

    # Sidebar with a slider input for number of bins
    dashboardSidebar(
      # radioButtons("profile", "Profiling", c("off", "on")),
      textOutput("user_greeting"),
      useShinyjs(),
      # uiOutput("snapshotdate"),
      uiOutput("partners"),
      uiOutput("areas"),
      uiOutput("link_select_all_areas"),
      uiOutput("countries"),
      uiOutput("link_select_all_countries"),
      hr(),
      uiOutput("manage_collections"),
      hr(),
      tags$p(
        style="margin-left: 20px",
        # style="text-align:center",
        a("Email Support",
          href = "mailto:mesa-support@partnerships.global",
          target = "_blank")
      ),
      hr(),
      tags$p(
        style="margin-left: 20px",
        # style="text-align:center",
        # style="margin-bottom: 0px",
        a("Provide input to ProgressBible",
          href = "https://docs.google.com/document/d/1mGchdGTjRhohSC1vBS84JLeHHoVNcRsUagXmq0dSQio/edit",
          target = "_blank")
      )
    ),

    dashboardBody(
        rclipboardSetup(), #adds clipboard.js to head tag

        # use_copy(),
        useShinyjs(),

        # css support
        tags$head(
          tags$script(src = "scroll_functions.js"),
          tags$link(rel = "shortcut icon", href = "favicon.ico"),
          tags$style(HTML("map-citation-footer {width: 80%;}")),
          tags$style(".rightAlign { display: flex; justify-content: flex-end; }"),
          tags$style(HTML(".dataTables_scrollBody {max-height: 80vh !important; height: auto !important;}")),
          tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),

        fluidRow(
          # tabsetPanel(
          tabBox(
             id = "main_page_tabset",
             width = 12,
             tabPanel(
               title = "Dashboards",
               tabName = "dashboards",
               fluidRow(
                 column(
                   width = 12,
                   valueBoxOutput("vb_all_languages"),
                   valueBoxOutput("vb_v2025"),
                   valueBoxOutput("vb_all_access")
                 )
               ),
               fluidRow(
                 column(
                   width = 12,
                   tagList(
                     tags$h5("Tip: Click a colored box above to filter language markers and list below.")
                   )
                 )
               ),
                fluidRow(
                  column(width = 6,
                    uiOutput("choose_dashboard")
                  )
                ),
                fluidRow(
                  box(
                    title = "Strategic Summaries",
                    width = 12,
                    column(width = 12,
                      tabsetPanel(
                        id = "summary_panels",
                        type = "pills",
                        tabPanel(
                          value = "v2025_summaries",
                          title = "Vision 2025",
                          tabsetPanel(
                            id = "ts_summaries",
                            type = "pills",
                            tabPanel(
                              id = "ts_area_countries",
                              title = "Areas and country summaries",
                              div(class = "action_link",
                                  actionLink(
                                    inputId = "link_change_V2025_summary_cols",
                                    label = "Select/Order Columns")
                              ),
                              reactableOutput("V2025_summary_table") |>
                                shinycssloaders::withSpinner(type = 5),
                              downloadButton("download_v2025_data", "Download")
                            ),
                            tabPanel(
                              id = "ts_history",
                              title = tagList(span("Vision 2025 Status History")),
                              # title = tagList(span("Vision 2025 Status History"),
                              #                 span(" NEW!", style = "font-size: 12px; color: red; font-weight: bold;")),
                              fluidRow(
                                column(
                                  width = 3,
                                  uiOutput("select_country_scope_ts")
                                ),
                                column(
                                  width = 3,
                                  uiOutput("select_first_snapshot_ts")
                                ),
                                column(
                                  width = 3,
                                  uiOutput("select_last_snapshot_ts")
                                )
                              ),
                              fluidRow(
                                column(width = 6,
                                   tags$script(HTML(
                                     "function setinput2(id) {Shiny.setInputValue('clicked_hist_id', id);}"
                                   )),
                                   reactableOutput("table_trans_status_history") |>
                                     shinycssloaders::withSpinner(type = 5)
                                ),
                                column(width = 6,
                                  plotOutput("remaining_needs_plot")
                                )
                              )
                            )
                          )
                        ),
                        tabPanel(
                          value = "aa_summaries",
                          title = "All Access",
                          tabsetPanel(
                            type = "pills",
                            tabPanel(
                              id = "aa_area_countries",
                              type = "pills",
                              title = "Area and country summaries",
                              div(class = "action_link",
                                  actionLink(
                                    inputId = "link_change_all_access_summary_cols",
                                    label = "Select/Order Columns")
                              ),
                              reactableOutput("all_access_summary_table") |>
                                shinycssloaders::withSpinner(type = 5),
                              downloadButton("download_aa_data", "Download")
                              ),
                            tabPanel(
                              title = tagList(span("All Access Status History")),
                              # title = tagList(span("All Access Status History"),
                              #                 span(" NEW!", style = "font-size: 12px; color: red; font-weight: bold;")),
                              fluidRow(
                                column(
                                  width = 3,
                                  uiOutput("select_country_scope_aa")
                                ),
                                column(
                                  width = 3,
                                  uiOutput("select_first_snapshot_aa")
                                ),
                                column(
                                  width = 3,
                                  uiOutput("select_last_snapshot_aa")
                                )
                              ),
                              fluidRow(
                                column(width = 6,
                                     tags$script(HTML(
                                       "function setinput3(id) {Shiny.setInputValue('clicked_aa_hist_id', id);}"
                                     )),
                                     reactableOutput("table_aa_status_history") |>
                                       shinycssloaders::withSpinner(type = 5)
                                ),
                                column(width = 6,
                                  tabsetPanel(
                                    type = "pills",
                                    tabPanel(
                                      title = "Goals Unmet, by month, with projections",
                                      plotOutput("goals_not_met_line_plot")
                                    ),
                                    tabPanel(
                                      title = "Goals met, by month",
                                      plotOutput("goals_met_line_plot")
                                    ),                                    tabPanel(
                                      title = "Goals Unmet vs Met, by year",
                                      plotOutput("goals_unmet_vs_met_plot")
                                    )
                                  )
                                )
                              )
                            )
                          )
                        ),
                        tabPanel(
                          value = "summary_map",
                          title = "Map (summaries by country)",
                          # value = "tab_summary_map",
                          column(
                            width = 12,
                            leafletOutput("summary_map",
                                          width = "100%",
                                          height = 600) |>
                              jqui_resizable() |>
                              shinycssloaders::withSpinner(type = 5)
                          ),
                          column(
                            width = 3,
                            downloadButton("download_summary_map_html",
                                           label = "Download as HTML")
                            # , downloadButton("download_summary_map_png",
                            #                label = "Download as image")
                          ),
                          column(width = 4,
                                 uiOutput("select_marker_status_type")
                          )
                        ),
                        tabPanel(
                          value = "distribution",
                          title = "Distribution (by selected countries and view)",
                          div(class = "rightAlign",
                              uiOutput("lnk_switch_categories")
                          ),
                          plotlyOutput("dashboard_chart") |>
                            shinycssloaders::withSpinner(type = 5)
                        )
                      )
                    )
                  )
                ),
                fluidRow(
                  div(class = "dashboard-source_citations",
                      style = "font-size: 10px",
                      attribution_text
                  )
                ),
                fluidRow(
                  column(width = 12,
                     div(id = 'db-details-container',
                       box(
                         title = "Language details (by selected countries and dashboard)",
                         width = 12,
                         div(
                           id = "scrollable-content",
                           style = "max-height: 600px; overflow-y: auto;",
                           tabsetPanel(
                             id = "language_details_tabset",  # Add this ID
                             type = "hidden",
                             tabPanel(
                               title = NULL,
                               width = 12,
                               DTOutput("table_dashboard_langs") |>
                                 shinycssloaders::withSpinner(type = 5),
                               uiOutput("btn_send_db_langs_to_RT"),
                               hr(),
                               div(id = "citation-wrapper",
                                   div(id = "db_details_table_citation",
                                       class = "dashboard-source_citations",
                                       style = "font-size: 10px; padding-top: 20px; padding-bottom: 20px;",
                                       attribution_text
                                   )
                               )
                             )
                           )
                         )
                       )
                     )
                  )
                )
             ),
             tabPanel(
                  title = "Research Table Builder",
                  value = "table_builder",
                  # tabName = "table_builder",
                  fluidRow(
                    column(
                      width = 2,
                      uiOutput("select_topic")),
                      # hr(),
                    column(
                      width = 2,
                      uiOutput("select_fields"),
                      actionLink("select_all_fields", "Select/Unselect all")
                    ),
                    column(
                      width = 8,
                      fluidRow(
                       tags$div(class="header", checked=NA,
                                tags$b("Selected countries by area/region(s):")),
                       gt_output("gt_stored_countries")
                      ),
                      fluidRow(
                        tags$div(class="header", checked=NA,
                                 tags$b("Selected fields by topic(s):")),
                        gt_output("gt_stored_fields")
                      )
                    )
                  ),
                  hr(),
                  fluidRow(
                      column(
                        width = 2,
                        # width = 2, offset = 10,
                        actionLink("lnk_adv_srch",
                                    label = "Search by Language (ISO) Code",
                                    icon = icon("magnifying-glass-plus"))
                      ),
                      column(
                        width = 3,
                        # width = 2, offset = 10,
                        uiOutput("save_collection_link")
                      ),
                    box(
                      width = 12,
                      title = "Main Table",
                      tags$script(HTML(
                        "function setinput(id) {Shiny.setInputValue('clicked_bib_id', id);}"
                      )),
                      shinycssloaders::withSpinner(type = 5,
                          DTOutput("research_table")
                          # DTOutput("research_table", height = "100%")
                      ),
                      fluidRow(
                        column(width = 2,
                               uiOutput("btn_send_to_langMap")),
                        column(width = 2,
                               uiOutput("btn_clear_reset_rt")),
                      ),
                      hr(),
                      div(class = "dashboard-source_citations",
                          style = "font-size: 10px",
                          attribution_text
                      )
                    ),
                    box(
                      width = 12,
                      title = "Detail Tables",
                      # uiOutput("RT_details", class = "research_table", server = FALSE),
                      uiOutput("RT_details", server = TRUE),
                      # uiOutput("RT_details", class = "research_table", server = TRUE),
                      hr(),
                      div(class = "dashboard-source_citations",
                          style = "font-size: 10px",
                          attribution_text
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      width = 2,
                      downloadButton(outputId = "export_RT",
                                     label = "Export to Excel",
                                     class = "btn_export")
                    ),
                    column(
                      width = 3,
                      tags$span(
                        # style="margin-left: 20px",
                        # style="text-align:center",
                        style="margin-bottom: 0px",
                        a("Provide input to ProgressBible",
                          href = "https://docs.google.com/document/d/1mGchdGTjRhohSC1vBS84JLeHHoVNcRsUagXmq0dSQio/edit",
                                      target = "_blank")
                      )
                    )
                  )

             ),
             tabPanel(
               title = "Language Distributions",
               tabName = "distribution",
               uiOutput("status_fields"),
               plotlyOutput("plot_status_dist"),
               DTOutput("table_status_dist")
             ),

             tabPanel(
                title = "Language Map",
                tabName = "lang_map",
                fluidRow(
                  column(width = 12,
                         leafletOutput("langMap1", height = 600) |>
                           jqui_resizable() |>
                           shinycssloaders::withSpinner(type = 5)
                         ),
                         column(
                           width = 4,
                           fluidRow(
                             downloadButton("download_langMap",
                              label = "Download as HTML")
                           )
                         )
                ),
                fluidRow(
                  box(
                    width = 12,
                    column(
                      uiOutput("status_type"),
                           width = 3),
                    column(uiOutput("selectedLevels", height="100%"),
                           width = 3),
                    column(
                      # tags$div(class="header", checked=NA,
                      #          tags$b("Highligted languages: "),
                      gt_output("highlighted_langs_list"),
                      uiOutput("btn_copy_languages"),
                      # CopyButton(
                      #   "btn_clip_highlighted_languages",
                      #   label = "Copy highlighted languages",
                      #   icon = icon("copy"),
                      #   text = selectedLocations()
                      # ),
                      width = 6
                    )
                      # textOutput("highlighted_langs_list"), height="100%"),
                      #      width = 5)
                  )
                ),

                # reactableOutput("map_table"),
                fluidRow(
                  box(
                    width = 12,
                    title = actionLink("table_options", "Map Table Options", icon = icon("gear")),
                    shinycssloaders::withSpinner(
                      DTOutput("map_table_DT", height = "auto", fill = TRUE),
                      # reactableOutput("map_table"),
                      type = 5),
                    hr(),
                    div(class = "dashboard-source_citations",
                        style = "font-size: 10px",
                        attribution_text
                    )                  ),
                  box(
                    width = 12,
                    title = "Detail Tables",
                    uiOutput("map_table_details"),
                    # uiOutput("map_table_details", class = "research_table"),
                    hr(),
                    div(class = "dashboard-source_citations",
                        style = "font-size: 10px",
                        attribution_text
                    )                  )
                ),
                downloadButton(outputId = "export_map_table",
                               label = "Export to Excel",
                               class = "btn_export")
                ),
             tabPanel(
                title = "Documents",
                tabName = "docs",
                tabBox(width = 12,
                    tabPanel(title = "Mesa Terms of Use and Citation Guidelines",
                             id = "mesa_tou",
                             includeMarkdown("data/assets/mesa_tou.Rmd")),
                    tabPanel(title = "Progress Bible Terms of Use",
                             id = "pb_tou",
                             renderDocument(htmlTemplate("data/assets/progressBibleAgreement_2023-05-04.doc"))),
                    tabPanel(title = "ProgressBible Field Definitions",
                             id = "pb_field_defs",
                             DTOutput("pb_field_defs")),
                    tabPanel(title = "WCD Abbreviations",
                             id = "wcd_abbr",
                             tags$iframe(height="1200", width="1600",
                                         src="Abbreviations.pdf",
                                         name=date())),
                    tabPanel(title = "Release notes",
                             id = "release_notes",
                             pre(includeText("data/assets/release-notes.txt")))
                )
             )
          )
       )
     )
    )


# ****************** Define server logic *************************

server <- function(input, output, session) {

  shinyjs::showLog()

  date_Vision2025 <- "2025-12-31 23:59:59 GMT"

  Vision2025CountDown <- reactive({
    invalidateLater(1000, session)
    TotalDays <- trunc(difftime(date_Vision2025, Nmisc::now_utc(), units='days', tz='GMT+0'))
    HoursToEndOfDay <- trunc(difftime(paste0(as.Date(substr(Nmisc::now_utc(),1,10)) + 1, " 00:00:00 GMT"), Nmisc::now_utc(), units='hours', tz='GMT+0'))
    MinutessToEndOfHour <- (60 - (as.numeric(substring(Nmisc::now_utc(),15,16)) + 1) -1)
    SecondsToEndOfMinute <- 60 - (as.numeric(substring(Nmisc::now_utc(),18,19)) + 1)
    paste0('Countdown to Vision 2025: ',
           TotalDays, ' Days, ',
           HoursToEndOfDay, ' Hours, ',
           MinutessToEndOfHour, ' Minutes, ',
           SecondsToEndOfMinute, ' Seconds')
    # paste0('Countdown to Vision 2025: ',
    #        TotalDays, ' Days, ',
    #        HoursToEndOfDay, ' Hours, ',
    #        MinutessToEndOfHour, ' Minutes, ',
    #        SecondsToEndOfMinute, ' Seconds')
    # paste0(TotalDays, ' Days', br(),
    #        HoursToEndOfDay, ' Hours', br(),
    #        MinutessToEndOfHour, ' Minutes',br(),
    #        SecondsToEndOfMinute, ' Seconds')
  })

  output$txt_V2025TimeRemaining <- renderText({
    Vision2025CountDown()
  })


   cookies_okay <- reactive({
    value <- get_cookie(
      cookie_name = "cookies_consent",
      # missing = "yes")
      missing = "no")
    return(value)
  })

  # observeEvent(get_cookie("cookie_consent"),{
  #   cookies_okay <- get_cookie("cookies_consent")
  # })

  observeEvent(cookies_okay(), {
    if (cookies_okay() != "yes") {
      showModal(modalDialog(
        title = tagList(
          h2("Important Notice: Mesa uses cookies"),
          p("Mesa uses cookies to store user configuration information, which enables Mesa to 'remember'
          the choices made from one session to another. Mesa also makes use of the user's id, along
          with information stored at the time of registration to customize the user's experience and
          record login dates and duration to better understand how Mesa is meeting needs.
          Please contact mesa-support@partnerships.global for more information or assistance."),
          h3("Click OK to consent or Exit."),
          p(em("(Note: Your consent, if given, will be requested again after 90 days.)"))),
        size = "m",
        footer = tagList(
          # modalButton("OK"),
          actionButton("consent_for_cookies", "OK"),
          actionButton("exit_mesa", "Exit")
        )
      ))
    }
  }, ignoreInit = TRUE, once = TRUE)

  observeEvent(input$consent_for_cookies, {
    removeModal()
    set_cookie("cookies_consent",
      cookie_value = "yes")
  })


  observeEvent(input$exit_mesa, {
    stopApp()
  })

  # *** functions scope within server ***

  update_country_selectors <- function(lang_codes) {
    language_codes <- lang_codes

    prior_context <- tibble(
      partner = input$selected_partner |> str_c(collapse = "@@"),
      areas = input$selected_areas |> str_c(collapse = "@@"),
      # countries = country_codes |> str_c(collapse = "@@")
      countries = input$selected_countries |> str_c(collapse = "@@")
    )

    country_codes <- main_rows %>%
      filter(`Language Code` %in% language_codes) %>%
      pull(`Country Code`) %>%
      as.character() %>%
      unique()

    # country_sel_df <- main_rows %>%
    #   filter(`Language Code` %in% language_codes) %>%
    #   select(`Country`, `Country Code`) |>
    #   unique()

    # country_sel_list <- setNames(as.list(country_sel_df$`Country Code`), country_sel_df$`Country`)

    areas <- combined_partner_areas %>%
      filter(Partner == input$selected_partner) %>%
      filter(`Country Code` %in% country_codes) %>%
      # filter(`Country` %in% country_codes) %>%
      pull(Area) %>%
      unique()

    set_cookie("sel_areas",
      cookie_value = areas |> str_c(collapse = "@@")
    )

    set_cookie("sel_countries",
      cookie_value = country_codes |> str_c(collapse = "@@")
    )

    values$prior_context <- prior_context
  }

  # ***** data imports from Google Drive ********

  user <- if (is.null(session$user)) {
  # user <<- if (is.null(session$user)) {
    # "janet.luigjes@wycliffe.nl"
    # "gary_sikma@wycliffe.org"
    # "morris_johnson@wycliffe.org"
    "Guest@partnerships.global"
    # "gail_nelson@wycliffe.org"
    # "aaron_joseph@sil.org"
  } else {
    session$user |> tolower()
  }

  start_date_time <<- Sys.time()

  # *** only for shinyapps.io version ***
  gp_app_users <- get_gp_app_users()
  mesa_users <- get_mesa_users(gp_app_users)
  # user_info <<- get_user_info(mesa_users, user)
  user_info <- get_user_info(mesa_users, user)

  output$user_is_admin <- renderText({
    user_info$admin
  })

  messageData <- reactive({
    df <- shared_coll_rows()
  })

  output$messageMenu <- renderMenu({
    messageData <- messageData()
    if (user != "Guest@partnerships.global" & nrow(messageData > 0)) {
      msgs <- apply(messageData, 1, function(row) {
        item <- messageItem(
          from = row[['Created by']],
          message = paste0("Shared Collection: ", row[['Collection name']]),
          icon = icon("rectangle-list"),
          time = row[['Last modified']],
          href = "#")
        # item$children[[idx]]$attribs$class<-"action-button"
        # item$children[[idx]]$attribs$id<-"md_modify_collection"
        return(item)
      })
    } else {
      msgs <- NULL
    }
    dropdownMenu(type = "messages", .list = msgs)
  })

  # observeEvent(input$md_modify_collection, {
  #   showModal(manage_collections_modal() , session)
  # })

  notificationData <- reactive({
    df <- tibble(
      text = c(glue_safe('Data last updated: {max_date_accessed}'),
               glue_safe('ProgressBible Snapshot: {max_snapshot_date}')),
      icon = c("calendar-check",
               "camera"),
      status = c("success",
                 "success")
      # icon = c("calendar-check",
      #          "square-plus"),
      # status = c("success",
      #            "success")
    )
  })

  output$notificationMenu <- renderMenu({
    data <- notificationData()
    msgs <- apply(data, 1, function(row) {
      notificationItem(text = row[["text"]],
                       icon = icon(row[["icon"]]),
                       status = row[["status"]])
    })
    dropdownMenu(type = "notification",
                 headerText = "News & Announcements",
                 icon = icon("bullhorn"),
                 .list = msgs)
  })

  # rm(list = c("gp_app_users", "mesa_users"))

  # ***** data imports from file system ********

  # ********* assets upload **************

  assets_path = "data/assets/" # for publication
  # assets_path = "R/data/assets/" # used for dev only

  # app resources stored R/data/assets
  # pb_tou_doc <- htmltools::htmlTemplate(paste0(assets_path, "progressBibleAgreement_2023-05-04.doc"))

  timezones_sf <- geojsonio::geojson_read(paste0(assets_path, "time_zones.geojson"),
                                          parse = TRUE,
                                          what = "sp",
                                          stringsAsFactors = TRUE)

  ## Strategic Languages

  SLI_Polygons <- geojsonio::geojson_read(paste0(assets_path, "SLI_Polygonsv0.2.geojson"),
                                          parse = TRUE,
                                          what = "sp",
                                          stringsAsFactors = TRUE)

  SLI_List <- read.csv(paste0(assets_path, "SLI_LIstv0.2.csv"), header = TRUE) |>
    mutate(`Resource Level` = factor(Resource.Level)) |>
    rename(`Language Code` = Iso.Code) |>
    select(-`Language.Name`)

  resource_lang_palette <- colorFactor(palette = c("#f7f5f5", "#e41a1c", "#377eb8", "#4daf4a", "#984ea3"),
                                       # domain = levels(SLI_List$`Resource Level`))
                                       domain = levels(SLI_Polygons$Resource.Level))


  country_centroids <- read.csv(paste0(assets_path, "country_centroids_az8.csv"), header = TRUE) |>
    select(admin, iso_a2, Latitude, Longitude) |>
    rename(Country = admin, `Country Code` = iso_a2, cntry_lat = Latitude, cntry_lon = Longitude) |>
    filter(!Country %in% c("Ashmore and Cartier Islands",
                           "Indian Ocean Territories",
                           "Kosovo",
                           "Northern Cyprus",
                           "Siachen Glacier",
                           "Somaliland"))

# print(country_centroids$`Country`)
# print(country_centroids$`Country Code`)

  combined_partner_areas <<- read_sheet(
    as_sheets_id("1Ye6o6UDCBrvGqLsj1UANsN4EaQANggKtmi56BQTjo8Y"),
    range = "Areas!A:I") |>
  # combined_partner_areas <<- readr::read_csv("data/assets/CombinedAreaTable.csv",
  #                                            show_col_types = FALSE) |>
    filter(Partner != "UN", Partner != "Ethnologue") |>
    mutate(`Country Code` = `Country Code` |> str_replace_na()) |>  #needed for Namibia (NA)
    select(-Area2, -Area3)

  fips_lookup <- combined_partner_areas |>
    filter(Partner == "Global Partnerships") |>
    select(`Country Code`, `FIPS Code`)

  country_code_lookup <- combined_partner_areas |>
    filter(Partner == "Global Partnerships") |>
    select(`Country Code`, `Country`)

  ## aPG People Groups

  # print("Loading people group data")

  # aPG_polygons <- geojsonio::geojson_read(paste0(assets_path, "aPG_(People_Group_areas).geojson"),
  #                                         parse = TRUE,
  #                                         what = "sp",
  #                                         stringsAsFactors = TRUE)

  # aPG_list <- read.csv(paste0(assets_path, "aPG_(People_Group_areas).csv"), header = TRUE) |>
  #   mutate(`Language Code` = ROL,
  #          `Group Name` = Name,
  #          `Alternate Grp Name(s)` = NmAlt,
  #          `FIPS Code` = ROG) |>
  #   mutate(`Scripture Lvl` = case_when(
  #     LvlBible == 0 ~ "0 - Not Available",
  #     LvlBible == 1 ~ "1 - Bible",
  #     LvlBible == 2 ~ "2 - New Testament",
  #     LvlBible == 3 ~ "3 - Selections",
  #     LvlBible == 4 ~ "4 - Stories")) |>
  #   mutate(`FIPS Code` = str_replace(`FIPS Code`, "WE", "PS"), # West Bank > Palestinian Territories
  #          `FIPS Code` = str_replace(`FIPS Code`, "GZ", "PS"), # Gaza Strip > Palestinian Territories
  #          `FIPS Code` = str_replace(`FIPS Code`, "SV", "NO"), # Svalbard > Norway
  #          # , `FIPS Code` = str_replace(`FIPS Code`, "KV", "XK")
  #          )
  #
  # aPG_list2 <- aPG_list |>
  #   left_join(fips_lookup)



  # groups_palette <- colorFactor(palette = c("#f7f5f5", "#e41a1c", "#377eb8", "#4daf4a", "#984ea3"),
  #                                      # domain = levels(SLI_List$`Resource Level`))
  #                                      domain = levels(aPG_polygons$LvlBible))

  # print("done")

  ## misc


  pb_field_definitions <- read.csv(paste0(assets_path, "PB_Fields_Definitions.csv"), header = TRUE, skip = 1) %>%
    select(1:2) %>%
    rename(`Field Name` = `Data.Element.Name`) %>%
    filter(`Field Name` != "[report menu group]", !is.na(Definition))

  missing_SL_coords <- read.csv("data/assets/missing_SL_coords.csv") %>%
    select(`Language.Code`, Latitude, Longitude) |>
    rename(`Language Code` = Language.Code)


  field_lists <- get_field_lists()

  field_hierarchy <- field_lists$sources
  dashboard_fields <- field_lists$dashboards

  # print("Loading v2025_history_df feather file")
  # v2025_history_df <- arrow::read_feather("data/assets/v2025_history.feather")
  # print("done")

  # dsi_ss <- "https://docs.google.com/spreadsheets/d/1xRg2j2TAIWO561Ahrc9kKDGiNoGn3A8HATGoocvP2Wk/edit#gid=827905161"
  # dsi_sheet <- "import_dsi_languages"
  # dsi_langs <- read_sheet(dsi_ss, dsi_sheet)

  ##################################################################################
  # load cached data files
  #################################################################################

  # cache_file_list <- list.files("R/data/cache/") |>
  # cache_file_list <- list.files("data/cache/") |>
  #   map_vec(\(x) paste0("data/cache/", x))
  #   # map_vec(\(x) paste0("R/data/cache/", x))
  #
  # data_list <- cache_file_list %>%
  #   map_dfr(\(fname) tibble(
  #     file_name = str_extract(fname, "(?<=cache/).+(?=\\.feather)"),
  #     data = list(read_feather(fname)))
  #     )

  # get_df <- function(fname){
  #   df <- data_list %>%
  #     filter(file_name == fname) %>%
  #     unnest(cols = c(data)) %>%
  #     select(-file_name)
  #   return(df)
  # }

  mesa_sql_con <- DBI::dbConnect(RSQLite::SQLite(), "data/sql_db/mesa.sqlite")
  tbl_list <- DBI::dbListTables(mesa_sql_con)

  data_list <- tbl_list %>%
    map_dfr(\(name) tibble(
      fname = name,
      data = list(tbl(mesa_sql_con, name) |> collect()))
    )

  DBI::dbDisconnect(mesa_sql_con)

  # print(data_list)

  get_df_feather <- function(name){
    df <- data_list %>%
      filter(fname == name) %>%
      unnest(cols = c(data)) %>%
      select(-fname)
    return(df)
  }

  # get_all_snapshot_dates <- function(ds_name){
  #   ds_name <- "pb_main_ds"
  #   source <- paste0("data/datasets/", ds_name)
  #   ds <- arrow::open_dataset(source, format = "parquet")
  #   # out <- ds$SnapshotDate
  #   df <- ds |>
  #     dplyr::collect()
  #   out <- df$SnapshotDate |> unique() |> sort()
  #   return(out)
  # }
  #
  # get_aa_snapshot_dates <- function(ds_name){
  #   ds_name <- "pb_main_ds"
  #   source <- paste0("data/datasets/", ds_name)
  #   ds <- arrow::open_dataset(source, format = "parquet")
  #   # out <- ds$SnapshotDate
  #   df <- ds |>
  #     dplyr::collect()
  #   out <- df |>
  #     filter(!is.na(`All Access Status`)) |>
  #     pull(SnapshotDate) |> unique() |> sort()
  #   return(out)
  # }

  get_all_snapshot_dates <- function(ds_name) {
    print("@ get_all_snapshot_dates")
    ds_name <- "pb_main_ds"
    source <- paste0("data/datasets/", ds_name)
    print(paste0("ds source = ", source))

    ds <- arrow::open_dataset(source, format = "parquet")

    dates <- ds |>
      dplyr::select(SnapshotDate) |>  # Select only the column we need
      dplyr::distinct() |>            # Get unique values
      dplyr::arrange(SnapshotDate) |> # Sort
      dplyr::collect()                # Now collect only unique dates

    print(paste0("count of all snapshot dates: ", nrow(dates)))
    return(dates$SnapshotDate)
  }

  get_aa_snapshot_dates <- function(ds_name) {
    print("@ get_aa_snapshot_dates")
    ds_name <- "pb_main_ds"
    source <- paste0("data/datasets/", ds_name)
    print(paste0("ds source = ", source))

    ds <- arrow::open_dataset(source, format = "parquet")

    # Option 1: Use Arrow compute operations
    dates <- ds |>
      dplyr::filter(!is.na(`All Access Status`)) |>
      dplyr::select(SnapshotDate) |>  # Select only the column we need
      dplyr::distinct() |>            # Get unique values
      dplyr::arrange(SnapshotDate) |> # Sort
      dplyr::collect()                # Now collect only unique dates

    print(paste0("count of aa snapshot dates: ", nrow(dates)))
    return(dates$SnapshotDate)
  }

  snapshot_dates_list <- get_all_snapshot_dates()

  aa_snapshot_dates_list <- get_aa_snapshot_dates()

  # v2025_history_df <- get_df_feather("v2025_history_data")
  # aa_history_df <- get_df_feather("all_access_history_data")

  filter_SnapshotDate_from <- function(year=NULL, month=NULL) {
    if (is.null(year)) {
      year = year(today()) |> as.character()
    }
    if (is.null(month)) {
      month = (month(today() - lubridate::dmonths(1))) |> as.character()
    }
    paste0(year, "-", month |> str_pad(2, pad = "0"), "-01")
  }

  get_df_dataset <- function(ds_name){
    print("Loading 'pb_main_ds' dataset")
    ds_name <- "pb_main_ds"
    # snapshot_date <- filter_SnapshotDate_from()
    source <- paste0("data/datasets/", ds_name)
    max_date <- get_all_snapshot_dates() |> max()
    print(paste0("max_date = ", max_date))
    ds <- arrow::open_dataset(source, format = "parquet")
    df <- ds |>
      dplyr::filter(`SnapshotDate` == max_date) |> # most recent snapshot only |>
      dplyr::collect()
    print("dataset loaded")
    return(df)
  }

  main_rows <- get_df_dataset("pb_main_ds")

  max_snapshot_date <- main_rows$`SnapshotDate` |> max()

  # get_df_dataset <- function(ds_name){
  #   ds_name <- "pb_main_ds"
  #   snapshot_date <- filter_SnapshotDate_from()
  #   source <- paste0("data/datasets/", ds_name)
  #   ds <- arrow::open_dataset(source, format = "parquet")
  #   df <- ds |>
  #     dplyr::collect()
  #   return(df)
  # }
  #
  # main_rows <- get_df_dataset("pb_main_ds") |>
  #   filter(`SnapshotDate` == max(`SnapshotDate`)) # most recent snaphot only


  # main_rows <- main_rows |>
  #   filter(`SnapshotDate` == max(`SnapshotDate`)) # most recent snaphot only

  # main_rows <- main_rows_all |>
  #   filter(`SnapshotDate` == filter_SnapshotDate_from())
  #
  # rm(main_rows_all)

  # observe({
  #   main_rows <- main_rows_all |>
  #     filter(`SnapshotDate` == filter_SnapshotDate_from())
  # }) |> bindEvent(input$selected_snapshot)

  # print("misc transformation to main_rows")
  main_rows <- main_rows |>
    mutate(`Is Sign Language` = factor(`Is Sign Language`, levels = c("Yes", "No"))) %>%
    mutate(`Is Remaining V2025 Need` = factor(`Is Remaining V2025 Need`, levels = c("Yes", "No"))) %>%
    mutate(`On All Access List` = factor(`On All Access List`, levels = c("Yes", "No"))) %>%
    # mutate(`In The Circle` = factor(`In The Circle`, levels = c("Yes", "No"))) %>%
    mutate(`Translation Status` = as.factor(str_replace_na(.$`Translation Status`, replacement = "Not available"))) %>%
    mutate(`All Access Goal` = as.factor(str_replace_na(.$`All Access Goal`, replacement = "Not available"))) %>%
    left_join(missing_SL_coords) |>
    mutate(`AAG chapters` = ifelse(`All Access Goal` == '25 Chapters',25,
                                   ifelse(`All Access Goal` == 'NT/260 chapters',260,
                                          ifelse(`All Access Goal` == 'Bible',1189,
                                                 ifelse(`All Access Goal` == 'Two Bibles',2378,0))))) |>
    # mutate(`DSI Eligibility` = if_else(`Language Code` %in% dsi_langs$`Language Code` &
    #                                    `Country Code` %in% dsi_langs$`Country Code`, 'Yes', 'No') |>
    #          as_factor()) |>
    mutate(`AAG chapters remaining` = if_else(is.na(`All Access Status`) | str_detect(`All Access Status`, "Goal Met"),
                                              0, `AAG chapters`))
  # print("done")

  main_rows <- main_rows |>
    rename(
      POSIXct_date = `P O S I Xct_date`,
      `Active OBT` = `Active O B T`,
      `Prior AA Status` = `Prior A A Status`,
      `Has OBT Engagements` = `Has O B T Engagements`,
      `IllumiNations Region` = `Illumi Nations Region`,
      `IllumiNations Group Name` = `Illumi Nations Group Name`
    )

  #################################################################################
  # 2024 04 30 Morris Johnson
  # Convert all character variables to factor to enable dropdown column filters.
  # For variable that need ordered levels, use factor() with the levels specified,
  # e.g., see above.
  #################################################################################
  main_rows <- main_rows |>
    mutate(across(where(is_character), as_factor))

  gp_areas <- combined_partner_areas %>%
    filter(Partner == "Global Partnerships") |>
    select(Area, Country, `Country Code`)

  # print("left joining main_rows and gp_areas")
  main_rows <- main_rows |>
    left_join(gp_areas)
  # print("done")

  WIP_rows_all <- get_df_feather("WIP_data") %>%
    mutate(`Dialect Code` = as.character(`Dialect Code`))

  WIP_rows <- get_WIP_rows(WIP_rows_all, user_info)

  WIP_summaries <- get_summary_field(WIP_rows, `Organization Name`, `Engagement Status`, `Org Engagements (WIP)`)

  main_rows <- main_rows |>
    left_join(WIP_summaries, by = "Language Code")

  # WIP_org_names <- WIP_rows |>
  #   pull(`Organization Name`) |>
  #   unique()

  alt_names_rows <- get_df_feather("alternate_names") |>
    select(-etl_timestamp)

  SP_rows <- get_df_feather("scripture_products") |>
    select(-etl_timestamp)

  SP_summaries <- SP_rows %>%
    group_by(`Language Code`) %>%
    reframe(row_count = n(),
            year_pub_max = max(`Year Published`),
            year_pub_min = min(`Year Published`)) %>%
    mutate(year_pub_max = str_replace_na(.$year_pub_max, replacement = "Unk")) %>%
    mutate(year_pub_min = str_replace_na(.$year_pub_min, replacement = "Unk")) %>%
    mutate(`Scripture Product Summary` = paste0("All Titles: ",
                                                row_count,
                                                "<br>Pub Year(s): ",
                                                year_pub_min, " to ", year_pub_max,
                                                "<br><a href='#' id=",
                                                `Language Code`,
                                                " onclick='setinput(this.id);'>",
                                                "<b>View title(s)</b></a>")) %>%
    select(-row_count, -year_pub_max, -year_pub_min)

  main_rows <- main_rows |>
    left_join(SP_summaries, by = "Language Code")

  WCD_rows <- get_df_feather("wcd_report") %>%
    select(-`Ethnic Region`) |>
    select(-etl_timestamp)

  ROLV_rows <- get_df_feather("rolv_varieties") |>
    select(-etl_timestamp)

  rm(data_list)

  # *** load user configs ***

  # user_config <- read_user_config(user)

  # *** additional transformations ***

  archive_link_prefix <- "<a href='https://www.sil.org/resources/search/language/"
  archive_link_infix <- "' target='_blank'>"
  archive_link_suffix <- "</a>"

  fips_lookup <- combined_partner_areas |>
    filter(Partner == "Global Partnerships") |>
    select(`Country Code`, `FIPS Code`)

  main_rows <- main_rows %>%
    left_join(fips_lookup, join_by(`Country Code`)) |>
    mutate(
      `Search SIL Archive` = paste0(
        archive_link_prefix,
        `Language Code`,
        archive_link_infix,
        `Language Name`,
        archive_link_suffix),
      `joshua_proj_country` = paste0(
        "<a href='https://joshuaproject.net/countries/",
        `FIPS Code`,
        archive_link_infix,
        `Country`,
        archive_link_suffix),
      `See Joshua Project` = paste0(
        "<a href='https://joshuaproject.net/languages/",
        `Language Code`,
        archive_link_infix,
        `Language Name`,
        archive_link_suffix)
    ) %>%
    left_join(SLI_List, by = c("Language Code"))

  rm(SLI_List)

  maps_table <- main_rows %>%
    select(Country, `Country Code`, `Language Name`, `Language Code`,
           `Is Sign Language`, `Translation Status`, `All Access Status`, `Is Remaining V2025 Need`, `On All Access List`,
           `EGIDS Group`, `Org Engagements (WIP)`,`Denominations (WCD)`, `Agencies (WCD)`,  `See Joshua Project`,
           Latitude, Longitude, rowID) %>%
    mutate(html_popup_text =   paste("<div style=width: 20%; display: inline-block; wordwrap: break-word; >",
                                     "<h5>",`Language Name`,"</h5>",
                                     "<ul>",
                                     "<li><strong>Translation Status: </strong>",`Translation Status`,"</li>",
                                     "<li><strong>All Access Status: </strong>",`All Access Status`,"</li>",
                                     "<li><strong>EGIDS Group: </strong>", `EGIDS Group`, "</li>",
                                     "<li><strong>Org Engagements: </strong>", `Org Engagements (WIP)`, "</li>",
                                     "<li><strong>Denominations: </strong>", `Denominations (WCD)`, "</li>",
                                     "<li><strong>Agencies: </strong>", `Agencies (WCD)`, "</li>",
                                     "<li><strong>See Joshua Project: </strong>", `See Joshua Project`, "</li>",
                                     "</ul>",
                                     "</div>")) %>%
    mutate(html_label_static =   paste("<div style=width: 20%; display: inline-block; wordwrap: break-word; color:blue; font-size:12>",
                                       `Language Name`,
                                       "</div>")) %>%
    # filter(!is.na(`Language Name`)) %>%
    left_join(country_centroids, by = "Country Code") %>%
    # left_join(country_centroids, by = "Country Code") %>%
    # select(-Country.y) %>%
    # rename(Country = Country.x) %>%
    mutate(cntry_lon = as.numeric(cntry_lon)) %>%
    mutate(cntry_lat = as.numeric(cntry_lat)) %>%
    mutate(Longitude = case_when(is.na(Longitude) ~ cntry_lon, TRUE ~ Longitude)) %>%
    mutate(Latitude = case_when(is.na(Latitude) ~ cntry_lat, TRUE ~ Latitude))
    # mutate(Longitude = case_when(Longitude == "" ~ cntry_lon, TRUE ~ Longitude)) %>%
    # mutate(Latitude = case_when(Latitude == "" ~ cntry_lat, TRUE ~ Latitude))

  # rm(country_centroids)

  maps_table$`All Access Status` <- maps_table$`All Access Status` %>%
    str_replace_all(c("Goal Met - Scripture accessed via second language" = "Goal Met through L2",
                      "Goal Met in the language" = "Goal Met through L1")) %>% as.factor()

  # write.csv(maps_table, "~/data_projects/mesa_temp/maps_table.csv")

  status_var_names <- maps_table %>%
    select(`Translation Status`, `All Access Status`, 'Is Remaining V2025 Need', 'On All Access List') %>%
    names()

  status_field_list <- field_hierarchy %>%
      filter(Source %in% main_sources) %>%
      filter(Topic == "Status") %>%
      filter(field_name != "Inherited") %>%
      pull(field_name) %>%
      unique()

  status_vars2 <- main_rows %>%
    select(Country, `Country Code`, `Language Name`, `Language Code`, all_of(status_field_list))

  # *** server scoped <- (i.e., reactive variables) ***

  default_partner <- "Global Partnerships"
  default_areas <- "Pacific"
  default_countries <- "PG"

  default_taxonomy <- "Topic"
  default_topic <- "Status"
  default_fields <- "Translation Status"

  values <- reactiveValues(
      # initial values
      stored_fields_table = NULL,

      db_tbl_fld_choices = NULL,
      db_tbl_chosen_flds = NULL,
      db_langs_tbl_data = NULL,
      V2025_summary_tbl_fld_choices = NULL,
      V2025_summary_tbl_chosen_flds = NULL,
      V2025_summary_tbl_data = NULL,
      all_access_summary_tbl_fld_choices = NULL,
      all_access_summary_tbl_chosen_flds = NULL,
      all_access_summary_tbl_data = NULL,
      # vb_click_filter = "Vision 2025",
      vb_click_filter = "none",
      # default_selected_dashboard = get_cookie("sel_db_view", missing = "Vision 2025"),
      val_plot_selected_only = TRUE,
      val_list_highlighted_only = TRUE,
      val_collections_load = FALSE,
      val_shares_load = FALSE,
      prior_context = NULL,
      val_codes = "",
      validation_message = "",
      # validation_message = NULL,
      val_msg_share_opt = NULL,
      collections = read_collections(),
      val_RT_rows_selected = NULL,
      sel_summary_panel = NULL,
      curr_db_plot_df = NULL,
      hist_langs = NULL,
      val_db_search = FALSE,
      val_hist_search = FALSE,
      val_iso_search = FALSE
  )



  shared_coll_rows <- reactive({
    req(!is.null(values$collections))
    values$collections |>
      filter(user_id == user |
               sharing_option == "all" |
               shares %in% user) |>
        select(date, name, description, languages, user_full_name) |>
        rename(`Last modified` = date,
               `Collection name` = name,
               Description = description,
               Languages = languages,
               `Created by` = user_full_name)
  })

      # *** render and reactive functions for sidebar: icon, partners, areas and country filters ***


  output$pb_field_defs <- renderDT({
    datatable(pb_field_definitions,
              options = list(scrollY = '60vh',
                             scrollcollapse = FALSE,
                             dom = 'lfrtip',
                             lengthMenu = list(c(-1, 10, 25, 50), c("All", 10, 25, 50))
              )
    )
  })

  output$user_greeting <- renderText({
    paste0("Welcome, ", user_info$first, "!")
  })

  # output$snapshotdate <- renderUI({
  #   selectInput(inputId = "selected_snapshot",
  #               label = "Select Snapshot Date",
  #               choices = main_rows_all$SnapshotDate,
  #               selected = filter_SnapshotDate_from(),
  #               multiple = FALSE,
  #               width = NULL)
  # })

  output$partners <- renderUI({
    selectInput(inputId = "selected_partner",
                label = "Select Area choices, by org",
                choices = unique(combined_partner_areas$Partner),
                selected = get_partner(),
                multiple = FALSE,
                width = NULL)
  })

  get_partner <- reactive({
    get_cookie(
      cookie_name = "sel_partner",
      missing = "Global Partnerships",
      # missing = NULL,
      session = shiny::getDefaultReactiveDomain()
    )
  })

  observeEvent(input$selected_partner,
    {
      # print("observeEvent(input$selected_partners")
      curr_val <- input$selected_partner
      stored_val <- get_cookie("sel_partner", missing = "Global Partnerships")
      if (curr_val != stored_val) {
        set_cookie("sel_partner", cookie_value = input$selected_partner)
      }

      selected <- get_cookie(
        cookie_name = "sel_areas",
        missing = "Pacific",
        session = shiny::getDefaultReactiveDomain()
      ) |> str_split_1("@@")

      updateSelectInput(
        inputId = "selected_areas",
        choices = area_choices(),
        selected = selected
        # selected = get_cookie(
        #   cookie_name = "sel_areas",
        #   missing = "Pacific",
        #   session = shiny::getDefaultReactiveDomain()
        # ) |> str_split_1("@@")
      )
    })

  area_choices <- reactive({
    # req(input$selected_partner)
    #  print("@area_choices()")
    areas <- combined_partner_areas %>%
        filter(Partner %in% input$selected_partner) %>%
        pull(Area) %>%
        unique()
    return(areas)
  })

  output$areas <- renderUI({
    selectInput(inputId = "selected_areas",
                label = "Select Area(s)",
                choices = area_choices(),
                selected = isolate(get_areas()),
                # selected = get_areas(),
                multiple = TRUE,
                selectize = FALSE,
                size = 8)
  })

  get_areas <- reactive({
    #  print("@get_areas()")
    out <- get_cookie(
      cookie_name = "sel_areas",
      missing = "Pacific",
      session = shiny::getDefaultReactiveDomain()
    ) |> str_split_1("@@")
    return(out)
  })

#   get_areas <- reactive({
#     curr_areas <- input$selected_areas
# print(paste0("curr_areas is null: ", is.null(curr_areas)))
#     if (!is.null(curr_areas)) {
#       out <- get_cookie(
#         cookie_name = "sel_areas",
#         missing = "Pacific",
#         # missing = NULL,
#         session = shiny::getDefaultReactiveDomain()
#       ) |> str_split_1("@@")
# print(paste0("out = ", out))
#       return(out)
#     } else {
#       return(curr_areas)
#     }
#   })

  get_countries <- reactive({
#  print("@get_countries")
    selected = get_cookie(
      cookie_name = "sel_countries",
      # missing = character(0),
      # missing = NULL,
      missing = "PG",
      session = shiny::getDefaultReactiveDomain()
    ) |> str_split_1("@@")
    return(selected)
  })

  observeEvent(input$selected_areas,
    {
#  print("@observeEvent(input$selected_areas")
      value <- input$selected_areas |> str_c(collapse = "@@")
      set_cookie("sel_areas",
        cookie_value = value
      )

      choices = country_choices()
      selected <- get_countries()

      updateSelectInput(
        session = session,
        inputId = "selected_countries",
        choices = choices,
        selected = selected
        # selected = get_cookie(
        #   cookie_name = "sel_countries",
        #   missing = character(0),
        #   # missing = NULL,
        #   # missing = "Papua New Guinea",
        #   session = shiny::getDefaultReactiveDomain()
        # ) |> str_split_1("@@")
      )
    }
  )

  output$link_select_all_areas <- renderUI({
    actionLink("select_all_areas", "Select/unselect all")
  })

  observe({ #bound to select_all_areas action link
    if(input$select_all_areas %% 2) {
      updateSelectInput(session, "selected_areas", selected = area_choices())
    } else {
      updateSelectInput(session, "selected_areas", selected = "")
    }
  }) %>% bindEvent(input$select_all_areas)

  # areas_selected <- reactive({
  country_choices <- reactive({
#  print("@country_choices()")
    df <- combined_partner_areas %>%
      filter(Partner %in% input$selected_partner,
             Area %in% input$selected_areas) %>%
      select(Area, Country, `Country Code`)

    choice_list <- map(df$`Country Code`, list)
    names(choice_list) <- df$`Country`
    choice_list <- choice_list %>% purrr::flatten()

    return(choice_list)
  })

  output$countries <- renderUI({
    choices <- country_choices()
    selected <- isolate(get_countries())
    # selected <- get_countries()

    selectInput(inputId = "selected_countries",
                label = "Select Country(s)",
                choices = choices,
                selected = selected,
                multiple = TRUE,
                selectize = FALSE,
                size = 10)
  })

  # get_countries <- reactive({
  #   get_cookie(
  #     cookie_name = "sel_countries",
  #     missing = "Vanuatu",
  #     # missing = NULL,
  #     session = shiny::getDefaultReactiveDomain()
  #   ) |> str_split_1("@@")
  # })

  observeEvent(input$selected_countries,
    {
      # req(input$selected_countries)
      value <- input$selected_countries |>
        str_c(collapse = "@@")
      set_cookie("sel_countries",
        cookie_value = value)
      out <- get_cookie("sel_countries")

      # curr_countries <- input$selected_countries |>
      #   str_c(collapse = "@@")
      # stored_countries <- get_cookie("sel_countries", missing = "")
      # if(curr_countries != stored_countries){
      #   set_cookie("sel_countries", cookie_value = curr_countries)
      # }
    })

  output$link_select_all_countries <- renderUI({
    actionLink("select_all_countries", "Select/unselect all")
  })

  observeEvent(input$select_all_countries,
    {
      if(input$select_all_countries %% 2) {
        updateSelectInput(session, "selected_countries", selected = country_choices())
      } else {
        updateSelectInput(session, "selected_countries", selected = "")
      }
    })

  countries_selected <- reactive({
    # req(input$selected_countries)

    countries <- input$selected_countries

    selected_countries <- combined_partner_areas %>%
        filter(`Country Code` %in% countries)

    return(selected_countries)
  })

  # *** Collections loading and managing ***

  output$manage_collections <- renderUI({
    actionLink("lnk_manage_collections",
      label = "Load/Manage Saved Collections",
      icon = icon("cloud-arrow-down"))
  })

  observeEvent(input$lnk_manage_collections, {
    updateTabsetPanel(session, "main_page_tabset", selected = "table_builder")
    showModal(manage_collections_modal())
  })

  my_collections_df <- reactive({
    df <- values$collections |>
      filter(user_id == user) |>
      select(date, name, description, languages, shares) |>
      rename(
        `Last Modified` = date,
        `Collection Name` = name,
        `Description` = description,
        `Languages` = languages,
        `Shared with` = shares
      )
    return(df)
  })

  my_shares_df <- reactive({
    df = values$collections |>
      filter(str_detect(shares, user)) |>
      select(date, name, description, languages, user_full_name) |>
      rename(
        `Last Modified` = date,
        `Collection Name` = name,
        `Description` = description,
        `Languages` = languages,
        `Created by` = user_full_name
      )
    return(df)
  })

  output$my_collections <- renderReactable({
    table <- my_collections_df()
    reactable(
      data = table,
      compact = TRUE,
      selection = "multiple",
      filterable = TRUE
    )
  })

  output$my_shares <- renderReactable({
    table = my_shares_df()
    reactable(
      data = table,
      compact = TRUE,
      selection = "multiple",
      filterable = TRUE
    )
  })

  manage_collections_modal <- function() {
    # stored_collections <- values$collections
    md <- modalDialog(
      box(
        width = 12,
        height = "auto",
        title = "My saved collections",
        shinycssloaders::withSpinner(
          reactableOutput("my_collections"),
        ),
        fluidRow(
          column(
            width = 3,
            actionButton("btn_load_my_collections",
                         label = "Load selected collection(s)",
                         icon = icon("book-open-reader")),
          ),
          column(
            width = 3,
            actionButton("btn_modify_collection",
                         label = "Modify selected collection",
                         icon = icon("pen-to-square")),
            textOutput("val_message_modify")
          ),
          column(
            width = 3,
            actionButton("btn_delete_collections",
                         label = "Delete selected collection(s)",
                         icon = icon("trash"))
          )
        )
      ),
      box(
        width = 12,
        height = "auto",
        title = "Collections shared with me",
        footer = "Select collection(s) to include in the Research Table",
        reactableOutput("my_shares"),
        fluidRow(
          column(
            width = 3,
            actionButton("btn_load_my_shares",
                         label = "Load selected collection(s)",
                         icon = icon("book-open-reader")),
          )
        )
      ),
      title = "Load and Manage Collections",
      footer = modalButton("Done"),
      size = "l",
      # size = c("m", "s", "l"),
      easyClose = TRUE,
      fade = TRUE
    )
    md$children[[1]]$attribs$class<-"action-button"
    md$children[[1]]$attribs$id<-"md_modify_collection"

    md
  }

  observeEvent(input$btn_load_my_collections, {
    values$val_iso_search <- FALSE
    values$val_collections_load <- TRUE
    values$val_shares_load <- FALSE
    # iso_codes <- lang_codes()
    indices <- getReactableState(
      outputId = "my_collections",
      name = "selected",
      session = session)
    # iso_codes <- if (length(indices > 0)) {
    iso_codes <- if (!is.null(indices)) {
      out <- my_collections_df() |>
        slice(indices)|>
        pull(Languages) |>
        str_c(collapse = "; ") |>
        str_extract_all("(?<=\\[).{3,5}(?=\\])") |>
        unlist()}
    values$val_collection_iso_codes <- iso_codes

    update_country_selectors(iso_codes)
  })

  observeEvent(input$btn_load_my_shares, {
    values$val_iso_search <- FALSE
    values$val_collections_load <- FALSE
    values$val_shares_load <- TRUE
    # iso_codes <- lang_codes()
    indices <- getReactableState(
      outputId = "my_shares",
      name = "selected",
      session = session)
    # iso_codes <- if (length(indices > 0)) {
    iso_codes <- if (!is.null(indices)) {
      out <- my_shares_df() |>
        slice(indices)|>
        pull(Languages) |>
        str_c(collapse = "; ") |>
        str_extract_all("(?<=\\[).{3,5}(?=\\])") |>
        unlist()}
    values$val_collection_iso_codes <- iso_codes

    update_country_selectors(iso_codes)
  })


  selected_row_indices <- reactive({
    selected_row_indices <- getReactableState(
      outputId = "my_collections",
      name = "selected",
      session = session
    )
  })

  observeEvent(input$btn_modify_collection, {
    values$val_iso_search <- FALSE
    values$val_collections_load <- TRUE
    values$val_shares_load <- FALSE

    curr_indices <- selected_row_indices()

    if (length(curr_indices) > 1) {
      msg_mod_opt <- 'Please select only one collection to modify.'
    } else if(length(curr_indices) == 0) {
      msg_mod_opt <- "Please select one collection to modify"
    } else {
      msg_mod_opt <- ""
      selected_name <- my_collections_df() |>
        slice(curr_indices) |>
        pull(`Collection Name`)
    }

    output$val_message_modify <- renderText({msg_mod_opt})

    if (exists("selected_name")) {
      collection_to_modify <- values$collections |>
        filter(user_id == user) |>
        filter(name == selected_name)

      removeModal(session = session)
      showModal(save_collection_modal(), session = session)

      updateTextInput(
        session = session,
        inputId = "collection_name",
        label = NULL,
        value = collection_to_modify$name)

      updateTextAreaInput(
        session = session,
        inputId = "description",
        label = NULL,
        value = collection_to_modify$description)

      iso_codes <- collection_to_modify$languages |>
        str_extract_all("(?<=\\[).{3,5}(?=\\])") |>
        unlist()

      update_country_selectors(iso_codes)

      table_df <- main_rows |>
        filter(`Language Code` %in% iso_codes) |>
        select(Country, Subdivision, `Language Name`)

      updateReactable(
        session = session,
        outputId = "collection_langs_list",
        data = table_df
      )

      # values$val_collection_iso_codes <- iso_codes

      updateRadioButtons(
        session = session,
        inputId = "share_opt",
        selected = collection_to_modify$sharing_option)

      share_ids <- collection_to_modify$shares |>
        str_c(collapse = "; ") |>
        str_split_1("; ") |>
        stringi::stri_remove_empty()

      selected_indices <- match(share_ids, collection_users()$Email)

      updateReactable(
        session,
        outputId = "sharing_list",
        data = collection_users(),
        selected = selected_indices,
        expanded = FALSE,
        page = NULL,
        meta = NULL
        # selected = {
        #   user_ids <- collection_to_modify$shares |> str_split_1("; ")
        #   selected_indices <- match(user_ids, collection_users()$Email)
        # }
      )

      values$val_collections_load <- FALSE
    }
  })

  observeEvent(input$btn_delete_collections, {
    selected_row_indices <- getReactableState(
      outputId = "my_collections",
      name = "selected",
      session = session
    )
    collections_to_delete <- my_collections_df() |>
      slice(selected_row_indices) |>
      pull(`Collection Name`)

    values$collections <- values$collections |>
      filter(!(name %in% collections_to_delete))

    write_collections(values$collections)
  })



  # *** reactive functions for creating data tables ***

  # RT_column_filters <- reactive({
  #   filters <- input$research_table_search_columns |>
  #     str_extract("(?<=\").+(?=\")") |>
  #     str_replace_na("")
  #   filter_string <- filters |>
  #     stringr::str_c(collapse = ";")
  #   RT_current_fields <- c("Country", "Language Name", "Language Code", values$stored_fields)
  #   RT_current_filters_df <- tibble(
  #     field = RT_current_fields,
  #     value = filters
  #   )
  # })
  #
  # observeEvent(input$research_table_search_columns,{
  #   col_filters <- RT_column_filters()
  #   set_cookie("RT_column_filters", col_filters)
  # })

  lang_codes <- reactive({
    # input$selected_countries
    # values$val_hist_search
    iso_search <- values$val_iso_search
    hist_search <- values$val_hist_search
    db_search <- values$val_db_search
    collections_load <- isolate(values$val_collections_load)
    # collections_load <- values$val_collections_load
    shares_load <- values$val_shares_load

    if (iso_search) {
      iso_codes <- input$textArea_lang_codes %>%
        str_remove_all("\\s+") %>%
        str_split_1(",") %>%
        unique()
      # values$val_iso_search <- FALSE
    } else if (hist_search){
      iso_codes <- values$hist_langs
    } else if (db_search) {
      curr_db_tbl_row_indices <- input$table_dashboard_langs_rows_all
      iso_codes <- dashboard_langs_df()[curr_db_tbl_row_indices, ] |>
        pull(`Language Code`)
    } else {
      country_codes <- countries_selected()$`Country Code` %>%
        unique()
      iso_codes <- main_rows %>%
        filter(`Country Code` %in% country_codes) %>%
        pull(`Language Code`) %>%
        unique()
      # selectRows(RT_proxy, values$val_RT_rows_selected)
    }
    # values$val_iso_search <- FALSE
    # values$val_hist_search <- FALSE
    return(iso_codes)
  })

#   observeEvent(lang_codes(), {
#     sel_rows <- values$val_RT_rows_selected
#     selectRows(RT_proxy, selected = sel_rows)
# print(paste0("observeEvent for lang_codes, selectRows: ", sel_rows))
#   })

  # lang_codes <- reactive({
  #   # req(input$selected_countries)
  #   country_codes <- countries_selected()$`Country Code` %>%
  #     unique()
  #   language_codes <- main_rows %>%
  #     filter(`Country Code` %in% country_codes) %>%
  #     pull(`Language Code`) %>%
  #     unique()
  #   return(language_codes)
  # })

  main_rows_reactive <- reactive({
    # req(values$stored_fields)
    valid_fields <- field_hierarchy$field_name |> unique()

    # 2024-08 MJ - added filter to catch deprecated field names previously stored in user cookie, e.g., 'All Access Category'
    fields <- values$stored_fields %>% .[. %in% valid_fields]
    if (is.null(fields)) { # minimum fields are the default_fields
      fields <- default_fields
    }

    df <- main_rows %>%
      filter(`Language Code` %in% lang_codes()) %>%
      droplevels() %>%
      select(Country, `Language Name`, `Language Code`, all_of(fields), rowID)
    return(df)
  })

  # use val_collection_iso_codes to select collection rows in the RT
  observeEvent(main_rows_reactive(), {
    iso_codes <- values$val_collection_iso_codes
    sel_rows <- main_rows_reactive() |>
      mutate(selected = if_else(`Language Code` %in% iso_codes,TRUE,FALSE)) |>
      pull(selected) |>
      which(arr.ind = TRUE)

    RT_proxy |> selectRows(sel_rows)
  })

  # *** dashboard page reactive and rendering functions ***

  output$choose_dashboard <- renderUI({
    choices <- c("Vision 2025", "All Access", "Engagements", "Sign Language")

    # selected <- get_cookie("sel_db_view", missing = values$vb_click_filter)
    selected <- "Vision 2025"
    # selected <- get_cookie("sel_db_view", missing = "Vision 2025")

    radioButtons("selected_db_view",
                 label = "Select view",
                 choices = choices,
                 selected = selected,
                 inline = TRUE)

  })

  # observeEvent(input$selected_db_view, {
  #   sel_view <- input$selected_db_view
  #   set_cookie("sel_db_view", cookie_value = sel_view)
  # }, ignoreInit = FALSE)

  # observeEvent(input$selected_db_view, {
  #   # req(input$selected_db_view)
  #
  #   # stored_view <- get_cookie("sel_db_view", missing = "values$vb_click_filter")
  #   stored_view <- get_cookie("sel_db_view", missing = "Vision 2025")
  #   if(sel_view != stored_view){
  #     print("sel_db_view cookie set")
  #     set_cookie("sel_db_view", cookie_value = sel_view)
  #   }
  # }, ignoreInit = FALSE)
  # }, priority = 10)

  output$select_marker_status_type <- renderUI({
    choices <- c("Vision 2025 Status", "All Access Status", "Is Remaining V2025 Need", "All Access Goal Not Met")
    # choices <- c("Vision 2025 Status", "All Access Status", "Is Remaining V2025 Need", "On All Access List")

    # selected <- get_cookie("sel_db_view", missing = values$vb_click_filter)
    # selected <- get_cookie("sel_marker_status", missing = "Vision 2025 Status")
    selected <- "Vision 2025 Status"

    radioButtons("selected_marker_status",
                 label = "Select marker status type",
                 choices = choices,
                 selected = selected,
                 inline = TRUE,
                 width = '380px')
  })

  lang_markers_reactive <- reactive(
    markers <- main_rows %>%
      # filter(Area %in% selected_areas) %>%
      select(`Country Code`, `Language Code`, `Language Name`,
             `Translation Status`, `All Access Status`, `Is Remaining V2025 Need`, `On All Access List`,
             `EGIDS Group`, `Org Engagements (WIP)`, `Denominations (WCD)`, `Agencies (WCD)`,
             `See Joshua Project`, Longitude, Latitude) %>%
      filter(`Country Code` %in% input$selected_countries) |>
      mutate(html_popup_text =   paste("<div style=width: 20%; display: inline-block; wordwrap: break-word; >",
                                       "<h5>",`Language Name`,"</h5>",
                                       "<ul>",
                                       "<li><strong>Translation Status: </strong>",`Translation Status`,"</li>",
                                       "<li><strong>All Access Status: </strong>",`All Access Status`,"</li>",
                                       "<li><strong>EGIDS Group: </strong>", `EGIDS Group`, "</li>",
                                       "<li><strong>Org Engagements: </strong>", `Org Engagements (WIP)`, "</li>",
                                       "<li><strong>Denominations: </strong>", `Denominations (WCD)`, "</li>",
                                       "<li><strong>Agencies: </strong>", `Agencies (WCD)`, "</li>",
                                       "<li><strong>See Joshua Project: </strong>", `See Joshua Project`, "</li>",
                                       "</ul>",
                                       "</div>"))
  ) |> bindCache(input$selected_countries)

  observeEvent(input$selected_marker_status, {
    sel_mkr_status <- input$selected_marker_status
    # set_cookie("sel_marker_status", cookie_value = sel_mkr_status)

    color_values <- selected_db_view_palette()$status_value
    color_palette <- selected_db_view_palette()$color
    marker_palette <- colorFactor(palette = color_palette,
                                  levels = color_values)
    # req(color_values)
    selected_status <- case_when(
      sel_mkr_status == "Vision 2025 Status" ~ "Translation Status",
      sel_mkr_status == "All Access Status" ~ "All Access Status",
      sel_mkr_status == "Is Remaining V2025 Need" ~ "Is Remaining V2025 Need",
      sel_mkr_status == "All Access Goal Not Met" ~ "On All Access List"
      # sel_mkr_status == "On All Access List" ~ "On All Access List"
    )

    lang_markers_df <- lang_markers_reactive()

    status_values <- lang_markers_df[[selected_status]]

    labels_language <- sprintf(
      "<strong><font size='+1'>%s</font></strong><br/>
         <strong>Translation Status:</strong> %s<br/>
         <strong>All Access Status:</strong> %s<br/>
         <strong>EGIDS:</strong> %s<br/>
         <strong>See Joshua Project: </strong> %s",
      lang_markers_df$`Language Name`,
      lang_markers_df$`Translation Status`,
      lang_markers_df$`All Access Status`,
      lang_markers_df$`EGIDS Group`,
      lang_markers_df$`See Joshua Project`) %>%
      map(htmltools::HTML)

    # labels_language <- sprintf(
    #   "<strong><font size='+1'>%s</font></strong><br/>
    #      <strong>Translation Status:</strong> %s<br/>
    #      <strong>All Access Status:</strong> %s<br/>
    #      <strong>EGIDS:</strong> %s",
    #   lang_markers_df$`Language Name`,
    #   lang_markers_df$`Translation Status`,
    #   lang_markers_df$`All Access Status`,
    #   lang_markers_df$`EGIDS Group`) %>%
    #   map(htmltools::HTML)

    labels_name_only <- sprintf(
      "<strong><font size='+1'>%s</font></strong>",
      lang_markers_df$`Language Name`) %>%
      map(htmltools::HTML)

    labels_name_only_small <- sprintf(
      "<span style='font-size:9px'>%s</span>",
      lang_markers_df$`Language Name`) %>%
      map(htmltools::HTML)

    proxy <- leafletProxy("summary_map", session = session)
    proxy <- proxy |> clearGroup("Languages - clustered")
    proxy <- proxy |> clearGroup("Languages - markers only")
    # proxy <- proxy |> clearGroup("Languages - markers & labels")

    proxy <- proxy |>  addCircleMarkers(
      # layerId = "lang_markers", # beware! causes only one marker, the last, to be added!
      data = lang_markers_df,
      group = "Languages - clustered",
      lng = lang_markers_df$Longitude,
      lat = lang_markers_df$Latitude,
      popup = ~map(lang_markers_df$html_popup_text, HTML),
      # popup = labels_language,
      label = labels_name_only,
      labelOptions = labelOptions(
        textOnly = TRUE,
        noHide = TRUE,
        # direction = "auto",
        direction = "bottom",
        style = list(
          "color" = "blue",
          "font-family" = "sans-serif",
          "font-size" = "10px",
          "font-weight" = "bold")),
      radius = 2,
      weight = 5,
      opacity = 0.9,
      clusterOptions = markerClusterOptions()
      , color = ~marker_palette(status_values)
    )

    proxy <- proxy |> addCircleMarkers(
        # layerId = "lang_markers", # beware! causes only one marker, the last, to be added!
        data = lang_markers_df,
        group = "Languages - markers only",
        lng = lang_markers_df$Longitude,
        lat = lang_markers_df$Latitude,
        popup = ~map(lang_markers_df$html_popup_text, HTML),
        label = labels_name_only,
        radius = 2,
        weight = 5,
        opacity = 0.9,
        clusterOptions = NULL
        , color = ~marker_palette(status_values)
      )

    # proxy <- proxy |> addCircleMarkers(
    #     # layerId = "lang_markers", # beware! causes only one marker, the last, to be added!
    #     data = lang_markers_df,
    #     group = "Languages - markers & labels",
    #     lng = lang_markers_df$Longitude,
    #     lat = lang_markers_df$Latitude,
    #     popup = ~map(lang_markers_df$html_popup_text, HTML),
    #     label = labels_name_only_small,
    #     labelOptions = labelOptions(
    #       textOnly = TRUE,
    #       noHide = TRUE,
    #       # direction = "auto",
    #       direction = "bottom",
    #       style = list(
    #         "color" = "blue",
    #         "font-family" = "sans-serif",
    #         "font-size" = "10px",
    #         "font-weight" = "bold")),
    #     radius = 2,
    #     weight = 4,
    #     opacity = 0.9,
    #     clusterOptions = NULL
    #     , color = ~marker_palette(status_values)
    #   )

    proxy <- proxy |> hideGroup(c("Vision 2025 Complete", "1 remaining", "Remaining by ranges",
                "All Access goals met", "1 unmet goal", "Unmet goals by ranges", "Languages - clustered",
                "Languages - markers only"))

    # proxy <- proxy |> hideGroup(c("Vision 2025 Complete", "1 remaining", "Remaining by ranges",
    #             "All Access goals met", "1 unmet goal", "Unmet goals by ranges", "Languages - clustered",
    #             "Languages - markers only", "Languages - markers & labels"))

    proxy <- proxy |> showGroup("Remaining by ranges")

    # proxy |> hideGroup(c("Vision 2025 Complete", "1 remaining", "Remaining by ranges",
    #             "All Access goals met", "1 unmet goal", "Unmet goals by ranges",
    #             "Languages - clustered", "Languages - markers only"))
    #

  }, ignoreInit = TRUE)

  v2025_summary <- reactive({
    req(input$selected_partner)

    area_table <- combined_partner_areas %>%
      filter(Partner == input$selected_partner) %>%
      select(Area, Country, `Country Code`, `FIPS Code`)

    # df <- main_rows %>%
    #   full_join(area_table, by = "Country Code") %>%
    df <- area_table %>%
      # full_join(main_rows)
      full_join(main_rows, by = "Country Code", multiple = "all")

    df <- df %>%
      # select(Area, `Country Code`, Country, `joshua_proj_country`,
      #        `Language Name`, `Is Remaining V2025 Need`, `Is Sign Language`) %>%
      select(Area.x, -Area.y, `Country Code`, Country.x, -Country.y, `joshua_proj_country`,
             `Language Name`, `Is Remaining V2025 Need`, `Is Sign Language`) %>%
      rename(Area = Area.x,
             Country = Country.x) %>%
      mutate(V2025 = as.character(`Is Remaining V2025 Need`)) %>%
      select(-`Is Remaining V2025 Need`) %>%
      rename(Countries = joshua_proj_country) %>%
      # rename(Countries = Country) %>%
      mutate(`Remaining need` = if_else(is.na(`V2025`) | V2025 == "No", 0, 1)) %>%
      mutate(`Sign language` = if_else(is.na(`V2025`) | `Is Sign Language` == "No", 0, 1)) %>%
      mutate(`Remaining Sign Language` = if_else(`Remaining need` == 1 & `Sign language` == 1, 1, 0)) %>%
      group_by(Area, Countries, Country, `Country Code`) %>%
      summarise(
        `Remaining Translation Needs` = sum(`Remaining need`),
        `Sign languages` = sum(`Sign language`),
        `Sign languages remaining` = sum(`Remaining Sign Language`)
      ) %>%
      mutate(`Vision 2025 Complete` = if_else(`Remaining Translation Needs` == 0, "Yes", "No")) %>%
      mutate(`1 remaining` = if_else(`Remaining Translation Needs` == 1, "Yes", "No")) %>%
      mutate(`2 to 5 remaining` = if_else(`Remaining Translation Needs` %in% 2:5, "Yes", "No")) %>%
      mutate(`6 to 20 remaining` = if_else(`Remaining Translation Needs` %in% 6:20, "Yes", "No")) %>%
      mutate(`21 or more remaining` = if_else(`Remaining Translation Needs` > 20, "Yes", "No")) %>%
      mutate(`Vision 2025 Not Complete` = if_else(`Remaining Translation Needs` != 0, "Yes", "No")) %>%
      ungroup() %>%
      select(Area, Country, Countries, `Country Code`,
             `Vision 2025 Complete`, `Vision 2025 Not Complete`, `1 remaining`, `2 to 5 remaining`,
             `6 to 20 remaining`, `21 or more remaining`, `Remaining Translation Needs`,
             `Sign languages`, `Sign languages remaining`)

    return(df)
  })

  v2025_summary_df <- reactive({
    df <- v2025_summary() |>
      select(-Country)
    return(df)
  })

  v2025_summary_export <- reactive({
    df <- v2025_summary() |>
      select(-Countries)
    return(df)
  })

  all_access_summary <- reactive({
    req(input$selected_partner)

    area_table <- combined_partner_areas %>%
      filter(Partner == input$selected_partner) %>%
      select(Area, Country, `Country Code`)

    df <- area_table %>%
      full_join(main_rows, by = "Country Code", multiple = "all")

    df <- df %>%
      select(Area.x, -Area.y, `Country Code`, Country.x, -Country.y, `joshua_proj_country`, `Language Name`,
             `All Access Goal`, `AAG chapters`, `AAG chapters remaining`,
             `All Access Status`, `On All Access List`, `Is Sign Language`) %>%
      rename(Area = Area.x,
             Country = Country.x) %>%
      mutate(aa_listed = as.character(`On All Access List`)) %>%
      select(-`On All Access List`) %>%
      rename(Countries = joshua_proj_country) %>%
      # rename(Countries = Country) %>%
      mutate(`Unmet goal` = if_else(is.na(aa_listed) | aa_listed == "No", 0, 1)) %>%
      mutate(`Sign language` = if_else(is.na(aa_listed) | `Is Sign Language` == "No", 0, 1)) %>%
      mutate(`Unmet sign language goal` = if_else(`Unmet goal` == 1 & `Sign language` == 1, 1, 0)) %>%
      group_by(Area, Countries, Country, `Country Code`) %>%
      summarise(
        `Total unmet goals` = sum(`Unmet goal`),
        `Sign languages` = sum(`Sign language`),
        `Unmet SL goals` = sum(`Unmet sign language goal`),
        `Total AAG chapters` = sum(`AAG chapters`),
        `Remaining AAG chapters` = sum(`AAG chapters remaining`),
        `% Remaining AAG chapters` = (`Remaining AAG chapters`/`Total AAG chapters`),
        # `% Remaining AAG chapters` = (`Remaining AAG chapters`/`Total AAG chapters`) |>
        #   scales::label_percent(),
        # `% Remaining AAG chapters` = paste0(format((`Remaining AAG chapters`/`Total AAG chapters`)*100, digits = 1),"%"),
        `Total SL AAG chapters` = sum(if_else(`Is Sign Language`=="Yes", `AAG chapters`, 0)),
        `Remaining SL AAG chapters reported` = sum(if_else(`Is Sign Language`=="Yes", `AAG chapters remaining`, 0))
      ) %>%
      mutate(`All Access goals met` = if_else(`Total unmet goals` == 0, "Yes", "No")) %>%
      mutate(`1 unmet goal` = if_else(`Total unmet goals` == 1, "Yes", "No")) %>%
      mutate(`2 to 5 unmet goals` = if_else(`Total unmet goals` %in% 2:5, "Yes", "No")) %>%
      mutate(`6 to 20 unmet goals` = if_else(`Total unmet goals` %in% 6:20, "Yes", "No")) %>%
      mutate(`21 or more unmet goals` = if_else(`Total unmet goals` > 20, "Yes", "No")) %>%
      ungroup() %>%
      select(Area, Country, Countries, `Country Code`,
             `All Access goals met`, `1 unmet goal`, `2 to 5 unmet goals`,
             `6 to 20 unmet goals`, `21 or more unmet goals`,
             `Total unmet goals`, `Total AAG chapters`, `Remaining AAG chapters`, `% Remaining AAG chapters`,
             `Sign languages`, `Unmet SL goals`, `Total SL AAG chapters`, `Remaining SL AAG chapters reported`)

    return(df)

   })

  all_access_summary_df <- reactive({
    all_access_summary() |>
      select(-Country)
  })

  all_access_summary_export <- reactive({
    all_access_summary() |>
      select(-Countries)
  })

  # used only for db chart
  dashboard_df <- reactive({
    # req(input$summary_panels == "distribution")
    # dashboard <- get_cookie("sel_db_view", missing = "Vision 2025")
    db_view <- input$selected_db_view

    fields <- dashboard_fields %>%
      filter(Dashboard == db_view) %>%
      # filter(field_name != "Language Code") %>%
      # filter(field_name != "Language Name") %>%
      # filter(field_name != "Country") %>%
      pull(field_name) %>%
      unique()

    # df <- main_rows |>
    #   filter(`Country Code` %in% input$selected_countries)

    if (db_view == "Engagements") {
      df <- WIP_rows |>
        filter(`Country Code` %in% input$selected_countries) |>
        left_join(main_rows) |>
        left_join(country_code_lookup) |>
        select(Country, all_of(fields)) |>
        filter(!is.na(Country))
      # df <- df |>
      #   left_join(WIP_rows) |>
      #   select(all_of(fields))
# print("str for dashboard_df reactive output")
# str(df)
# print(n = 100, df |> count(Country, `Country Code`, `Engagement Status`))
    } else {
      df <- main_rows %>%
        filter(`Country Code` %in% input$selected_countries) |>
        select(all_of(fields))
      # df <- df %>%
      #   select(all_of(fields))
    }
    return(df)
  })

  global_lang_count <- reactive({
    df <- main_rows %>%
      summarise(
        total_langs = n(),
      )
  })

  global_SL_count <- reactive({
    df <- main_rows %>%
      filter(`Is Sign Language` == "Yes") %>%
      summarise(
        total_langs = n(),
      )
  })

  local_lang_count <- reactive({
    df <- main_rows %>%
      filter(`Country Code` %in% input$selected_countries) %>%
      summarise(
        total_langs = n(),
      )
    return(df)
  })

  local_SL_count <- reactive({
    df <- main_rows %>%
      filter(`Is Sign Language` == "Yes") %>%
      filter(`Country Code` %in% input$selected_countries) %>%
      summarise(
        total_langs = n(),
      )
  })

  global_AA_count <- reactive({
    df <- main_rows %>%
      filter(`On All Access List` == "Yes") %>%
      summarise(total_langs = n())
  })

  global_SL_AA_count <- reactive({
    df <- main_rows %>%
      filter(`Is Sign Language` == "Yes") %>%
      filter(`On All Access List` == "Yes") %>%
      summarise(total_langs = n())
  })

  local_AA_count <- reactive({
    df <- main_rows %>%
      filter(`Country Code` %in% input$selected_countries,
             `On All Access List` == "Yes") %>%
      summarise(total_langs = n())
  })

  local_SL_AA_count <- reactive({
    df <- main_rows %>%
      filter(`Is Sign Language` == "Yes") %>%
      filter(`Country Code` %in% input$selected_countries,
             `On All Access List` == "Yes") %>%
      summarise(total_langs = n())
  })

  global_V2025_count <- reactive({
    df <- main_rows %>%
      filter(`Is Remaining V2025 Need` == "Yes") %>%
      summarise(
        total_langs = n(),
      )
  })

  global_SL_V2025_count <- reactive({
    df <- main_rows %>%
      filter(`Is Sign Language` == "Yes") %>%
      filter(`Is Remaining V2025 Need` == "Yes") %>%
      summarise(
        total_langs = n(),
      )
  })

  local_V2025_count <- reactive({
    df <- main_rows %>%
      filter(`Country Code` %in% input$selected_countries,
             `Is Remaining V2025 Need` == "Yes") %>%
      summarise(
        total_langs = n(),
      )
  })

  local_SL_V2025_count <- reactive({
    df <- main_rows %>%
      filter(`Is Sign Language` == "Yes") %>%
      filter(`Country Code` %in% input$selected_countries,
             `Is Remaining V2025 Need` == "Yes") %>%
      summarise(
        total_langs = n(),
      )
  })

  output$vb_all_languages <- renderValueBox({
    req(input$selected_countries)
    # req(input$selected_db_view)

    # if (input$selected_db_view == "Sign Language") {
    #   value = global_SL_count()$total_langs
    #   subtitle = "Total sign languages in all countries. (See list below)"
    # } else {
    #   value = local_lang_count()$total_langs
    #   subtitle = paste0("Total languages in selected countries. (Global total: ", global_lang_count()$total_langs, ")")
    # }

    if (input$selected_db_view == "Sign Language") {
      value = local_SL_count()$total_langs
      subtitle = paste0("Total sign languages in selected countries. (Global total: ",
                        global_SL_count()$total_langs, ")")
    } else {
      value = local_lang_count()$total_langs
      subtitle = paste0("Total languages in selected countries. (Global total: ",
                        global_lang_count()$total_langs, ")")
    }

    vbox <- valueBox(value = value,
             subtitle = subtitle,
             icon = icon("language"),
             # color = "lightblue",
             color = "light-blue",
             href = "#")
             # href = "#table_dashboard_langs")

    vbox$children[[1]]$attribs$class<-"action-button"
    vbox$children[[1]]$attribs$id<-"button_vb_all_languages"
    return(vbox)
  })

  output$vb_all_access <- renderValueBox({
    req(input$selected_countries)
    # req(input$selected_db_view)

    # if (input$selected_db_view == "Sign Language") {
    #   value = global_SL_AA_count()$total_langs
    #   subtitle = "Sign languages with unmet All Access goals."
    # } else {
    #   value = local_AA_count()$total_langs
    #   subtitle = paste0("Unmet All Access goals. (Global total:  ", global_AA_count()$total_langs, ")")
    # }

    if (input$selected_db_view == "Sign Language") {
      value = local_SL_AA_count()$total_langs
      subtitle = "Sign languages with unmet All Access goals."
    } else {
      value = local_AA_count()$total_langs
      subtitle = paste0("Unmet All Access goals. (Global total:  ", global_AA_count()$total_langs, ")")
    }

    vbox <- valueBox(value = value,
             subtitle = subtitle,
             icon = icon("bullseye"),
             color = "orange",
             href = "#")

    vbox$children[[1]]$attribs$class<-"action-button"
    vbox$children[[1]]$attribs$id<-"button_vb_all_access"
    return(vbox)
  })

  output$vb_v2025 <- renderValueBox({
    req(input$selected_countries)
    # req(input$selected_db_view)

    # if (input$selected_db_view == "Sign Language") {
    #   value = global_SL_V2025_count()$total_langs
    #   subtitle = "Sign languages yet to begin translation (Vision 2025)"
    # } else {
    #   value = local_V2025_count()$total_langs
    #   subtitle = paste0("Languages yet to begin translation (Vision 2025). (Global total: ", global_V2025_count()$total_langs, ")")
    # }

    if (input$selected_db_view == "Sign Language") {
      value = local_SL_V2025_count()$total_langs
      subtitle = "Vision 2025 sign languages remaining"
    } else {
      value = local_V2025_count()$total_langs
      subtitle = paste0("Vision 2025 languages remaining (Global total: ", global_V2025_count()$total_langs, ")")
    }

    vbox <- valueBox(value = value
            ,subtitle = subtitle
            ,icon = icon("hourglass-start")
            # ,color = "danger"
            ,color = "red"
            ,href = "#")

    vbox$children[[1]]$attribs$class<-"action-button"
    vbox$children[[1]]$attribs$id<-"button_vb_V2025"
    return(vbox)
  })

  observeEvent(input$button_vb_all_languages, {
    updateTabsetPanel(
      session = getDefaultReactiveDomain(),
      inputId = "summary_panels",
      selected = "Map (summaries by country)"
    )
    values$vb_click_filter <- "none"
    shinyjs::runjs("scrollToCitation();")
  })

  observeEvent(input$button_vb_V2025, {
    # curr_selected_dashboard <- get_cookie("sel_db_view", missing = "Vision 2025")
    curr_selected_dashboard <- input$selected_db_view
    values$vb_click_filter <- "V2025"
    values$sel_summary_panel = "Map (summaries by country)"
    if (curr_selected_dashboard != "Vision 2025" & curr_selected_dashboard != "Sign Language") {
    # if (curr_selected_dashboard != "Vision 2025") {
      # values$default_selected_dashboard = "Vision 2025"
      updateRadioButtons(
        inputId = "selected_db_view",
        selected = "Vision 2025")
    }
    updateTabsetPanel(
      session = getDefaultReactiveDomain(),
      inputId = "summary_panels",
      # selected = "tab_summary_map"
      selected = "Map (summaries by country)"
    )
    shinyjs::runjs("scrollToCitation();")
    # shinyjs::runjs("enhancedScrollToCitation(10, 150);")
  })

  observeEvent(input$button_vb_all_access, {
    # curr_selected_dashboard <- get_cookie("sel_db_view", missing = "All Access")
    curr_selected_dashboard <- input$selected_db_view
    values$vb_click_filter <- "All Access"
    values$sel_summary_panel = "Map (summaries by country)"
    # if (!curr_selected_dashboard %in% C("All Access", "Sign Language")) {
    if (curr_selected_dashboard != "All Access" & curr_selected_dashboard != "Sign Language") {
      # values$default_selected_dashboard = "All Access"
      updateRadioButtons(
        inputId = "selected_db_view",
        selected = "All Access")
    }
    updateTabsetPanel(
      session = getDefaultReactiveDomain(),
      inputId = "summary_panels",
      selected = "Map (summaries by country)"
    )
    shinyjs::runjs("scrollToCitation();")
    # shinyjs::runjs("enhancedScrollToCitation(10, 150);")
  })


  observeEvent(values$vb_click_filter, {
    req(values$vb_click_filter)
    # filter_clicked <- values$vb_click_filter
    current_db_view <- input$selected_db_view
    # if (current_db_view == "Vision 2025") {
    #   status_type <- "Translation Status"
    # } else if (current_db_view == "All Access") {
    #   status_type <- "All Access Status"
    # }
    status_type <- case_when(
      current_db_view == "Vision 2025" ~ "Translation Status",
      current_db_view == "All Access" ~ "All Access Status",
      current_db_view == "Engagement" ~ "All Access Status",
      current_db_view == "Sign Language" ~ "Translation Status",
    )

    color_values <- summary_map_palette()$status_value
    color_palette <- summary_map_palette()$color
    mrkr_palette <- colorFactor(palette = color_palette,
                           levels = color_values)
    req(color_values)

    lng_codes <- isolate(dashboard_langs_df()) |>
      pull(`Language Name`)

    lng_markers_df <- main_rows %>%
      select(`Country Code`, `Language Code`, `Language Name`, `Translation Status`, `All Access Status`,
             `EGIDS Group`, Longitude, Latitude) %>%
      # filter(`Country Code` %in% input$selected_countries) |>
      filter(`Language Name` %in% lng_codes)



    lng_labels <- sprintf(
      "<strong><font size='+1'>%s</font></strong><br/>
         <strong>Translation Status:</strong> %s<br/>
         <strong>All Access Status:</strong> %s<br/>
         <strong>EGIDS:</strong> %s",
      lng_markers_df$`Language Name`,
      lng_markers_df$`Translation Status`,
      lng_markers_df$`All Access Status`,
      lng_markers_df$`EGIDS Group`) %>%
      map(htmltools::HTML)

    # if (values$vb_click_filter %in% c("V2025", "All Access") && length(lng_codes) > 0) {
    if (length(lng_codes) > 0) {
      m <- leafletProxy("summary_map", session) |>
        hideGroup(c("Remaining by ranges")) |>
        # showGroup("Language markers") |>
        # removeMarkerCluster("lang_markers") |>
        clearMarkerClusters() |>

        addCircleMarkers(
          # layerId = "lang_markers",
          data = lng_markers_df,
          group = "Languages - clustered",
          lng = lng_markers_df$Longitude,
          lat = lng_markers_df$Latitude,
          label = lng_labels,
          radius = 4,
          weight = 5,
          opacity = 0.9,
          clusterOptions = markerClusterOptions()
          , color = ~mrkr_palette(lng_markers_df[[status_type]])
          ) |>

        addCircleMarkers(
          # layerId = "lang_markers", # beware! causes only one marker, the last, to be added!
          data = lng_markers_df,
          group = "Languages - markers only",
          lng = lng_markers_df$Longitude,
          lat = lng_markers_df$Latitude,
          label = lng_labels,
          # labelOptions = labelOptions(
          #   textOnly = TRUE,
          #   noHide = TRUE,
          #   # direction = "auto",
          #   direction = "bottom",
          #   style = list(
          #     "color" = "blue",
          #     "font-family" = "serif",
          #     "font-size" = "12px",
          #     "font-weight" = "bold")),
          radius = 4,
          weight = 5,
          opacity = 0.9,
          clusterOptions = NULL
          , color = ~mrkr_palette(lng_markers_df[[status_type]])
        ) |>
        showGroup("Languages - clustered")

      return(m)
    }
  }, ignoreInit = TRUE)


  partner_areas_df <- reactive({
    # req(input$partners)
    df <- combined_partner_areas %>%
      filter(Partner == input$selected_partner) %>%
      select(Area, `Country Code`)
    return(df)
  })

  ##############################################################################################
  # observer bound to dashboard chart plotly_click events to filter the dashboard_langs_df
  # based on the langs represented by the clicked segment
  ##############################################################################################

  observe({ # bound to event_data(event = "plotly_click", source = "A", priority = "event")

    switch_cats <- ifelse(is.null(values$val_switch_cats), 0, values$val_switch_cats)
    plot_data <- values$curr_db_plot_df

    db_plot_EventData <- db_plot_event_reactive()

    if (input$selected_db_view %in% c("Vision 2025", "All Access", "Sign Language")) {
      if (switch_cats == 0) {
        valid_y_values <- plot_data[[2]]
        y_list <- plot_data[[2]] |> levels() |> intersect(valid_y_values)
        fill_list <- plot_data[[1]] |> unique()
        db_plot_y <- y_list[[db_plot_EventData[[4]]]]
        db_plot_fill <- fill_list[[db_plot_EventData[[1]]+1]]
      } else {
        valid_y_values <- plot_data[[2]]
        y_list <- plot_data[[2]] |> levels() |> rev() |> intersect(valid_y_values)
        valid_fill_values <- plot_data[[1]]
        fill_list <- plot_data[[1]] |> unique()
        db_plot_y <- y_list[[db_plot_EventData[[4]]]]
        db_plot_fill <- fill_list[[db_plot_EventData[[1]]+1]]
      }

    } else if (input$selected_db_view == "Engagements") {
      if (switch_cats == 0) {
        y_list <- plot_data[[2]] |> levels() |> rev()
        fill_list <- plot_data[[1]] |> unique()
        db_plot_y <- y_list[[db_plot_EventData[[4]]]]
        db_plot_fill <- fill_list[[db_plot_EventData[[1]]+1]]
      } else {
        y_list <- plot_data[[2]] |> levels() |> rev()
        valid_fill_values <- plot_data[[1]]
        fill_list <- plot_data[[1]] |> levels() |> intersect(valid_fill_values)
        db_plot_y <- y_list[[db_plot_EventData[[4]]]]
        db_plot_fill <- fill_list[[db_plot_EventData[[1]]+1]]
      }

      org_list <- plot_data[[3]] |> unique() |> sort()

    }

    fill_var_name <- names(plot_data)[1]
    y_var_name <- names(plot_data)[2]

    if (input$selected_db_view == "Engagements") {
      table <- WIP_rows |>
        left_join(main_rows) %>%
        select(values$db_lang_table_names) |>
        filter(.data[[fill_var_name]] == db_plot_fill,
               .data[[y_var_name]] == db_plot_y)
    } else {
      table <- main_rows %>%
        select(values$db_lang_table_names) |>
        filter(.data[[fill_var_name]] == db_plot_fill,
               .data[[y_var_name]] == db_plot_y)
    }

    if (input$selected_db_view == "Sign Language") {
      table <- table |>
        filter(`Is Sign Language` == "Yes")
    }

    # improves the formatting of these date field in the datatable
    if("Project Began" %in% names(table)) {
      table <- table |>
        mutate(`Project Began` = as.Date(`Project Began`)) |>
        mutate(`Project Ended` = as.Date(`Project Ended`))
    }

    replaceData(proxy_table_db_langs, table)

    # shinyjs::runjs(sprintf("document.getElementById('%s').scrollIntoView({behavior: 'smooth'});", "db_details_table_citation"))

    shinyjs::runjs(
      "
      function scrollToCitation() {
        // Ensure we're on the 'dashboards' tab
        var dashboardsTab = $('a[data-value=\"dashboards\"]');
        if (dashboardsTab.length && !dashboardsTab.parent().hasClass('active')) {
          dashboardsTab.tab('show');
        }

        // Give the DOM time to update
        setTimeout(function() {
          var container = document.getElementById('scrollable-content');
          var table = document.getElementById('table_dashboard_langs');  // Assuming this is the ID of your table
          if (container && table) {
            // Ensure the language details tab is visible
            var languageDetailsTabset = document.getElementById('language_details_tabset');
            if (languageDetailsTabset) {
              $(languageDetailsTabset).find('a:first').tab('show');
            }

            // Give the DOM time to update again
            setTimeout(function() {
              var containerRect = container.getBoundingClientRect();
              var tableRect = table.getBoundingClientRect();
              var scrollAmount = tableRect.top - containerRect.top - 20;  // 20px offset from the top

              container.scrollTo({
                top: container.scrollTop + scrollAmount,
                behavior: 'smooth'
              });

              // Scroll the page to make sure the container is in view
              container.scrollIntoView({behavior: 'smooth', block: 'start'});
            }, 100);
          }
        }, 100);
      }
      scrollToCitation();
      "
    )
 }) %>% bindEvent(event_data(event = "plotly_click", source = "A", priority = "event"))

  db_plot_event_reactive <- reactive({
    req(input$summary_panels == "distribution")

    click_data <- event_data(event = "plotly_click", source = "A", priority = "event") |> unlist()
    # print("click_data for 'A'")
    # str(click_data)

    plot_df <- values$curr_db_plot_df
    # print("str for plot_df for db_plot_event_reactive")
    # str(plot_df)

    return(click_data)
  })

  dashboard_langs_df <- reactive({
    req(input$selected_db_view)
    # req(values$vb_click_filter)

    curr_selected_view <- input$selected_db_view
    # curr_selected_view <- get_cookie("sel_db_view", missing = values$vb_click_filter)
    # curr_selected_view <- values$vb_click_filter

    # default dashboard fields from google sheet
    fields <- dashboard_fields %>%
      filter(Dashboard == curr_selected_view) %>%
      # filter(Dashboard == input$selected_db_view) %>%
      filter(field_name != "Language Code") %>%
      filter(field_name != "Language Name") %>%
      filter(field_name != "Country") %>%
      pull(field_name) %>%
      unique()

    # default table data, by dashboard

    if (curr_selected_view == "Engagements") {
    # if (input$selected_db_view == "Engagements") {
      # df <- main_rows %>%
      #   left_join(WIP_rows, by = c("Language Code", "Country Code")) %>%
      df <- WIP_rows %>%
        left_join(main_rows, by = c("Language Code", "Country Code")) %>%
        select(Country, `Is Remaining V2025 Need`, `On All Access List`, `Language Code`,
               `Language Name`, `Is Sign Language`, all_of(fields), -`1st Language Pop`, -Latitude, -Longitude) %>%
        # select(`Language Name`, all_of(fields), -`1st Language Pop`, -Latitude, -Longitude) %>%
        filter(`Country Code` %in% input$selected_countries) %>%
        select(-`Country Code`)

    } else if (curr_selected_view == "Sign Language") {
    # } else if (input$selected_db_view == "Sign Language") {
      df <- main_rows %>%
        left_join(partner_areas_df(), by = c("Country Code")) %>%
        mutate(Area = factor(Area.y)) %>%
        select(-Area.x, -Area.y)

      df <- df %>%
        filter(`Country Code` %in% input$selected_countries) %>%
        filter(`Is Sign Language` == "Yes") %>%
        select(
          `Is Remaining V2025 Need`, `On All Access List`, `Language Code`,
          Area, Country, `Language Name`, `Is Sign Language`, all_of(fields),
          -Latitude, -Longitude, -`Country Code`)
    # } else if (input$selected_db_view == "Status by Country") {
    #   df <- main_rows %>%
    #     filter(`Country Code` %in% input$selected_countries) %>%
    #     select(Country,
    #            all_of(fields),
    #            -Latitude, -Longitude,
    #            -`Country Code`)
    } else {
      df <- main_rows %>%
        select(`Is Remaining V2025 Need`, `On All Access List`,
               `Language Code`, Country, `Country Code`,
               `Language Name`, all_of(fields), -Latitude, -Longitude) %>%
        # filter(`Country` %in% input$selected_countries)
        filter(`Country Code` %in% input$selected_countries) %>%
        select(-`Country Code`)
    }

    if(values$vb_click_filter == "All Access") {
      df <- df %>%
        filter(`On All Access List` == "Yes")
    } else if (values$vb_click_filter == "V2025") {
      df <- df %>%
        filter(`Is Remaining V2025 Need` == "Yes")
    } else if (values$vb_click_filter == "none") {
      df
    }


    # return(df)

  })

  observeEvent(input$selected_db_view, {
    # print("observeEvent(input$selected_db_view")
    if (input$selected_db_view == "Vision 2025") {
      if (!(input$summary_panels %in% c("summary_map", "distribution"))) {
        updateTabsetPanel(session, "summary_panels", selected = "v2025_summaries")
        # updateTabsetPanel(session, "summary_panels", selected = "Vision 2025")
      }
      values$vb_click_filter == "Vision 2025"
    } else if (input$selected_db_view == "All Access") {
      if (!(input$summary_panels %in% c("summary_map", "distribution"))) {
        updateTabsetPanel(session, "summary_panels", selected = "aa_summaries")
        # updateTabsetPanel(session, "summary_panels", selected = "All Access")
      }
      values$vb_click_filter == "All Access"
    } else if (input$selected_db_view == "Engagements") {
      updateTabsetPanel(session, "summary_panels", selected = "distribution")
      values$vb_click_filter == "none"
    } else if (input$selected_db_view == "Sign Language") {
      values$vb_click_filter == "none"
    }
    # values$db_langs_tbl_data <- dashboard_langs_df()
    # values$db_tbl_chosen_flds <- NULL
    # values$vb_click_filter = "none"
  })

  # observe({
  #   browser()
  #   values$default_selected_dashboard <- case_when(
  #     input$summary_panels == "Vision 2025 Summary (by org areas and countries)" ~ "Vision 2025",
  #     input$summary_panels == "All Access Summary (by org areas and countries)" ~ "All Access",
  #     input$summary_panels == "Map (summary by country)" ~ "Vision 2025",
  #     input$summary_panels == "Language Distribution (by selected countries)" ~ "Vision 2025"
  #   )
  # }) %>% bindEvent(input$summary_panels)

  observe({
    panel <- input$summary_panels
    view <- case_when(
              panel == "v2025_summaries" ~ "Vision 2025",
              panel == "aa_summaries" ~ "All Access"
            )
    if (!is.null(view)) {
    updateRadioButtons(session,
                       "selected_db_view",
                       selected = view)
    }
  }) %>% bindEvent(input$summary_panels)

  dataModel <- function(left_hand_list, right_hand_list, update_btn_name, failed = FALSE) {
    modalDialog(
      title = "Select and order columns",
      size = "m",
      bucket_list(
        header = "Drag and drop column names in order",
        group_name = "ordered_names",
        orientation = "horizontal",
        add_rank_list(
          text = "Optional columns",
          labels = left_hand_list,
          # labels = values$db_tbl_fld_choices,
          input_id = "bucket_choices"),
        add_rank_list(
          text = "Drop and arrange here",
          labels = right_hand_list,
          # labels = values$db_tbl_chosen_flds
          input_id = "bucket_chosen")
      ),
      footer = tagList(
        actionButton(update_btn_name, "Update table"),
        # actionButton("update_langs_tbl", "Update table"),
        modalButton("Done")
      )
    )
  }

  # observeEvent(input$link_change_lang_detail_cols, {
  #
  #   showModal(dataModel(
  #     left_hand_list = values$db_tbl_fld_choices,
  #     right_hand_list = values$db_tbl_chosen_flds,
  #     update_btn_name = "update_langs_table"
  #     ))
  # })
  #
  # observe({ # bound to input$update_langs_table, i.e., the "update" action button in bucket list modal
  #   values$db_tbl_fld_choices <- input$bucket_choices
  #   values$db_tbl_chosen_flds <- input$bucket_chosen
  #   if(length(input$bucket_chosen) > 0) {
  #     values$db_langs_tbl_data <- dashboard_langs_df() %>%
  #       select(input$bucket_chosen)
  #   } else {
  #     values$db_langs_tbl_data <- dashboard_langs_df()
  #   }
  # }) %>% bindEvent(input$update_langs_table)

  # reset values when new dashboard is selected
  # observeEvent(input$selected_db_view, {
  #   values$db_langs_tbl_data <- dashboard_langs_df()
  #   values$db_tbl_chosen_flds <- NULL
  #   # values$vb_click_filter = "none"
  # })

  # *** summary table field select/order model ***

  observeEvent(input$link_change_V2025_summary_cols, {
    t
    showModal(dataModel(
      left_hand_list = values$V2025_summary_tbl_fld_choices,
      right_hand_list = values$V2025_summary_tbl_chosen_flds,
      update_btn_name = "update_V2025_summary_table"
      ))
  })

  observeEvent(input$link_change_all_access_summary_cols, {

    showModal(dataModel(
      left_hand_list = values$all_access_summary_tbl_fld_choices,
      right_hand_list = values$all_access_summary_tbl_chosen_flds,
      update_btn_name = "update_all_access_summary_table"
      ))
  })

  observe({ # bound to input$update_V2025_summary_table, i.e., the "update" action button in bucket list modal
    values$V2025_summary_tbl_fld_choices <- input$bucket_choices
    values$V2025_summary_tbl_chosen_flds <- input$bucket_chosen
    if(length(input$bucket_chosen) > 0) {
      values$V2025_summary_tbl_data <- v2025_summary_df() %>%
        select(Area, Countries, `Country Code`, all_of(values$V2025_summary_tbl_chosen_flds))
        # select(input$bucket_chosen)
    } else {
      values$V2025_summary_tbl_data <- v2025_summary_df()
    }
  }) %>% bindEvent(input$update_V2025_summary_table)

  observe({ # bound to input$update_all_access_summary_table, i.e., the "update" action button in bucket list modal
    values$all_access_summary_tbl_fld_choices <- input$bucket_choices
    values$all_access_summary_tbl_chosen_flds <- input$bucket_chosen
    if(length(input$bucket_chosen) > 0) {
      values$all_access_summary_tbl_data <- all_access_summary_df() %>%
        select(Area, Countries, `Country Code`, all_of(values$all_access_summary_tbl_chosen_flds))
        # select(input$bucket_chosen)
    } else {
      values$all_access_summary_tbl_data <- all_access_summary_df()
    }
  }) %>% bindEvent(input$update_all_access_summary_table)

  # reset values when new partner is selected
  observe({ # bound to input$selected_partner
#  print("@observer: input$selected_partner and input$selected_areas")
    values$V2025_summary_tbl_data <- v2025_summary_df()
    values$V2025_summary_tbl_chosen_flds <- NULL
    values$all_access_summary_tbl_data <- all_access_summary_df()
    values$all_access_summary_tbl_chosen_flds <- NULL
  }) %>% bindEvent(input$selected_partner, input$selected_areas)
  # }) %>% bindEvent(input$selected_partner)

  # *** dashboard language table creation ***

  output$table_dashboard_langs <- renderDT({
    # req(input$selected_db_view)
    # req(countries_selected()$`Country Code`)
    # req(dashboard_langs_df())

    # if(is.null(values$db_tbl_chosen_flds)) {
    #   table <- dashboard_langs_df()
    #   values$db_tbl_fld_choices <- names(table)
    # } else {
    #   table <- values$db_langs_tbl_data
    # }

    table <- dashboard_langs_df()

    table_names <- names(table)

    # improves the format of these date field in the datatable
    if("Project Began" %in% table_names) {
      table <- table |>
        mutate(`Project Began` = as.Date(`Project Began`)) |>
        mutate(`Project Ended` = as.Date(`Project Ended`))
    }

    values$db_lang_table_names <- table_names

    datatable(table,
              # elementId = "db_langs_table",
              caption = "Note: To refine the list use the search box and/or column filters.",
              filter = list(position = 'top', clear = TRUE, plain = TRUE),
              escape = FALSE,
              class = 'display compact',
              # selection = NULL,
              selection = list(mode = "single", target = "cell"),
              extensions = c('Buttons', 'ColReorder'),
              options = list(
                             scrollY = TRUE,
                             scrollX = TRUE,
                             scrollcollapse = TRUE,

                             # scrollY = '60vh',
                             # scrollX = '100%',
                             # scrollcollapse = FALSE,
                             # options = list(scrollY = '60vh',
                             #                scrollcollapse = TRUE,
                             dom = 'Blfritip',
                             buttons = list('copy', 'print',
                                            list(extend = 'collection',
                                                 buttons = c('csv', 'excel', 'pdf'),
                                                 text = 'Download')
                             ),
                             lengthMenu = list(c(-1, 10, 25, 50), c("All", 10, 25, 50)),
                             colReorder = TRUE,
                             columnDefs = list(
                               list(
                                 targets = c(1, 2, 3),
                                 visible = FALSE
                               )
                             )
              )
    ) %>%
      { if("1st Language Pop" %in% table_names)
        formatCurrency(., columns = "1st Language Pop",
                         currency = "",
                         digits = 0)
      else .}
    # %>%
    #   { if ("Project Began" %in% table_names)
    #     formatDate(., columns = c(13, 14),
    #                method = JS("function(data) { return data ? data.split(' ')[0] : ''; }"))
    #     else .}
  })

  proxy_table_db_langs <- dataTableProxy("table_dashboard_langs")

  output$btn_send_db_langs_to_RT <- renderUI({
    actionButton("btn_send_db_langs_2_RT",
                 label = "Send list to Research Table",
                 icon = icon("share"))
  })

  observe({
    values$val_db_search <- TRUE
    values$val_hist_search <- FALSE
    values$val_iso_search <- FALSE
    updateTabsetPanel(session, "main_page_tabset", selected = "table_builder")
    # updateTabsetPanel(session, "main_page_tabset", selected = "Research Table Builder")
  }) |> bindEvent(input$btn_send_db_langs_2_RT, ignoreInit = TRUE)

  ########################################################################
  # Morris Johnson - 2024-06-26
  # Translation status history table
  ########################################################################

  combine_rows_by_date <- function(df) {
    df %>%
      group_by(SnapshotDate) %>%
      summarise(
        # CountryCode = paste(unique(CountryCode), collapse = ", "),
        year = first(year),
        month = first(month),
        No = sum(No),
        Yes = sum(Yes),
        Net_starts_this_month = sum(`Net starts this month`),
        Removed = sum(Removed),
        Added = sum(Added),
        RemainingNeeds_Langs = paste(unique(unlist(str_split(RemainingNeeds_Langs, ", "))), collapse = ", "),
        Removed_Langs = paste(unique(unlist(str_split(Removed_Langs, ", "))), collapse = ", "),
        Added_Langs = paste(unique(unlist(str_split(Added_Langs, ", "))), collapse = ", ")
      ) %>%
      arrange(SnapshotDate)
  }

  output$select_first_snapshot_ts <- renderUI({
    fluidRow(
      column(width = 6,
         tags$label("Select earliest Snapshot:",
                    style = "display: inline-block;
                         vertical-align: top;
                         padding-top: 6px;")
      ),
      column(width = 6,
         selectInput("selected_first_snapshot_ts",
                    label = NULL,
                    # label = "Select earliest Snapshot",
                    choices = snapshot_dates_list,
                    selected = min(snapshot_dates_list))
      )
    )
  })

  output$select_last_snapshot_ts <- renderUI({
    fluidRow(
      column(width = 6,
             tags$label("Select latest Snapshot:",
                        style = "display: inline-block;
                         vertical-align: top;
                         padding-top: 6px;")
      ),
      column(width = 6,
          selectInput("selected_last_snapshot_ts",
                    label = NULL,
                    # label = "Select latest Snapshot",
                    choices = snapshot_dates_list,
                    selected = max(snapshot_dates_list))
      )
    )
  })

  output$select_first_snapshot_aa <- renderUI({
    fluidRow(
      column(width = 6,
             tags$label("Select earliest Snapshot:",
                        style = "display: inline-block;
                         vertical-align: top;
                         padding-top: 6px;")
      ),
      column(width = 6,
             selectInput("selected_first_snapshot_aa",
                         label = NULL,
                         # label = "Select earliest Snapshot",
                         choices = aa_snapshot_dates_list,
                         selected = min(aa_snapshot_dates_list))
      )
    )
  })

  output$select_last_snapshot_aa <- renderUI({
    fluidRow(
      column(width = 6,
             tags$label("Select latest Snapshot:",
                        style = "display: inline-block;
                         vertical-align: top;
                         padding-top: 6px;")
      ),
      column(width = 6,
             selectInput("selected_last_snapshot_aa",
                         label = NULL,
                         # label = "Select latest Snapshot",
                         choices = aa_snapshot_dates_list,
                         selected = max(aa_snapshot_dates_list))
      )
    )
  })

  output$select_country_scope_ts <- renderUI({
    radioButtons("selected_country_scope_ts",
                 label = "Countries to include",
                 choices = c("Selected only", "All (global)"),
                 selected = "Selected only",
                 inline = TRUE)
  })

  output$select_country_scope_aa <- renderUI({
    radioButtons("selected_country_scope_aa",
                 label = "Countries to include",
                 choices = c("Selected only", "All (global)"),
                 selected = "Selected only",
                 inline = TRUE)
  })

  summary_history_df <- reactive({
    print("summary_history_df loading...")

    curr_panel <- input$summary_panels

    if (curr_panel == "v2025_summaries") {
      req(input$selected_country_scope_ts)
    } else if (curr_panel == "aa_summaries") {
      req(input$selected_country_scope_aa)
    }

    req(input$selected_countries)

    # print(paste0("input$summary_panels: ", input$summary_panels))

    selected_countries <- input$selected_countries
    # selected_countries <- c("NG", "ET", "TZ", "GH", "UG", "SD", "SS",
    #                         "AO", "MG", "LR", "KE", "BW", "MZ", "NA")


    req(curr_panel)
    if (curr_panel == "v2025_summaries") {
      country_scope <- input$selected_country_scope_ts
      first_snapshot <- input$selected_first_snapshot_ts
      last_snapshot <- input$selected_last_snapshot_ts
    } else if (curr_panel == "aa_summaries") {
      country_scope <- input$selected_country_scope_aa
      first_snapshot <- input$selected_first_snapshot_aa
      last_snapshot <- input$selected_last_snapshot_aa
    } else if (curr_panel == "distribution") {
      country_scope <- "Selected only"
      first_snapshot <- min(snapshot_dates_list)
      last_snapshot <- max(snapshot_dates_list)
    }

    # first_snapshot <- NULL
    # last_snapshot <- NULL
    # first_snapshot <- input$first_snapshot
    # last_snapshot <- input$last_snapshot

    ds_name <-  "pb_main_ds"
    source <- paste0("data/datasets/", ds_name)
    ds <- arrow::open_dataset(source, format = "parquet")

    df <- ds %>%
      (function(x) {
        if(country_scope == "Selected only") {
          x <- x %>% dplyr::filter(`Country Code` %in% selected_countries)
        }
        if(!is.null(first_snapshot)) {
          x <- x %>% dplyr::filter(SnapshotDate >= first_snapshot & SnapshotDate <= last_snapshot)
        }
        if(curr_panel == "v2025_summaries") {
          x <- x %>% dplyr::select(SnapshotDate,
                                   year,
                                   month,
                                   `Language Code`,
                                   `Country Code`,
                                   `Is Remaining V2025 Need`)
        }
        if(curr_panel == "aa_summaries") {
          x <- x %>% dplyr::select(SnapshotDate,
                                   year,
                                   month,
                                   `Language Code`,
                                   `Country Code`,
                                   `All Access Goal Met`)
        }
        return(x)
      }) %>%
      dplyr::collect()


    if (curr_panel == "v2025_summaries") {
      df <- df |> get_v2025_history_df()
    } else if (curr_panel == "aa_summaries") {
      df <- df |> get_aa_history_df()
    }
    print("summary_history_df loaded")

    return(df)
  })

  # summary_history_df <- reactive({
  #   curr_panel <- input$summary_panels
  #
  #   if (curr_panel == "v2025_summaries") {
  #     req(input$selected_country_scope_ts)
  #   } else if (curr_panel == "aa_summaries") {
  #     req(input$selected_country_scope_aa)
  #   }
  #
  #   req(input$selected_countries)
  #
  #   # print(paste0("input$summary_panels: ", input$summary_panels))
  #
  #   ds_name <-  "pb_main_ds"
  #
  #   selected_countries <- input$selected_countries
  #   # selected_countries <- c("NG", "ET", "TZ", "GH", "UG", "SD", "SS",
  #   #                         "AO", "MG", "LR", "KE", "BW", "MZ", "NA")
  #
  #
  #   req(curr_panel)
  #   if (curr_panel == "v2025_summaries") {
  #     country_scope <- input$selected_country_scope_ts
  #     first_snapshot <- input$selected_first_snapshot_ts
  #     last_snapshot <- input$selected_last_snapshot_ts
  #   } else if (curr_panel == "aa_summaries") {
  #     country_scope <- input$selected_country_scope_aa
  #     first_snapshot <- input$selected_first_snapshot_aa
  #     last_snapshot <- input$selected_last_snapshot_aa
  #   } else if (curr_panel == "distribution") {
  #     country_scope <- "Selected only"
  #     first_snapshot <- min(snapshot_dates_list)
  #     last_snapshot <- max(snapshot_dates_list)
  #   }
  #
  #   # first_snapshot <- NULL
  #   # last_snapshot <- NULL
  #   # first_snapshot <- input$first_snapshot
  #   # last_snapshot <- input$last_snapshot
  #
  #   source <- paste0("data/datasets/", ds_name)
  #   ds <- arrow::open_dataset(source, format = "parquet")
  #
  #   df <- ds %>%
  #     (function(x) {
  #       if(country_scope == "Selected only") {
  #         x <- x %>% filter(`Country Code` %in% selected_countries)
  #       }
  #       if(!is.null(first_snapshot)) {
  #         x <- x %>% filter(SnapshotDate >= first_snapshot & SnapshotDate <= last_snapshot)
  #       }
  #       return(x)
  #     }) %>%
  #     dplyr::collect()
  #
  #
  #   if (curr_panel == "v2025_summaries") {
  #     df <- df |> get_v2025_history_df()
  #   } else if (curr_panel == "aa_summaries") {
  #     df <- df |> get_aa_history_df()
  #   }
  #
  #   return(df)
  # })

  output$table_trans_status_history <- renderReactable({

    v2025 <- summary_history_df()
    # Calculate years summary
    years_df <- v2025 %>%
      group_by(year) %>%
      summarize(`Net starts this year` = sum(`Net starts this month`, na.rm = TRUE)) %>%
      arrange(desc(year))

    # Create the reactable
    reactable(
      data = years_df,
      columns = list(
        year = colDef(name = "Year", style = "font-weight: bold;", align = "left"),
        `Net starts this year` = colDef(align = "center")
      ),
      details = function(index) {
        index_year <- years_df$year[index]

        year_details <- v2025 %>%
          filter(year == index_year) %>%
          select(SnapshotDate, month, Yes, `Net starts this month`, Removed, Added,
                 RemainingNeeds_Langs, Removed_Langs, Added_Langs)

        reactable(
          data = year_details,
          defaultPageSize = 12,
          columns = list(
            SnapshotDate = colDef(show = FALSE),
            month = colDef(name = "Month",
                           align = "left",
                           cell = function(value) {
                             month.name[value]
                           }),
            # month = colDef(name = "Month"),
            Yes = colDef(
              name = "Remaining Needs",
              align = "center"
              ,cell = function(value, index) {
                snap_date <- year_details$SnapshotDate[[index]]
                langs <- year_details$RemainingNeeds_Langs[[index]]
                htmltools::div(
                  style = "cursor: pointer; color: blue; text_decoration-line: underline;",
                  id = paste0(snap_date,"_Yes"),
                  onclick='setinput2(this.id);',
                  value
                )
              }
            ),
            `Net starts this month` = colDef(align = "center"),
            Removed = colDef(
              name = "Newly removed",
              align = "center"
              ,cell = function(value, index) {
                snap_date <- year_details$SnapshotDate[[index]]
                langs <- year_details$Removed_Langs[[index]]
                htmltools::div(
                  style = "cursor: pointer; color: blue; text_decoration-line: underline;",
                  id = paste0(snap_date,"_Removed"),
                  onclick='setinput2(this.id);',
                  value
                )
              }
            ),
            Added = colDef(
              name = "Newly added",
              align = "center"
              ,cell = function(value, index) {
                snap_date <- year_details$SnapshotDate[[index]]
                langs <- year_details$Added_Langs[[index]]
                htmltools::div(
                  style = "cursor: pointer; color: blue; text_decoration-line: underline;",
                  id = paste0(snap_date,"_Added"),
                  onclick='setinput2(this.id);',
                   value
                )
              }
            )
            ,RemainingNeeds_Langs = colDef(show = FALSE),
            Removed_Langs = colDef(show = FALSE),
            Added_Langs = colDef(show = FALSE)
          ),
          defaultExpanded = TRUE
        )
      }
    )
  })

  # output$remaining_needs_plot <- renderPlot({
  #   # Ensure summary_history_df is reactive or available in the server environment
  #   graph_data <- summary_history_df() %>%
  #     select(SnapshotDate, Yes) %>%
  #     mutate(SnapshotDate = as.Date(SnapshotDate))
  #
  #   ggplot(graph_data, aes(x = SnapshotDate, y = Yes)) +
  #     geom_line(color = "blue") +
  #     geom_point(color = "blue") +
  #     theme_minimal() +
  #     labs(title = "Remaining Needs Over Time",
  #          x = "Date",
  #          y = "Remaining Needs") +
  #     scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # })

  output$remaining_needs_plot <- renderPlot({
    # Ensure summary_history_df is reactive or available in the server environment
    graph_data <- summary_history_df() %>%
      select(SnapshotDate, Yes) %>%
      mutate(SnapshotDate = as.Date(SnapshotDate),
             Days = as.numeric(SnapshotDate - min(SnapshotDate)))

    # Apply smoothing to the historical data using custom function
    graph_data <- graph_data %>%
      mutate(Smooth_Yes = rollmean_custom(Yes, k = 3))

    # Get the latest date and value
    latest_date <- max(graph_data$SnapshotDate)
    latest_value <- graph_data$Yes[graph_data$SnapshotDate == latest_date]

    # Calculate the target date
    target_date <- as.Date("2025-12-31")
    # target_date <- as.Date("2026-01-01")

    # Create data for the goal trend line
    days_to_target <- as.numeric(target_date - latest_date)
    daily_reduction <- latest_value / days_to_target
    monthly_reduction <- (daily_reduction * 30) |> as.integer()

    goal_trend <- data.frame(
      SnapshotDate = seq(latest_date, target_date, by = "day"),
      Yes = seq(latest_value, 0, by = -daily_reduction)
    )

    goal_trend <- goal_trend[goal_trend$Yes >= 0, ]
# print(head(goal_trend))
    # Calculate the projected trend using polynomial regression (degree 2)
    # poly_model <- lm(Yes ~ poly(Days, 2, raw = TRUE), data = graph_data)

    # Handle outliers
    Q1 <- quantile(graph_data$Yes, 0.25, na.rm = TRUE)
    Q3 <- quantile(graph_data$Yes, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    graph_data <- graph_data %>%
      mutate(Yes_clean = ifelse(Yes < lower_bound | Yes > upper_bound, Smooth_Yes, Yes))

    # Use a more flexible regression model (GAM)
    gam_model <- gam(Yes ~ s(Days, k = 10), data = graph_data, method = "REML")
    # gam_model <- gam(Yes_clean ~ s(Days, k = 10), data = graph_data, method = "REML")

    # Evaluate the GAM model
    summary_gam <- summary(gam_model)
    dev_explained <- summary_gam$dev.expl
    r_sq <- summary_gam$r.sq
    gcv <- summary_gam$sp.criterion

    # Print model evaluation metrics
    # print(paste("Deviance explained:", round(dev_explained, 4)))
    # print(paste("R-squared:", round(r_sq, 4)))
    # print(paste("GCV score:", round(gcv, 4)))

    # Project the trend
    future_days <- seq(max(graph_data$Days), max(graph_data$Days) + 365*3, by = 1)  # Project 3 years into the future
    projected_trend <- data.frame(
      SnapshotDate = min(graph_data$SnapshotDate) + days(future_days),
      Yes = predict(gam_model, newdata = data.frame(Days = future_days))
    )

# print(head(projected_trend))

    projected_trend <- projected_trend[projected_trend$SnapshotDate >= latest_date, ]
    projected_trend$Yes <- pmax(projected_trend$Yes, 0)
    projected_trend <- projected_trend[projected_trend$Yes > 0, ]

    # Determine the maximum date for x-axis
    max_date <- max(target_date, max(projected_trend$SnapshotDate))

    # Create the plot
    p <- ggplot() +
      geom_line(data = graph_data, aes(x = SnapshotDate, y = Yes_clean), color = "blue", linewidth = 1) +
      geom_point(data = graph_data, aes(x = SnapshotDate, y = Yes_clean), color = "blue", size = 2) +
      # geom_line(data = graph_data, aes(x = SnapshotDate, y = Yes), color = "blue", linewidth = 1) +
      # geom_point(data = graph_data, aes(x = SnapshotDate, y = Yes), color = "blue", size = 2) +
      geom_line(data = goal_trend, aes(x = SnapshotDate, y = Yes), color = "purple", linetype = "dashed", linewidth = 1) +
      geom_line(data = projected_trend, aes(x = SnapshotDate, y = Yes), color = "#006400", linetype = "dashed", linewidth = 1) +
      theme_minimal() +
      labs(title = "Remaining Needs Over Time with Projections",
           x = "Date",
           y = "Remaining Needs") +
      scale_x_date(date_breaks = "6 months", date_labels = "%b %Y", limits = c(min(graph_data$SnapshotDate), max_date)) +
      scale_y_continuous(limits = c(0, max(graph_data$Yes, na.rm = TRUE) * 1.2)) +  # Increased y-axis limit
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.margin = unit(c(1, 1, 1, 0.5), "cm"))  # Top, right, bottom, left margins

    # Add annotations with improved positioning
    max_y <- max(graph_data$Yes, na.rm = TRUE) * 1.1
    # max_y <- max(graph_data$Yes, na.rm = TRUE)
    annotation_y_positions <- seq(max_y * .9, max_y * .7, length.out = 4)
    # annotation_y_positions <- seq(max_y * 1.18, max_y * 1.06, length.out = 4)

    annotations <- data.frame(
      x = max_date,
      y = annotation_y_positions,
      label = c(
        paste("Goal Target Date: Jan 1, 2026"),
        paste("Latest Actual Data:", format(latest_date, "%b %d, %Y"), "-", latest_value),
        paste("Pace required to reach V2025:", monthly_reduction, "new starts each month"),
        # paste("Goal ends:", format(max(goal_trend$SnapshotDate), "%b %d, %Y")),
        paste("Projected Data ends:", format(max(projected_trend$SnapshotDate), "%b %d, %Y"))
      ),
      color = c("black", "black", "purple", "#006400")
    )

    p <- p +
      geom_text(data = annotations, aes(x = x, y = y, label = label, color = color), hjust = 1, size = 4.0) +
      # geom_text(data = annotations, aes(x = x, y = y, label = label, color = color), hjust = 1, size = 3.5) +
      scale_color_identity()

    # Add legend elements
    legend_y_positions <- seq(max_y * 0.25, max_y * 0.1, length.out = 3)
    # legend_y_positions <- seq(max_y * 0.15, max_y * 0.05, length.out = 3)
    legend_x_position <- min(graph_data$SnapshotDate) + days(30)  # Adjust as needed

    legend_data <- data.frame(
      x = legend_x_position,
      y = legend_y_positions,
      label = c("Past Data", "Trend line required to reach V2025", "Projected trend to no starts remaining"),
      color = c("blue", "purple", "#006400")
    )

    p <- p +
      geom_text(data = legend_data, aes(x = x, y = y, label = label, color = color), hjust = 0, size = 4.0) +
      # geom_text(data = legend_data, aes(x = x, y = y, label = label, color = color), hjust = 0, size = 3.5) +
      scale_color_identity()

    # Print diagnostic information
    # print(paste("Goal end date:", max(goal_trend$SnapshotDate)))
    # print(paste("Projected Data end date:", max(projected_trend$SnapshotDate)))

    # Add model evaluation metrics to the plot
    annotation_metrics <- data.frame(
      x = min(graph_data$SnapshotDate),
      y = max(graph_data$Yes, na.rm = TRUE) * 1.1,
      label = sprintf("Model (GAM) Fit: Dev. Expl. = %.2f%%, R-sq = %.2f, GCV = %.2f", dev_explained * 100, r_sq, gcv)
    )

    p <- p +
      geom_text(data = annotation_metrics, aes(x = x, y = y, label = label),
                hjust = 0, vjust = 1, size = 3.5, color = "darkgray")

    # Add model evaluation metrics to the plot
    annotation_metrics <- data.frame(
      x = min(graph_data$SnapshotDate),
      y = max(graph_data$Yes, na.rm = TRUE) * 1.1,
      label = sprintf("Model Fit: Dev. Expl. = %.2f%%, R-sq = %.2f, GCV = %.2f", dev_explained * 100, r_sq, gcv)
    )

    p <- p +
      geom_text(data = annotation_metrics, aes(x = x, y = y, label = label),
                hjust = 0, vjust = 1, size = 4.0, color = "darkgray")
                # hjust = 0, vjust = 1, size = 3.5, color = "darkgray")

    p
  })

  # Create specific modal functions

  # hist_list_modal <- create_history_modal(
  #   history_df = v2025_history_df,
  #   main_rows = main_rows,
  #   modal_title = "Translation Status History (ProgressBible)",
  #   snapshot_field = "RemainingNeeds",
  #   values = values
  # )
  #
  # shiny::observe({
  #   shiny::showModal(hist_list_modal(input$clicked_hist_id))
  # }) %>% shiny::bindEvent(input$clicked_hist_id)
  #
  # create_close_modal_observer("btn_send_to_RT_RemainingNeeds", "Research Table Builder")

  # close_modal_observer_RemainingNeeds <- create_close_modal_observer("btn_send_to_RT_RemainingNeeds", "Research Table Builder")
  # close_modal_observer_RemainingNeeds(input, output, session)

  observe({
    hist_id <- input$clicked_hist_id
    showModal(hist_list_modal(hist_id))
  }) |> bindEvent(input$clicked_hist_id)

  hist_list_modal <- function(hist_id) {
    md <- modalDialog(
      {
        id_parts <- str_split_1(hist_id, "_")
        snap_date <- id_parts[1]
        f <- id_parts[2]
        field <- case_when(
                  f == "Yes" ~ "RemainingNeeds_Langs",
                  f == "Removed" ~ "Removed_Langs",
                  f == "Added" ~ "Added_Langs")
        label <- case_when(
                  f == "Yes" ~ "All remaining",
                  f == "Removed" ~ "Newly removed",
                  f == "Added" ~ "Newly added")
        lang_codes <- summary_history_df() |>
          filter(SnapshotDate == snap_date) |>
          pull(field) |> str_split_1(", ")
        values$hist_langs <- lang_codes
        data <- main_rows |>
          filter(`Language Code` %in% lang_codes) |>
          select(Area, Country, `Language Name`, `Translation Status`, `Is Sign Language`, `Org Engagements (WIP)`)
          # select(Area, Country, `Language Name`, `Translation Status`, `Prior Trans Status`,  `Is Sign Language`, `Org Engagements (WIP)`)
        caption_text <- paste0(label, " V2025 languages, as reported in the ProgressBible Snapshot, dated ", snap_date," (Current status shown).")
        tabsetPanel(
          header = tagList(tags$h4(caption_text)),
          tabPanel(
            title = "Table",
            renderDT(
              datatable(data,
                        escape = FALSE,
                        # caption = HTML(paste0("<h4>", caption_text, "</h4>")),
                        options = list(
                          scrollY = '60vh',
                          # scrollX = 'true',
                          # scrollX = 'false',
                          scrollcollapse = TRUE,
                          # columnDefs = list(list(visible = FALSE, targets = 1)),
                          dom = 'Blfriti',
                          buttons = c('copy', 'csv'),
                          lengthMenu = list(c(-1, 10, 25, 50), c("All", 10, 25, 50))
                        )
              )
            ),
            # actionButton("btn_send_to_RT", "Send list to Research Table"),
            uiOutput("btn_send2RT")
          ),
          tabPanel(
            title = "Map",
            leafletOutput("map_ts_hist_modal")
          )
        )
      },
      title = "Translation Status History (ProgressBible)",
      footer = modalButton("Dismiss"),
      size = "l",
      # size = c("m", "s", "l", "xl"),
      easyClose = FALSE,
      fade = TRUE
    )
    return(md)
  }

  output$btn_send2RT <- renderUI({
    actionButton("btn_send_to_RT",
                 label = "Send list to Research Table",
                 icon = icon("share"))
  })

  observe({
    removeModal()
    values$val_db_search <- FALSE
    values$val_hist_search <- TRUE
    values$val_iso_search <- FALSE
    updateTabsetPanel(session, "main_page_tabset", selected = "table_builder")
  }) |> bindEvent(input$btn_send_to_RT)

  ########################################################################
  # Morris Johnson - 2024-09-23
  # Translation status history map
  ########################################################################

  # Reactive function to get the data
  map_data_ts_hist_modal <- reactive({
    # Replace this with your actual data loading logic
    req(values$hist_langs)
    lang_codes <- values$hist_langs |> isolate()
    df <- maps_table |>
      filter(`Language Code` %in% lang_codes)
    # str(df)
    return(df)
  })

  # Reactive function to generate status colors
  status_colors_ts_hist_modal <- reactive({
    req(map_data_ts_hist_modal())
    statuses <- unique(map_data_ts_hist_modal()$`Translation Status`)
    n <- length(statuses)

    #** Print debugging information
    # print(paste("3614: Number of unique statuses:", n))
    # print("Unique statuses:")
    # print(statuses)

    if (n == 0) {
      return(NULL)  # Return NULL if there are no statuses
    } else if (n == 1) {
      colors <- "#E41A1C"  # Use a single color if there's only one status
    } else if (n <= 8) {
      # colors <- brewer.pal(max(3, n), "Accent")[1:n]
      colors <- brewer.pal(max(3, n), "Set1")[1:n]
      # colors <- brewer.pal(max(3, n), "Set2")[1:n]
    } else {
      colors <- hue_pal(l = 65)(n)
    }

    result <- setNames(colors, statuses)

    #** Print the result for debugging
    # print("Generated color mapping:")
    # print(result)

    return(result)
  })

  # Create a reactive color function
  color_func_ts_hist_modal <- reactive({
    colorFactor(status_colors_ts_hist_modal(),
                domain = names(status_colors_ts_hist_modal()),
                reverse = TRUE)
  })

  # Reactive function to calculate view settings


  view_settings_ts_hist_modal <- reactive({
    req(map_data_ts_hist_modal())
    data <- map_data_ts_hist_modal()

    lat <- data$Latitude
    lon <- data$Longitude

    lat_range <- max(lat, na.rm = TRUE) - min(lat, na.rm = TRUE)
    lon_range <- max(lon, na.rm = TRUE) - min(lon, na.rm = TRUE)

    # print(paste0("lat_range: ", lat_range, ", lon_range: ", lon_range))

    # Calculate zoom based on the larger of latitude or longitude range
    zoom <- 8 - log2(max(lat_range, lon_range))

    # print(paste0("initial zoom: ", zoom))

    # Limit zoom to a reasonable range
    zoom <- max(min(zoom, 10), 4)  # Minimum zoom 5, maximum zoom 10

    # print(paste0("final zoom: ", zoom))

    list(
      lng = mean(lon, na.rm = TRUE),
      lat = mean(lat, na.rm = TRUE),
      zoom = zoom
    )
  })

  # Create the leaflet map
  output$map_ts_hist_modal <- renderLeaflet({
    req(map_data_ts_hist_modal(),
        color_func_ts_hist_modal(),
        view_settings_ts_hist_modal())

    view <- view_settings_ts_hist_modal()
    colors <- status_colors_ts_hist_modal()
    req(colors)

    leaflet(map_data_ts_hist_modal()) %>%
      addTiles(group = "Base features") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Show topography") %>%
      addCircleMarkers(
        ~Longitude, ~Latitude,
        popup = ~html_popup_text,
        label = ~`Language Name`,
        color = ~color_func_ts_hist_modal()(`Translation Status`),
        radius = 4,
        stroke = FALSE,
        fillOpacity = 1.0,
        # fillOpacity = 0.8,
        group = "Language Markers"
      ) %>%
      addLabelOnlyMarkers(
        ~Longitude, ~Latitude,
        label = ~`Language Name`,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "right",
          offset = c(10, 0),
          textOnly = TRUE,
          style = list(
            "color" = "blue",
            "font-weight" = "normal",
            "font-size" = "10px",
            "border-color" = "rgba(0,0,0,0.2)",
            "border-radius" = "4px",
            "padding" = "4px"
          )
        ),
        group = "Language Names"
      ) %>%
      addLayersControl(
        baseGroups = c("Base features", "Show topography"),
        overlayGroups = c("Language Markers", "Language Names"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addLegend(
        position = "bottomright",
        colors = unname(colors),
        labels = names(colors),
        title = "Translation Status",
        opacity = 1,
        layerId = "status_legend"
      ) |>
      setView(
        lng = view_settings_ts_hist_modal()$lng,
        lat = view_settings_ts_hist_modal()$lat,
        zoom = view_settings_ts_hist_modal()$zoom
      ) |>
      hideGroup("Language Names")
  })

  # observe({
  #   req()
  #   colors <- status_colors_ts_hist_modal()
  #   req(colors)
  #
  #   #** Print debugging information
  #   print("Colors for legend:")
  #   print(colors)
  #   print("Names (labels) for legend:")
  #   print(names(colors))
  #
  #   tryCatch({
  #     leafletProxy("map_ts_hist_modal") %>%
  #       clearControls() %>%
  #       addLegend(
  #         position = "bottomright",
  #         colors = unname(colors),
  #         labels = names(colors),
  #         title = "Translation Status"
  #       )
  #     print("Legend added successfully")
  #   }, error = function(e) {
  #     print(paste("Error adding legend:", e$message))
  #   })
  # }) |> bindEvent(status_colors_ts_hist_modal())

  # Observe changes in view_settings and update the view
  observe({
    req(view_settings_ts_hist_modal())
    leafletProxy("map_ts_hist_modal") %>%
      setView(
        lng = view_settings_ts_hist_modal()$lng,
        lat = view_settings_ts_hist_modal()$lat,
        zoom = view_settings_ts_hist_modal()$zoom
      )
  })

  ########################################################################
  # Morris Johnson - 2024-08-05
  # All Access status history table
  ########################################################################


  output$table_aa_status_history <- renderReactable({

    aa_goals <- summary_history_df()

    req(aa_goals)

    # Calculate years summary
    years_df <- aa_goals %>%
      group_by(year) %>%
      summarize(`Net goals met this year` = sum(`Net goals met this month`, na.rm = TRUE) |> abs()) %>%
      arrange(desc(year))

    # Create the reactable
    reactable(
      data = years_df,
      columns = list(
        year = colDef(name = "Year", style = "font-weight: bold;", align = "left"),
        `Net goals met this year` = colDef(align = "center")
      ),
      details = function(index) {
        index_year <- years_df$year[index]

        year_details <- aa_goals %>%
          filter(year == index_year) %>%
          select(SnapshotDate, month, Yes, `No/other`,  `Net goals met this month`, Removed, Added,
                 GoalMet_Langs, GoalNotMet_Langs, Removed_Langs, Added_Langs)
                 # GoalMet_Langs, Removed_Langs, Added_Langs)
          # select(SnapshotDate, month, Yes, `Net goals met this month`, Removed, Added,
          #        GoalMet_Langs, Removed_Langs, Added_Langs)

        reactable(
          data = year_details,
          defaultPageSize = 12,
          columns = list(
            SnapshotDate = colDef(show = FALSE),
            month = colDef(name = "Month",
                           align = "left",
                           cell = function(value) {
                             month.name[value]
                           }),
            # month = colDef(name = "Month"),
            Yes = colDef(
              name = "Goals met, running total",
              align = "center"
              ,cell = function(value, index) {
                snap_date <- year_details$SnapshotDate[[index]]
                langs <- year_details$GoalMet_Langs[[index]]
                htmltools::div(
                  style = "cursor: pointer; color: blue; text_decoration-line: underline;",
                  id = paste0(snap_date,"_Yes"),
                  onclick='setinput3(this.id);',
                  value
                )
              }
            ),
            `No/other` = colDef(
              name = "Goals not met, running total",
              align = "center"
              ,cell = function(value, index) {
                snap_date <- year_details$SnapshotDate[[index]]
                langs <- year_details$GoalNotMet_Langs[[index]]
                htmltools::div(
                  style = "cursor: pointer; color: blue; text_decoration-line: underline;",
                  id = paste0(snap_date,"_No/other"),
                  onclick='setinput3(this.id);',
                  value
                )
              }
            ),
            `Net goals met this month` = colDef(align = "center"),
            Added = colDef(
              name = "Newly 'Goal Met'",
              align = "center"
              ,cell = function(value, index) {
                snap_date <- year_details$SnapshotDate[[index]]
                langs <- year_details$Added_Langs[[index]]
                htmltools::div(
                  style = "cursor: pointer; color: blue; text_decoration-line: underline;",
                  id = paste0(snap_date,"_Added"),
                  onclick='setinput3(this.id);',
                  value
                )
              }
            ),
            Removed = colDef(
              name = "Newly 'Goal Not Met'",
              align = "center"
              ,cell = function(value, index) {
                snap_date <- year_details$SnapshotDate[[index]]
                langs <- year_details$Removed_Langs[[index]]
                htmltools::div(
                  style = "cursor: pointer; color: blue; text_decoration-line: underline;",
                  id = paste0(snap_date,"_Removed"),
                  onclick='setinput3(this.id);',
                  value
                )
              }
            ),
            GoalMet_Langs = colDef(show = FALSE),
            GoalNotMet_Langs = colDef(show = FALSE),
            Removed_Langs = colDef(show = FALSE),
            Added_Langs = colDef(show = FALSE)
          ),
          defaultExpanded = TRUE
        )
      }
    )
  })

  output$goals_met_line_plot <- renderPlot({
    # Ensure v2025_history_df is reactive or available in the server environment
    graph_data <- summary_history_df() %>%
      select(SnapshotDate, Yes) %>%
      mutate(SnapshotDate = as.Date(SnapshotDate))

    ggplot(graph_data, aes(x = SnapshotDate, y = Yes)) +
      geom_line(color = "blue") +
      geom_point(color = "blue") +
      theme_minimal() +
      labs(title = "Goals Met Over Time",
           x = "Date",
           y = "Goals Met") +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$goals_not_met_line_plot <- renderPlot({
    req(input$aa_subs == "All Access Status History")

    graph_data <- summary_history_df() %>%
      select(SnapshotDate, `No/other`) %>%
      rename(No = `No/other`) %>%
      mutate(SnapshotDate = as.Date(SnapshotDate),
             Days = as.numeric(SnapshotDate - min(SnapshotDate)))

    # Apply smoothing to the historical data using custom function
    graph_data <- graph_data %>%
      mutate(Smooth_No = rollmean_custom(No, k = 3))

    # Get the latest date and value
    latest_date <- max(graph_data$SnapshotDate)
    latest_value <- graph_data$No[graph_data$SnapshotDate == latest_date]

    # Calculate the target date (December 31, 2033)
    target_date <- as.Date("2033-12-31")

    # Create data for the goal trend line
    days_to_target <- as.numeric(target_date - latest_date)
    daily_reduction <- latest_value / days_to_target
    monthly_reduction <- (daily_reduction * 30) %>% as.integer()

    goal_trend <- data.frame(
      SnapshotDate = seq(latest_date, target_date, by = "day"),
      No = seq(latest_value, 0, by = -daily_reduction)
    )

    goal_trend <- goal_trend[goal_trend$No >= 0, ]

    # Handle outliers
    Q1 <- quantile(graph_data$No, 0.25, na.rm = TRUE)
    Q3 <- quantile(graph_data$No, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    graph_data <- graph_data %>%
      mutate(No_clean = ifelse(No < lower_bound | No > upper_bound, Smooth_No, No))

    # Use a more flexible regression model (GAM)
    gam_model <- gam(No_clean ~ s(Days, k = 10), data = graph_data, method = "REML")

    # Evaluate the GAM model
    summary_gam <- summary(gam_model)
    dev_explained <- summary_gam$dev.expl
    r_sq <- summary_gam$r.sq
    gcv <- summary_gam$sp.criterion

    # Print model evaluation metrics
    # print(paste("Deviance explained:", round(dev_explained, 4)))
    # print(paste("R-squared:", round(r_sq, 4)))
    # print(paste("GCV score:", round(gcv, 4)))

    # Project the trend
    future_days <- seq(max(graph_data$Days), max(graph_data$Days) + 365*10, by = 1)
    projected_trend <- data.frame(
      SnapshotDate = min(graph_data$SnapshotDate) + days(future_days),
      No = predict(gam_model, newdata = data.frame(Days = future_days))
    )

    projected_trend <- projected_trend[projected_trend$SnapshotDate >= latest_date, ]
    projected_trend$No <- pmax(projected_trend$No, 0)
    projected_trend <- projected_trend[projected_trend$No > 0, ]

    # Determine the maximum date for x-axis
    max_date <- max(target_date, max(projected_trend$SnapshotDate))

    # Create the plot
    p <- ggplot() +
      geom_line(data = graph_data, aes(x = SnapshotDate, y = No_clean), color = "blue", linewidth = 1) +
      geom_point(data = graph_data, aes(x = SnapshotDate, y = No_clean), color = "blue", size = 2) +
      geom_line(data = goal_trend, aes(x = SnapshotDate, y = No), color = "purple", linetype = "dashed", linewidth = 1) +
      geom_line(data = projected_trend, aes(x = SnapshotDate, y = No), color = "#006400", linetype = "dashed", linewidth = 1) +
      theme_minimal() +
      labs(title = "Goals Unmet Over Time with Pace Projections",
           x = "Date",
           y = "Goals Unmet") +
      scale_x_date(date_breaks = "6 months", date_labels = "%b %Y", limits = c(min(graph_data$SnapshotDate), max_date)) +
      scale_y_continuous(limits = c(0, max(graph_data$No, na.rm = TRUE) * 1.2)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.margin = unit(c(1, 1, 1, 0.5), "cm"))

    # Add annotations with improved positioning
    max_y = max(graph_data$No, na.rm = TRUE) * 1.1
    # max_y <- max(graph_data$`No`, na.rm = TRUE)
    annotation_y_positions <- seq(max_y * .95, max_y * .68, length.out = 4)
    # annotation_y_positions <- seq(max_y * .95, max_y * .65, length.out = 5)
    # annotation_y_positions <- seq(max_y * .9, max_y * .7, length.out = 4)
    # annotation_y_positions <- seq(max_y * 1.18, max_y * 1.06, length.out = 4)

    annotations <- data.frame(
      x = max_date,
      y = annotation_y_positions,
      label = c(
        paste("Completion Target Date: Dec 31, 2033"),
        paste("Latest Actual Data:", format(latest_date, "%b %d, %Y"), "-", latest_value, " goals unmet"),
        # paste("Target completion:", format(max(goal_trend$SnapshotDate), "%b %d, %Y")),
        # paste("Monthly pace required to reach target:", monthly_pace),
        paste("Pace required to reach 2033:", monthly_reduction, "goal(s) met each month"),
        paste("Projected Data ends:", format(max(projected_trend$SnapshotDate), "%b %d, %Y"))
      ),
      color = c("black", "black", "purple", "#006400")
      # color = c("black", "black", "purple", "purple", "#006400")
    )

    p <- p +
      geom_text(data = annotations, aes(x = x, y = y, label = label, color = color), hjust = 1, size = 4.0) +
      # geom_text(data = annotations, aes(x = x, y = y, label = label, color = color), hjust = 1, size = 3.5) +
      scale_color_identity()

    # Add legend elements
    legend_y_positions <- seq(max_y * 0.25, max_y * 0.1, length.out = 3)
    # legend_y_positions <- seq(max_y * 0.15, max_y * 0.05, length.out = 3)
    legend_x_position <- min(graph_data$SnapshotDate) + days(30)  # Adjust as needed

    legend_data <- data.frame(
      x = legend_x_position,
      y = legend_y_positions,
      label = c("Past Data", "Trend line required to 2033", "Projected trend to all goals met"),
      color = c("blue", "purple", "#006400")
    )

    p <- p +
      geom_text(data = legend_data, aes(x = x, y = y, label = label, color = color), hjust = 0, size = 4.0) +
      # geom_text(data = legend_data, aes(x = x, y = y, label = label, color = color), hjust = 0, size = 3.5) +
      scale_color_identity()

    # Add model evaluation metrics to the plot
    annotation_metrics <- data.frame(
      x = min(graph_data$SnapshotDate),
      y = max(graph_data$No, na.rm = TRUE) * 1.1,
      label = sprintf("Model (GAM) Fit: Dev. Expl. = %.2f%%, R-sq = %.2f, GCV = %.2f", dev_explained * 100, r_sq, gcv)
    )

    p <- p +
      geom_text(data = annotation_metrics, aes(x = x, y = y, label = label),
                hjust = 0, vjust = 1, size = 3.5, color = "darkgray")
    p


  })

  output$goals_unmet_vs_met_plot <- renderPlot({
    aa_goals <- summary_history_df() |>
      select(year, Yes, `No/other`)
    plot <- create_dodged_bar_chart(aa_goals)
    plot  # This line returns the plot object
  })

  create_dodged_bar_chart <- function(data) {
    # Prepare the data
    plot_data <- data %>%
      group_by(year) %>%
      summarise(
        Yes = max(Yes, na.rm = TRUE),
        `No/other` = min(`No/other`, na.rm = TRUE)
        # `No/other` = abs(n() - max(Yes, na.rm = TRUE))
      ) %>%
      tidyr::pivot_longer(cols = c(Yes, `No/other`), names_to = "Status", values_to = "Count")

    # Create the plot
    ggplot(plot_data, aes(x = factor(year), y = Count, fill = Status)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
      geom_text(aes(label = Count), position = position_dodge(width = 0.9),
                vjust = -0.5, size = 3) +
      scale_fill_manual(values = c("Yes" = "#4CAF50", "No/other" = "#F44336")) +
      labs(
        title = "Comparison of Goals Met 'Yes' (max) vs 'No/other' (min), by Year",
        x = "Year",
        y = "Count",
        fill = "Status"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      )
  }

  # # Create specific modal functions
  # aa_hist_list_modal <- create_history_modal(
  #   history_df = aa_history_df,
  #   main_rows = main_rows,
  #   modal_title = "All Access Status History (ProgressBible)",
  #   snapshot_field = "GoalMet",
  #   values = values
  # )
  #
  #
  # # Observers for showing modals
  # shiny::observe({
  #   shiny::showModal(aa_hist_list_modal(input$clicked_aa_hist_id))
  # }) %>% shiny::bindEvent(input$clicked_aa_hist_id)
  #
  # create_close_modal_observer("btn_send_to_RT_GoalMet", "Research Table Builder")

  # close_modal_observer_GoalMet <- create_close_modal_observer("btn_send_to_RT_GoalMet", "Research Table Builder")
  # close_modal_observer_GoalMet(input, output, session)

  observe({
    aa_hist_id <- input$clicked_aa_hist_id
    showModal(aa_hist_list_modal(aa_hist_id))
  }) |> bindEvent(input$clicked_aa_hist_id)

  aa_hist_list_modal <- function(aa_hist_id) {
    # print(aa_hist_id)
    md <- modalDialog(
      {
        id_parts <- str_split_1(aa_hist_id, "_")
        snap_date <- id_parts[1]
        f <- id_parts[2]
        field <- case_when(
                  f == "Yes" ~ "GoalMet_Langs",
                  f == "No/other" ~ "GoalNotMet_Langs",
                  f == "Removed" ~ "Removed_Langs",
                  f == "Added" ~ "Added_Langs")
        label <- case_when(
                  f == "Yes" ~ "Total",
                  f == "No/other" ~ "Total",
                  f == "Removed" ~ "Newly 'Goal Not Met'",
                  f == "Added" ~ "Newly 'Goal Met'")
        lang_codes <- summary_history_df() |>
        # lang_codes <- aa_history_df |>
          filter(SnapshotDate == snap_date) |>
          pull(field) |> str_split_1(", ")
          # pull(field) |> unlist()
        values$hist_langs <- lang_codes
        data <- main_rows |>
          filter(`Language Code` %in% lang_codes) |>
          select(Area, Country, `Language Name`, `All Access Status`, `Is Sign Language`, `Org Engagements (WIP)`)
          # select(Area, Country, `Language Name`, `All Access Status`, `Prior AA Status`, `Is Sign Language`, `Org Engagements (WIP)`)
        caption_text <- paste0(label, " languages, as reported in the ProgressBible Snapshot, dated ", snap_date," (Current status shown).")
        # caption_text <- paste0(label, " All Access Goals Met as reported in the ProgressBible Snapshot, dated ", snap_date," (Current status shown).")
        tabsetPanel(
          header = tagList(tags$h4(caption_text)),
          tabPanel(
            title = "Table",
            renderDT(
              datatable(data,
                        escape = FALSE,
                        # caption = HTML(paste0("<h4>", caption_text, "</h4>")),
                        options = list(
                          scrollY = '60vh',
                          # scrollX = 'true',
                          # scrollX = 'false',
                          scrollcollapse = TRUE,
                          # columnDefs = list(list(visible = FALSE, targets = 1)),
                          dom = 'Blfriti',
                          buttons = c('copy', 'csv'),
                          lengthMenu = list(c(-1, 10, 25, 50), c("All", 10, 25, 50))
                        )
              )
            ),
            actionButton("btn_aa_send_to_RT", "Send list to Research Table"),
          ),
          tabPanel(
            title = "Map",
            leafletOutput("map_aa_hist_modal")
          )
        )

      },
      title = "All Access Status History (ProgressBible)",
      footer = modalButton("Dismiss"),
      size = "l",
      # size = c("m", "s", "l", "xl"),
      easyClose = FALSE,
      fade = TRUE
    )
    return(md)
  }

  observe({
    removeModal()
    values$val_db_search <- FALSE
    values$val_hist_search <- TRUE
    values$val_iso_search <- FALSE
    updateTabsetPanel(session, "main_page_tabset", selected = "table_builder")
  }) |> bindEvent(input$btn_aa_send_to_RT)

  ########################################################################
  # Morris Johnson - 2024-09-24
  # All Access status history map
  ########################################################################

  # Reactive function to get the data
  map_data_aa_hist_modal <- reactive({
    # Replace this with your actual data loading logic
    req(values$hist_langs)
    lang_codes <- values$hist_langs |> isolate()
    df <- maps_table |>
      filter(`Language Code` %in% lang_codes)
    # str(df)
    return(df)
  })

  # Reactive function to generate status colors
  status_colors_aa_hist_modal <- reactive({
    req(map_data_aa_hist_modal())
    statuses <- unique(map_data_aa_hist_modal()$`All Access Status`)
    n <- length(statuses)

    #** Print debugging information
    # print(paste("Number of unique statuses:", n))
    # print("Here: Unique statuses:")
    # print(statuses)

    if (n == 0) {
      return(NULL)  # Return NULL if there are no statuses
    } else if (n == 1) {
      colors <- "#377EB8"  # Use a single color if there's only one status
    } else if (n <= 8) {
      colors <- brewer.pal(n = max(3, n), name = "Set1")[1:n]
      # colors <- brewer.pal(max(3, n), "Set2")[1:n]
    } else {
      colors <- hue_pal(l = 65)(n)
    }

    result <- setNames(colors, statuses)

    #** Print the result for debugging
    # print("Generated color mapping:")
    # print(result)

    return(result)
  })

  # Create a reactive color function
  color_func_aa_hist_modal <- reactive({
    colorFactor(status_colors_aa_hist_modal(),
                domain = names(status_colors_aa_hist_modal()))
  })

  # Reactive function to calculate view settings


  view_settings_aa_hist_modal <- reactive({
    req(map_data_aa_hist_modal())
    data <- map_data_aa_hist_modal()

    lat <- data$Latitude
    lon <- data$Longitude

    lat_range <- max(lat, na.rm = TRUE) - min(lat, na.rm = TRUE)
    lon_range <- max(lon, na.rm = TRUE) - min(lon, na.rm = TRUE)

    # print(paste0("lat_range: ", lat_range, ", lon_range: ", lon_range))

    # Calculate zoom based on the larger of latitude or longitude range
    zoom <- 8 - log2(max(lat_range, lon_range))

    # print(paste0("initial zoom: ", zoom))

    # Limit zoom to a reasonable range
    zoom <- max(min(zoom, 10), 4)  # Minimum zoom 5, maximum zoom 10

    # print(paste0("final zoom: ", zoom))

    list(
      lng = mean(lon, na.rm = TRUE),
      lat = mean(lat, na.rm = TRUE),
      zoom = zoom
    )
  })

  # Create the leaflet map
  output$map_aa_hist_modal <- renderLeaflet({
    req(map_data_aa_hist_modal(),
        color_func_aa_hist_modal(),
        view_settings_aa_hist_modal())

    view <- view_settings_aa_hist_modal()
    colors <- status_colors_aa_hist_modal()
    req(colors)

    leaflet(map_data_aa_hist_modal()) %>%
      addTiles(group = "Base features") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Show topography") %>%
      addCircleMarkers(
        ~Longitude, ~Latitude,
        popup = ~html_popup_text,
        label = ~`Language Name`,
        color = ~color_func_aa_hist_modal()(`All Access Status`),
        radius = 4,
        stroke = FALSE,
        fillOpacity = 1.0,
        # fillOpacity = 0.8,
        group = "Language Markers"
      ) %>%
      addLabelOnlyMarkers(
        ~Longitude, ~Latitude,
        label = ~`Language Name`,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "right",
          offset = c(10, 0),
          textOnly = TRUE,
          style = list(
            "color" = "blue",
            "font-weight" = "normal",
            "font-size" = "10px",
            "border-color" = "rgba(0,0,0,0.2)",
            "border-radius" = "4px",
            "padding" = "4px"
          )
        ),
        group = "Language Names"
      ) %>%
      addLayersControl(
        baseGroups = c("Base features", "Show topography"),
        overlayGroups = c("Language Markers", "Language Names"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addLegend(
        position = "bottomright",
        colors = unname(colors),
        labels = names(colors),
        title = "All Access Status",
        opacity = 1,
        layerId = "status_legend"
      ) |>
      setView(
        lng = view_settings_aa_hist_modal()$lng,
        lat = view_settings_aa_hist_modal()$lat,
        zoom = view_settings_aa_hist_modal()$zoom
      ) |>
      hideGroup("Language Names")
  })

  # observe({
  #   req()
  #   colors <- status_colors_aa_hist_modal()
  #   req(colors)
  #
  #   #** Print debugging information
  #   print("Here: Colors for legend:")
  #   print(colors)
  #   print("Names (labels) for legend:")
  #   print(names(colors))
  #
  #   tryCatch({
  #     leafletProxy("map_aa_hist_modal") %>%
  #       clearControls() %>%
  #       addLegend(
  #         position = "bottomright",
  #         colors = unname(colors),
  #         labels = names(colors),
  #         title = "All Access Status"
  #       )
  #     print("Legend added successfully")
  #   }, error = function(e) {
  #     print(paste("Error adding legend:", e$message))
  #   })
  # }) |> bindEvent(status_colors_aa_hist_modal())

  # Observe changes in view_settings and update the view
  observe({
    req(view_settings_aa_hist_modal())
    leafletProxy("map_aa_hist_modal") %>%
      setView(
        lng = view_settings_aa_hist_modal()$lng,
        lat = view_settings_aa_hist_modal()$lat,
        zoom = view_settings_aa_hist_modal()$zoom
      )
  })


      # *** table builder reactive and rendering functions ***


  # selected_subgroup <- reactive({
  #     # choice <- "Topic"
  #     if (is.null(input$selected_topic)) {
  #         choice <- default_topic
  #     } else {
  #         choice <- input$selected_topic
  #     }
  #     return(choice)
  # })

  # selected_fields <- reactive({
  #     # stored_field_selections <- values$stored_fields
  #     stored_field_selections <- values$stored_fields
  #     if (is_null(stored_field_selections)) {
  #       # fields <- stored_fields_default
  #       fields <- default_fields
  #     } else {
  #       fields <- stored_field_selections %>%
  #           filter(sub %in% input$selected_topic) %>%
  #           pull(fields) %>% unique()
  #     }
  #     return(fields)
  # })

  all_stored_fields <- reactive({
      stored_selected_fields <- values$stored_fields
      default_fields <- default_fields
      if (!is.null(stored_selected_fields)) {
          flds <- stored_selected_fields %>%
              unique()
          fields <- c("Country", "Language Name", flds)
          # fields <- c(default_fields, flds)
      } else {
        fields <- c("Country", "Language Name", default_fields)
      }
      return(fields)
  })

  output$select_topic <- renderUI({
      current_taxonomy <- "Topic"
      choices <- field_hierarchy %>%
                  filter(Topic != "Metadata",
                         Topic != "Minimum") %>%
                  pull(Topic) %>%
                    unique() %>%
                    sort()

      radioButtons(
          inputId = "selected_topic",
          label = "Select topic",
          choices = choices,
          # selected = default_topic,
          selected = get_cookie(
            cookie_name = "sel_topic",
            # missing = NULL,
            missing = default_topic,
            session = shiny::getDefaultReactiveDomain()
          ),
          inline = FALSE,
          # inline = TRUE,
          width = NULL
      )
  })

  observeEvent(input$selected_topic,
    {
      current_topic <- input$selected_topic
      stored_val <- get_cookie("sel_topic", missing = "")
      if (stored_val != current_topic) {
        set_cookie("sel_topic",
          cookie_value = current_topic)
      }

      stored_selected = get_cookie(
          cookie_name = paste0("sel_", current_topic),
          missing = default_fields,
          # missing = " @@ ",
          session = shiny::getDefaultReactiveDomain()) |>
        str_split_1("@@")

      updateCheckboxGroupInput(
        inputId = "selected_fields",
        choices = field_choices(),
        selected = stored_selected)
        # selected = get_cookie(
        #   cookie_name = paste0("sel_", input$selected_topic),
        #   missing = default_fields,
        #   # missing = " @@ ",
        #   session = shiny::getDefaultReactiveDomain()
        # ) |> str_split_1("@@"))

    # }, ignoreNULL = FALSE, ignoreInit = FALSE
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    # }, label = "input$selected_topic", ignoreInit = FALSE)
    # }, label = "input$selected_topic", ignoreNULL = TRUE, ignoreInit = FALSE)

  field_choices <- reactive({
    # req(input$selected_topic)
      selected_taxonomy <- "Topic"
      selected_topic <- input$selected_topic
      vctr <- field_hierarchy %>%
          filter(Source %in% main_sources) %>%
          filter(.data[[selected_taxonomy]] %in% selected_topic) %>%
          # filter(.data[[selected_taxonomy]] %in% selected_subgroup) %>%
          filter(field_name != "Language Code") %>%
          filter(field_name != "Language Name") %>%
          filter(field_name != "Country") %>%
          filter(str_detect(.$field_name, "(ELG)", negate = TRUE)) %>%
          pull(field_name) %>%
          unique() %>%
          sort()
      return(vctr)
  })


  output$select_fields <- renderUI({
    req(input$selected_topic)
#     table <- get_stored_fields_table()
# print("output$select_fields")
# str(table)
#     # sel_flds <- NULL
#     sel_flds <- table %>%
#       filter(topic == input$selected_topic) %>%
#       pull(fields) %>%
#       str_split(", ") %>%
#       unlist()
# str(sel_flds)
    # sel_flds <- table |>
    #   filter(topic == input$selected_topic) |>
    #   pull(fields) |>
    #   str_c(collapse = ", ") |>
    #   str_split_1(", ") |>
    #   stringi::stri_remove_empty()
    checkboxGroupInput(
      inputId = "selected_fields",
      label = "Select fields to include in table",
      choices = field_choices(),
      selected = NULL,
      # selected = get_stored_fields_list(),
      # selected = sel_flds,
      inline = FALSE,
      width = NULL)
  })

  observeEvent(input$selected_fields, {
      req(input$selected_topic)
      # req(input$selected_fields)

      current_val <- input$selected_fields
      cookie_val <- current_val |> str_c(collapse = "@@")
      cookie_name <- paste0("sel_", input$selected_topic)
      stored_val <- get_cookie(cookie_name, missing = "")
      if (stored_val != cookie_val) {
        set_cookie(cookie_name, cookie_val)
      }
    }, ignoreNULL=FALSE, ignoreInit = TRUE)



  observeEvent(input$select_all_fields,
    {
      # req(input$selected_fields)
      if(!input$select_all_fields %% 2) {
        updateCheckboxGroupInput(session, "selected_fields", selected = field_choices())
      } else {
        updateCheckboxGroupInput(session, "selected_fields", selected = character(0))
      }
    }
  )

  output$lnk_switch_categories <- renderUI({
    actionLink("switch_y", "Switch categories", icon = icon("right-left"))
  })

  # output$dashboard_chart <- renderPlot({
  # output$dashboard_chart <- renderPlotly({
  #   print("here")
  #   req(input$summary_panels == "distribution")
  #   # req(input$selected_countries)
  #   # req(input$selected_vb_view)
  #   switch_cats <- input$switch_y %% 2
  #   df <- dashboard_df()
  #   dashboard <- input$selected_db_view
  #   countries <- df$Country %>% unique() %>% sort() %>% str_c(collapse = ", ")
  #
  #   get_db_chart(df, dashboard, countries, switch_cats) %>%
  #     plotly::config(displayModeBar = FALSE) %>%
  #     plotly::event_register('plotly_click')
  # })

  output$dashboard_chart <- renderPlotly({
    # req(dashboard_df())
    req(input$summary_panels == "distribution")
    if(is.null(input$switch_y)) {
      switch_cats <- 0
    } else {
      switch_cats <- input$switch_y %% 2
    }

    values$val_switch_cats <- switch_cats

    df <- dashboard_df()
    dashboard <- input$selected_db_view
    # countries <- df$Country %>% unique() %>% sort() %>% str_c(collapse = ", ")

    p_plot <- get_db_chart(df, dashboard, switch_cats)
    # p_plot <- get_db_chart(df, dashboard, countries, switch_cats)
    values$curr_db_plot_df <- prepare_data(df, dashboard, switch_cats)

    # print(p_plot$x$source)

    p_plot

  })
  # }) %>% bindEvent(input$summary_panels, input$switch_y, input$selected_db_view)

  # observeEvent(event_data("plotly_click", source = "A"), {
  #   click_data <- event_data(event = "plotly_click", source = "A", priority = "event")
  #   # print(click_data)  # This will print the click data to the console for debugging
  #   # Add your click handling logic here
  # })
  # }, suspended = TRUE)

  output$download_v2025_data <- downloadHandler(
    filename = function() {
        paste0("Vision 2025 Summary - ", today(), ".csv")
    },
    content = function(file) {
      df <- v2025_summary_export() |>
        get_appended_download_df()
      readr::write_csv(
        df, file,
        col_names = FALSE,
        na = ""
      )
    }
  )

  output$download_aa_data <- downloadHandler(
    filename = function() {
        paste0("All Access Summary - ", today(), ".csv")
    },
    content = function(file) {
      df <- all_access_summary_export() |>
        get_appended_download_df()
      readr::write_csv(
        df, file,
        col_names = FALSE,
        na = ""
      )
    }
  )

  output$download_data <- downloadHandler(
    filename = function() {
      selected_tab <- input$summary_panels
      fn <- case_when(
        selected_tab == "Vision 2025 Summary (by org areas and countries)" ~ "Vision 2025 Summary.csv",
        selected_tab == "All Access Summary (by org areas and countries)" ~ "All Access Summary.csv")
      return(fn)
    },
    content = function(file) {
      selected_tab <- input$summary_panels
      data <- case_when(
        selected_tab == "Vision 2025 Summary (by org areas and countries)" ~ v2025_summary_export(),
        selected_tab == "All Access Summary (by org areas and countries)" ~ all_access_summary_export()
      )
      # str(data)
      write.csv(
        data, file, row.names = FALSE
      )
    }
  )

  output$V2025_summary_table <- renderReactable({
    req(input$selected_partner)
    req(input$selected_areas)
    req(input$selected_countries)

    # *** field selection ***

    if(is.null(values$V2025_summary_tbl_chosen_flds)) {
      df <- v2025_summary_df()
      choices <- names(df)
      values$V2025_summary_tbl_fld_choices <- choices[5:length(choices)]
    } else {
      df <- values$V2025_summary_tbl_data
    }

    # *** row selection ***

    if (length(input$selected_areas) == 0) {
      selected_areas <-  df$Area %>% unique()
    } else {
      selected_areas <-  input$selected_areas
    }

    if (length(input$selected_countries) == 0) {
      selected_countries = df$`Country Code` %>% unique()
    } else {
      selected_countries = input$selected_countries
    }

    # columns name simplified for JS() code
    names_to_simplify <- c(V2025_complete = "Vision 2025 Complete", one_remaining = "1 remaining",
                           two_to_5 = "2 to 5 remaining", six_to_20 = "6 to 20 remaining",
                           twentyone_plus = "21 or more remaining", v2025_not_complete = "Vision 2025 Not Complete")

    df <- df %>%
      filter(Area %in% selected_areas) %>%
      filter(`Country Code` %in% selected_countries) %>%
      rename(any_of(names_to_simplify)) %>%
      select(-`Country Code`)

# str(df)

    # values$summary_tbl_data <- df

    # *** colDef selection for table columns argument ***

    col_def_defaults <-  list(
      Area = colDef(grouped = JS("function(cellInfo) {return cellInfo.value + ' (' + cellInfo.subRows.length + ' countries)'}"),
                    footer = "<b>Totals</b>",
                    html = TRUE),
      Countries = colDef(name = "Countries",
                         html = TRUE,
                         footer = "(click country name to see Joshua Project entry)",
                         footerStyle = "font-size: 10px;"),
      V2025_complete = colDef(aggregated = JS("function(cellInfo) {
                                                let count = cellInfo.subRows.filter(val => val.V2025_complete === 'Yes').length
                                                return count}"),
                              footer = JS(
                                "function(cellInfo, state) {
                                  let tally = {};
                                  let group_total = {};
                                  tally = state.data.filter(val => val.V2025_complete === 'Yes').length
                                  tally = tally.toLocaleString('en-US');
                                  group_total = '<b>' + tally + '</b>';
                                  return group_total;
                                }"
                              ),
                              align = 'center',
                              name = "Vision 2025 Complete"),
      v2025_not_complete = colDef(
        aggregated = JS(
          "function(cellInfo, state) {
            let out = {};
            out = cellInfo.subRows.filter(val => val.v2025_not_complete === 'Yes').length;
            return out;
          }"
        ),
        footer = JS(
          "function(cellInfo, state) {
            let tally = {};
            let group_total = {};
            tally = state.data.filter(val => val.v2025_not_complete === 'Yes').length
            tally = tally.toLocaleString('en-US');
            group_total = '<b>' + tally + '</b>';
            return group_total;
          }"
        ),
        html = TRUE,
        align = "center",
        name = "Countries With V2025 Needs"
      ),
      one_remaining = colDef(aggregated = JS("function(cellInfo) {
                                                 let count = cellInfo.subRows.filter(val => val.one_remaining === 'Yes').length
                                                 return count}"),
                             footer = JS(
                               "function(cellInfo, state) {
                                  let tally = {};
                                  let group_total = {};
                                  tally = state.data.filter(val => val.one_remaining === 'Yes').length
                                  tally = tally.toLocaleString('en-US');
                                  group_total = '<b>' + tally + '</b>';
                                  return group_total;
                                }"
                             ),
                             align = "center",
                             name = "1 remaining"),
      two_to_5 = colDef(aggregated = JS("function(cellInfo) {
                                                 let count = cellInfo.subRows.filter(val => val.two_to_5 === 'Yes').length
                                                 return count}"),
                        footer = JS(
                          "function(cellInfo, state) {
                            let tally = {};
                            let group_total = {};
                            tally = state.data.filter(val => val.two_to_5 === 'Yes').length
                            tally = tally.toLocaleString('en-US');
                            group_total = '<b>' + tally + '</b>';
                            return group_total;
                          }"
                        ),
                        align = "center",
                        name = "2 to 5 remaining"),
      six_to_20 = colDef(aggregated = JS("function(cellInfo) {
                                                 let count = cellInfo.subRows.filter(val => val.six_to_20 === 'Yes').length
                                                 return count}"),
                         footer = JS(
                           "function(cellInfo, state) {
                              let tally = {};
                              let group_total = {};
                              tally = state.data.filter(val => val.six_to_20 === 'Yes').length
                              tally = tally.toLocaleString('en-US');
                              group_total = '<b>' + tally + '</b>';
                              return group_total;
                            }"
                         ),
                         align = "center",
                         name = "6 to 20 remaining"),
      twentyone_plus = colDef(aggregated = JS("function(cellInfo) {
                                                 let count = cellInfo.subRows.filter(val => val.twentyone_plus === 'Yes').length
                                                 return count}"),
                              footer = JS(
                                "function(cellInfo, state) {
                                  let tally = {};
                                  let group_total = {};
                                  tally = state.data.filter(val => val.twentyone_plus === 'Yes').length
                                  tally = tally.toLocaleString('en-US');
                                  group_total = '<b>' + tally + '</b>';
                                  return group_total;
                                }"
                              ),
                              align = "center",
                              name = "21 or more remaining"),
      `Remaining Translation Needs` = colDef(aggregate = "sum", align = "center",
                                             footer = JS(
                                               "function(cellInfo, state) {
                                                  let group_total = 0;
                                                  state.sortedData.forEach(function(row) {
                                                    group_total += row['Remaining Translation Needs'];
                                                  })
                                                  group_total = group_total.toLocaleString('en-US');
                                                  group_total = '<b>' + group_total + '</b>';
                                                  return group_total;
                                                }"
                                              )
                                            ),
      `Sign languages` = colDef(aggregate = "sum", align = "center",
                                footer = JS(
                                  "function(cellInfo, state) {
                                    let group_total = 0;
                                    state.sortedData.forEach(function(row) {
                                      group_total += row['Sign languages'];
                                    })
                                    group_total = group_total.toLocaleString('en-US');
                                    group_total = '<b>' + group_total + '</b>';
                                    return group_total;
                                  }"
                                )
                              ),
      `Sign languages remaining` = colDef(aggregate = "sum", align = "center",
                                           footer = JS(
                                             "function(cellInfo, state) {
                                                let group_total = 0;
                                                state.sortedData.forEach(function(row) {
                                                  group_total += row['Sign languages remaining'];
                                                })
                                                group_total = group_total.toLocaleString('en-US');
                                                group_total = '<b>' + group_total + '</b>';
                                                return group_total;
                                              }"
                                           )
                                        )
    )

    col_defs <- col_def_defaults[names(df)]

    # *** table creation ***
    table <- reactable::reactable(
      # elementId = "V2025_summary_table",
      data = df,
      sortable = TRUE,
      searchable = TRUE,
      # filterable = TRUE,
      striped = TRUE,
      compact = TRUE,
      defaultColDef = colDef(html = TRUE),
      groupBy = c("Area"),
      columns = col_defs
    )

    table
  })

  output$all_access_summary_table <- renderReactable({
    req(input$selected_partner)

    # *** field selection ***

    if(is.null(values$all_access_summary_tbl_chosen_flds)) {
      df <-  all_access_summary_df()
      choices <- names(df)
      values$all_access_summary_tbl_fld_choices <- choices[5:length(choices)]
    } else {
      df <- values$all_access_summary_tbl_data
    }

    # *** row selection ***

    if (length(input$selected_areas) == 0) {
      selected_areas = df$Area %>% unique()
    } else {
      selected_areas = input$selected_areas
    }

    if (length(input$selected_countries) == 0) {
      selected_countries = df$`Country Code` %>% unique()
    } else {
      selected_countries = input$selected_countries
    }

    names_to_simplify <- c(no_unmet_goals = "All Access goals met", one_unmet = "1 unmet goal",
                           two_to_5 = "2 to 5 unmet goals", six_to_20 = "6 to 20 unmet goals",
                           twentyone_plus= "21 or more unmet goals")

    df <- df %>%
      filter(Area %in% selected_areas) %>%
      filter(`Country Code` %in% selected_countries) %>%
      rename(any_of(names_to_simplify)) %>%
      select(-`Country Code`, -`Remaining AAG chapters`)

    # *** colDef selection for table columns argument ***

    col_def_defaults <-  list(
      Area = colDef(grouped = JS("function(cellInfo) {return cellInfo.value + ' (' + cellInfo.subRows.length + ' countries)'}"),
                    footer = "<b>Totals</b>",
                    html = TRUE),
      # Countries = colDef(name = "Countries"),
      Countries = colDef(name = "Countries",
                         html = TRUE,
                         footer = "(click country name to see Joshua Project entry)",
                         footerStyle = "font-size: 10px;"),
      no_unmet_goals = colDef(aggregated = JS("function(cellInfo) {
                                                 let count = cellInfo.subRows.filter(val => val.no_unmet_goals === 'Yes').length
                                                 return count}"),
                              footer = JS(
                                "function(cellInfo, state) {
                                  let tally = {};
                                  let group_total = {};
                                  tally = state.data.filter(val => val.no_unmet_goals === 'Yes').length;
                                  tally = tally.toLocaleString('en-US');
                                  group_total = '<b>' + tally + '</b>';
                                  return group_total
                                }"
                              ),
                              align = 'center',
                              name = "Countries with All Access goals met"),
      one_unmet = colDef(aggregated = JS("function(cellInfo) {
                                                 let count = cellInfo.subRows.filter(val => val.one_unmet === 'Yes').length
                                                 return count}"),
                         footer = JS(
                           "function(cellInfo, state) {
                                  let tally = {};
                                  let group_total = {};
                                  tally = tally.toLocaleString('en-US');
                                  tally = state.data.filter(val => val.one_unmet === 'Yes').length
                                  group_total = '<b>' + tally + '</b>';
                                  return group_total
                                }"
                         ),
                             align = "center", name = "1 unmet goal"),
      two_to_5 = colDef(aggregated = JS("function(cellInfo) {
                                                 let count = cellInfo.subRows.filter(val => val.two_to_5 === 'Yes').length
                                                 return count}"),
                        footer = JS(
                          "function(cellInfo, state) {
                                  let tally = {};
                                  let group_total = {};
                                  tally = state.data.filter(val => val.two_to_5 === 'Yes').length
                                  tally = tally.toLocaleString('en-US');
                                  group_total = '<b>' + tally + '</b>';
                                  return group_total
                                }"
                        ),
                        align = "center",
                        name = "2 to 5 unmet goals"),
      six_to_20 = colDef(aggregated = JS("function(cellInfo) {
                                                 let count = cellInfo.subRows.filter(val => val.six_to_20 === 'Yes').length
                                                 return count}"),
                         footer = JS(
                           "function(cellInfo, state) {
                                  let tally = {};
                                  let group_total = {};
                                  tally = state.data.filter(val => val.six_to_20 === 'Yes').length
                                  tally = tally.toLocaleString('en-US');
                                  group_total = '<b>' + tally + '</b>';
                                  return group_total
                                }"
                         ),
                         align = "center",
                         name = "6 to 20 unmet goals"),
      twentyone_plus = colDef(aggregated = JS("function(cellInfo) {
                                                 let count = cellInfo.subRows.filter(val => val.twentyone_plus === 'Yes').length
                                                 return count}"),
                              footer = JS(
                                "function(cellInfo, state) {
                                  let tally = {};
                                  let group_total = {};
                                  tally = state.data.filter(val => val.twentyone_plus === 'Yes').length
                                  tally = tally.toLocaleString('en-US');
                                  group_total = '<b>' + tally + '</b>';
                                  return group_total;
                                }"
                              ),
                              align = "center",
                              name = "21 or more unmet goals"),
      `Total unmet goals` = colDef(aggregate = "sum",
                                   footer = JS(
                                     "function(cellInfo, state) {
                                                let group_total = 0;
                                                state.sortedData.forEach(function(row) {
                                                  group_total += row['Total unmet goals'];
                                                })
                                                group_total = group_total.toLocaleString('en-US');
                                                group_total = '<b>' + group_total + '</b>';
                                                return group_total
                                              }"
                                   ),
                                   align = "center",
                                   name = "Total unmet goals",
                                   format = colFormat(separators = TRUE)),
      `Total AAG chapters` = colDef(aggregate = "sum",
                                    footer = JS(
                                      "function(cellInfo, state) {
                                                let group_total = 0;
                                                state.sortedData.forEach(function(row) {
                                                  group_total += row['Total AAG chapters'];
                                                })
                                                group_total = group_total.toLocaleString('en-US');
                                                group_total = '<b>' + group_total + '</b>';
                                                return group_total
                                              }"
                                    ),
                                    align = "center",
                                    name = "Total AAG chapters",
                                    format = colFormat(separators = TRUE)),
      # `% Remaining AAG chapters` = colDef(name = "% chapters remaining",
      #                                     align = "center",
      #                                     format = colFormat(percent = TRUE, digits = 0),
      #                                     na = "--"),
      `% Remaining AAG chapters` = colDef(aggregate = "mean",
                                          footer = JS(
                                            "function(cellInfo, state) {
                                          let total = 0;
                                          let count = 0;
                                          state.sortedData.forEach(function(row) {
                                            if (row[`% Remaining AAG chapters`] !== null) {
                                              total += row[`% Remaining AAG chapters`];
                                              count++;
                                            }
                                          })
                                          let mean = count > 0 ? total / count : 0;
                                          mean = (mean * 100).toFixed(0) + '%';
                                          return '<b>' + mean + '</b>';
                                        }"
                                          ),
                                          name = "% chapters remaining",
                                          align = "center",
                                          format = colFormat(percent = TRUE, digits = 0),
                                          na = "--"),
      `Sign languages` = colDef(aggregate = "sum",
                                footer = JS(
                                  "function(cellInfo, state) {
                                                let group_total = 0;
                                                state.sortedData.forEach(function(row) {
                                                  group_total += row['Sign languages'];
                                                })
                                                group_total = group_total.toLocaleString('en-US');
                                                group_total = '<b>' + group_total + '</b>';
                                                return group_total
                                              }"
                                ),
                                align = "center"),
      `Unmet SL goals` = colDef(aggregate = "sum",
                                footer = JS(
                                  "function(cellInfo, state) {
                                                let group_total = 0;
                                                state.sortedData.forEach(function(row) {
                                                  group_total += row['Unmet SL goals'];
                                                })
                                                group_total = group_total.toLocaleString('en-US');
                                                group_total = '<b>' + group_total + '</b>';
                                                return group_total
                                              }"
                                ),
                                align = "center"),
      `Total SL AAG chapters` = colDef(aggregate = "sum",
                                       footer = JS(
                                         "function(cellInfo, state) {
                                                let group_total = 0;
                                                state.sortedData.forEach(function(row) {
                                                  group_total += row['Total SL AAG chapters'];
                                                })
                                                group_total = group_total.toLocaleString('en-US');
                                                group_total = '<b>' + group_total + '</b>';
                                                return group_total
                                              }"
                                       ),
                                       align = "center",
                                       name = "Total SL AAG chapters",
                                       format = colFormat(separators = TRUE)),
      `Remaining SL AAG chapters reported` = colDef(aggregate = "sum",
                                                    footer = JS(
                                                      "function(cellInfo, state) {
                                                let group_total = 0;
                                                state.sortedData.forEach(function(row) {
                                                  group_total += row['Remaining SL AAG chapters reported'];
                                                })
                                                group_total = group_total.toLocaleString('en-US');
                                                group_total = '<b>' + group_total + '</b>';
                                                return group_total
                                              }"
                                        ),
                                        align = "center",
                                        name = "Remaining SL AAG chapters reported",
                                        format = colFormat(separators = TRUE))
    )

    col_defs <- col_def_defaults[names(df)]

    # *** table creation ***

    table <- reactable::reactable(
      # elementId = "all_access_summary_table",
      data = df,
      sortable = TRUE,
      searchable = TRUE,
      striped = TRUE,
      compact = TRUE,
      groupBy = c("Area"),
      columns = col_defs,
      defaultColDef = colDef(html = TRUE),
    )

    table
  })

  output$dashboard_SL_chart <- renderPlot ({
    # req(input$selected_db_view)
    req(input$selected_countries)
    df <- main_rows %>%
      filter(`Country Code` %in% input$selected_countries)
    get_SL_chart(df)
  })

  observeEvent(input$lnk_adv_srch, {
    showModal(adv_search_modal())
  })

  adv_search_modal <- function() {
    modalDialog(
      textAreaInput("textArea_lang_codes",
                    label = "Enter list language codes (comma separated).",
                    value = "",
                    width = "100%",
                    placeholder = "Include only codes used in ProgressBible"),
      fluidRow(
        box(
          width = 12,
          actionButton("btn_adv_search",
                       label = "Search")
          # actionButton("btn_adv_search",
          #              label = "Search"),
          # actionButton("btn_clear_search",
          #              label = "Clear search and reset Main Table")
        )
      ),
      title = "Search by Language Code",
      footer = modalButton("Done"),
      size = "m",
      # size = c("m", "s", "l", "xl"),
      easyClose = TRUE,
      fade = TRUE
    )
  }

  iso_codes <- reactive({
    codes <- if(!is.null(input$textArea_lang_codes)) {
      input$textArea_lang_codes %>%
      str_remove_all("\\s+") %>%
      str_split_1(",") %>%
      unique()
    } else {
      lang_codes()
    }
  })

  observe(
    {
      values$val_iso_search = TRUE
    }
  ) |> bindEvent(input$btn_adv_search)

  observe(
     {
      # values$val_iso_search = TRUE
      # values$val_hist_search = TRUE

      language_codes <- lang_codes()

      update_country_selectors(language_codes)

      # language_codes <- isolate(input$textArea_lang_codes) %>%
      #   str_remove_all("\\s+") %>%
      #   str_split_1(",") %>%
      #   unique()

      # country_codes <- main_rows %>%
      #   filter(`Language Code` %in% language_codes) %>%
      #   # pull(`Country`) %>%
      #   pull(`Country Code`) %>%
      #   as.character() %>%
      #   unique()
      #
      # areas <- combined_partner_areas %>%
      #   filter(Partner == input$selected_partner) %>%
      #   filter(`Country Code` %in% country_codes) %>%
      #   # filter(`Country` %in% country_codes) %>%
      #   pull(Area) %>%
      #   unique()
      #
      # prior_context <- tibble(
      #   partner = input$selected_partner |> str_c(collapse = "@@"),
      #   areas = input$selected_areasr |> str_c(collapse = "@@"),
      #   countries = input$selected_countriesr |> str_c(collapse = "@@")
      # )
      #
      # values$prior_context <- prior_context
      #
      #
      # updateSelectInput(session = session,
      #                   inputId = "selected_areas",
      #                   selected = areas)
      #
      # updateSelectInput(session = session,
      #                   inputId = "selected_countries",
      #                   choices = country_choices(),
      #                   selected = country_codes)
      #
      # if(is.null(input$selected_countries)) {
      #   updateSelectInput(session = session,
      #                     inputId = "selected_countries",
      #                     choices = areas_selected(),
      #                     selected = country_codes)
      # }

  }) |> bindEvent(input$btn_adv_search,
                  input$btn_send_to_RT
                  ,ignoreInit = TRUE)

  observeEvent(input$btn_clear_search,
    { # bound to input$btn_clear_search
      prior_context <- values$prior_context

      if (is.null(prior_context)) {
        prior_context <- tibble(
          partner = get_cookie(
            cookie_name = "sel_partner",
            missing = "Global Partnerships",
            session = shiny::getDefaultReactiveDomain()),
          areas = get_cookie(
            cookie_name = "sel_areas",
            missing = "Pacific",
            session = shiny::getDefaultReactiveDomain()),
          countries = get_cookie(
            cookie_name = "sel_countries",
            missing = "VU",
            session = shiny::getDefaultReactiveDomain())
        )
      }

      values$val_db_search <- FALSE
      values$val_iso_search = FALSE
      values$val_hist_search = FALSE

      updateTextAreaInput(session = session,
                          inputId = "textArea_lang_codes",
                          value = "")
      updateSelectInput(session = session,
                        inputId = "partner",
                        selected = prior_context %>%
                          pull(partner))
                          # pull(partner) |> str_split_1("@@"))
                          # pull(partner) |> str_c(collapse = "@@"))
      updateSelectInput(session = session,
                        inputId = "selected_areas",
                        selected = prior_context %>%
                          pull(areas))
                          # pull(partner) |> str_split_1("@@"))
                          # pull(partner) |> str_c(collapse = "@@"))
      if ("countries" %in% names(prior_context)) {
        updateSelectInput(session = session,
                          inputId = "selected_countries",
                          choices = country_choices(),
                          selected = prior_context %>%
                            pull(countries) |> str_split_1("@@"))
                            # pull(partner) |> str_split_1("@@"))
                            # pull(partner) |> str_c(collapse = "@@"))
      }

      clearSearch(RT_proxy)
  })

  # this resets val_iso_search to FALSE whenever selected countries are changed
  # observe({ # bound to input$selected_countries
  #   values$val_iso_search = FALSE
  # }) %>% bindEvent(input$selected_countries)

  # *** collection creation and sharing ***

  collection_langs <- reactive({
    # df <- isolate(main_rows) |>
    # main_rows_reactive() |>
    # values$val_collections_load <- TRUE
    df <- main_rows |>
    # df <- isolate(main_rows_reactive()) |>
      # filter(`Language Code` %in% lang_codes()) |>
      filter(`Language Code` %in% selected_lang_codes()) |>
      select(Country, Subdivision, `Language Name`)
    # values$val_collections_load <- FALSE
    return(df)
  })

  collection_users <- reactive({
    df <- isolate(gp_app_users) |>
      filter(mesa_user == "yes") |>
      select(full_name, Organization, email) |>
      rename(Name = full_name,
             Email = email)
  })

  # collection_shares <- reactive({
  #   df <- isolate(gp_app_users) |>
  #   select(full_name, email, Organization)
  # })

  collection_df <- reactive({
    if (is.null(values$collections)) {
      # initialize collection_df
      df <- tibble(
        name = "test",
        user_id = user,
        languages = "",
        sharing_option = "",
        shares = "",
        date = Sys.Date()
      )
    } else {
      df <- values$collections
    }
  })

  output$save_collection_link <- renderUI({
    log_val <- input$research_table_rows_selected
    if(!is_null(log_val)) {
    # if("input.research_table_rows_selected != ''") {
       label <- "Save selected rows as a collection"
     } else {
       label <- "Select one or more rows to define a collection."
     }
    actionLink("lnk_save_collection",
              label = label,
              icon = icon("layer-group"))
  })


  observeEvent(input$lnk_save_collection, {
    showModal(save_collection_modal())
  })

  # validate collection name
  observeEvent(input$collection_name, {
    # req(input$collection_name)
    input_name <- input$collection_name
    # used_names <- collection_df() |>
    #   pull(name)
    used_names <- collection_df() |>
      select(name, user_id)
    users_names <- used_names |>
      filter(user_id == user) |>
      pull(name)
    if (input_name %in% used_names$name) {
      if(input_name %in% users_names){
        values$validation_message <- paste0('You already have a collection named "', input_name, '." Please enter another name or click "Save and close" to overwrite it.')
        values$collection_to_replace <- input_name
        values$name_is_ok <- TRUE
      } else {
        values$validation_message <- paste0('"', input_name, '" is already taken. Please enter another name.')
        values$collection_to_replace <- ""
        values$name_is_ok <- FALSE
      }
    } else if(input_name == "") {
      values$collection_to_replace <- ""
      values$name_is_ok <- TRUE
      values$validation_message <- ""
    } else {
      values$collection_to_replace <- ""
      values$name_is_ok <- TRUE
      values$validation_message <- paste0('"', input_name, '" is available.')
    }
    output$val_message <- renderText({values$validation_message})
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  output$sharing_list <- renderReactable({
  # output$sharing_list <- renderUI({
    if (input$share_opt == 'some') {
      reactable(
        data = collection_users(),
        compact = TRUE,
        selection = "multiple",
        filterable = TRUE
      )
    }
  })

  observeEvent(input$btn_save_collection, {
    df <- values$collections
    coll_to_replace <- values$collection_to_replace # set at validation
    if (coll_to_replace != "") {
      revised_collection_df <- df |>
        filter(name != coll_to_replace)
      values$collections <- revised_collection_df
      values$collection_to_replace <- ""
      # write_collections(revised_collection_df)
    }

      # values$val_RT_rows_selected <- input$research_table_rows_selected
    if (values$name_is_ok){
      name <- input$collection_name
      description <- input$description
      languages <- isolate(collection_langs()) |>
      # languages <- collection_langs() |>
        pull(`Language Name`) |>
        str_trim() |>
        str_c(collapse = "; ")
      shares <- if (input$share_opt == 'some') {
          indices <- getReactableState(
            # outputId = "user_table",
            outputId = "sharing_list",
            name = "selected",
            session = session)
          isolate(collection_users()) |>
            slice(indices)|>
            pull(Email) |>
            str_trim() |>
            str_c(collapse = "; ")
        }else {
          ""
        }
    } else {
      showNotification(
        ui = "Please enter an unused collection name",
        duration = NULL,
        closeButton = TRUE,
        type = "error")
    }

    new_collection <- tibble(
      name = name,
      description = description,
      user_id = user,
      user_full_name = mesa_users_df |>
        filter(email == user) |>
        pull(full_name),
      languages = languages,
      sharing_option = input$share_opt,
      shares = shares,
      date = as.character(Sys.Date())
    )

    if (name == "") {
      showNotification(
        "A unique collection name is required.",
        type = "error"
      )
    } else {
      removeModal(session = session)
      updated_collections <- append_collection(new_collection, values$collections)
      values$collections <- updated_collections
      write_collections(values$collections)
    }

    # values$collection <- new_collection

  })

  observeEvent(input$btn_cancel_save_collection_modal, {

    removeModal(session = session)
  })

  output$collection_langs_list <- renderReactable({
    table <- collection_langs()
    # table <- isolate(collection_langs())
    reactable(
      data = table,
      # data = isolate(collection_langs()),
      compact = TRUE
    )
  })

  save_collection_modal <- function(collection) {
    modalDialog(
      box(
        width = 12,
        title = "Name for the collection (required)",
        textInput(inputId = "collection_name",
          label = NULL,
          placeholder = "Enter a unique name"),
        textOutput("val_message"),
        textAreaInput("description",
          label = "Description",
          placeholder = "Enter a brief description for the purpose of the collection.")
      ),
      box(
        width = 12,
        height = "auto",
        title = "Selected languages",
        # gt_output("collection_langs_list")
        # shinycssloaders::withSpinner(
        #   gt_output("collection_langs_list")
        # )
        shinycssloaders::withSpinner(
          reactableOutput("collection_langs_list")
        )
        # shinycssloaders::withSpinner(
        #   DTOutput("collection_langs_list")
        # )
      ),
      box(
        width = 12,
        title = "Sharing options",
        radioButtons(inputId = "share_opt",
                     label = NULL,
                     choices = c("Private, do not share" = "none",
                                 "Share with all Mesa users" = "all",
                                 "Select from list" = "some"),
                     selected = "none",
                     inline = TRUE),
        reactableOutput("sharing_list", height = 'auto')
        # uiOutput("sharing_list", height = 'auto')
      ),
      fluidRow(
        box(
          width = 12,
          actionButton("btn_save_collection",
                       label = "Save and close"),
          actionButton("btn_cancel_save_collection_modal",
                       label = "Cancel")
        )
      ),
      title = "Save and share selected languages as a collection",
      footer = modalButton("Quit"),
      size = "l",
      # size = c("m", "s", "l"),
      easyClose = TRUE,
      fade = TRUE
    )
  }


  output$research_table <- renderDT({
    # req(input$main_page_tabset == "table_builder")
    df <- main_rows_reactive()
    get_DT_main_obj(df)
  }, server = TRUE)
  # }, server = FALSE)

  RT_proxy <- dataTableProxy("research_table")

  observeEvent(input$research_table_rows_selected, {
    selected_rows <- input$research_table_rows_selected
    values$val_RT_rows_selected <- selected_rows
    # RT_proxy |> selectRows(selected_rows)
    # if (length(values$val_RT_rows_selected) > 0) {
    #   selected_rows <- values$val_RT_rows_selected
    # } else {
    #   selected_rows <- input$research_table_rows_selected
    #   values$val_RT_rows_selected <- selected_rows
    # }
  })

  observe({
    bib_id <- input$clicked_bib_id
    showModal(bib_list_modal(bib_id))
  }) |> bindEvent(input$clicked_bib_id)

  bib_list_modal <- function(bib_id) {
    md <- modalDialog(
      {
        data <- SP_rows |>
          filter(`Language Code` == bib_id) |>
          mutate(`Publisher Info` = glue::glue("<b>Publishing Org:</b> {`Publishing Org`}<br>
                                                <b>Rights Holder:</b> {`Rights Holder`}<br>
                                                <b>Rights Stmt:</b> {`Rights Stmt`}")) |>
          select(`Language Name`, `Year Published`, `Scripture Level`,
                 `Name Primary`, `Other Titles`,  `Mode Name`, `Publisher Info`) |>
          mutate(`Year Published` = str_replace_all(`Year Published`, "^1$", "-")) |>
          replace_na(replace = list(`Other Titles` = '-')) |>
          arrange(`Year Published`)
        caption_text <- data$`Language Name` |> unique()
        renderDT(
          datatable(data,
                    escape = FALSE,
                    caption = HTML(paste0("<h4>", caption_text, "</h4>")),
                    options = list(
                      scrollY = '60vh',
                      # scrollX = 'true',
                      # scrollX = 'false',
                      scrollcollapse = TRUE,
                      columnDefs = list(list(visible = FALSE, targets = 1)),
                      dom = 'Blfriti',
                      buttons = c('copy', 'csv'),
                      lengthMenu = list(c(-1, 10, 25, 50), c("All", 10, 25, 50))
                      )
                    )
        )
        # gt(data, caption = lang_name)
      },
      title = "Scripture Products (ProgressBible)",
      # footer = actionButton("dismiss", "Dismiss"),
      footer = modalButton("Dismiss"),
      size = "l",
      # size = c("m", "s", "l", "xl"),
      easyClose = FALSE,
      fade = TRUE
    )
    return(md)
  }

  observe({
    removeModal()
  }) |> bindEvent(input$dismiss)

  # returns lang_code(s) for given indices
  selected_lang_codes <- reactive({
    rows_selected <- values$val_RT_rows_selected
    # rows_selected <- input$research_table_rows_selected
    # main_df <- main_rows_reactive()
    main_df <- isolate(main_rows_reactive())
    selected_lang_codes <- main_df |>
      slice(rows_selected) %>%
      pull(`Language Code`)
  })

  output$RT_details <- renderUI({

    rows_selected <- input$research_table_rows_selected
    # rows_selected <- req(getReactableState("research_table", "selected"))

    main_df <- isolate(main_rows_reactive())

    details_lang_codes <- if (!is.null(selected_lang_codes())) {
      selected_lang_codes()
    } else {
      "none"
    }

    # details_lang_codes <- if (!is.null(rows_selected)) {
    #   main_df %>%
    #     slice(rows_selected) %>%
    #     pull(`Language Code`)
    # } else {
    #   "none"
    # }

    # details_lang_codes <- main_df %>%
    #   slice(rows_selected) %>%
    #   pull(`Language Code`)

    nested_tables_list <- list(
      "WIP" = filter(WIP_rows, `Language Code` %in% details_lang_codes),
      "alt_names" = filter(alt_names_rows, `Language Code` %in% details_lang_codes),
      "shared_collections" = shared_coll_rows() |>
        filter(str_detect(Languages, paste0('(', str_c(details_lang_codes, collapse = "|"),')'))),
      "SP" = filter(SP_rows, `Language Code` %in% details_lang_codes),
      "WCD" = filter(WCD_rows, `Language Code` %in% details_lang_codes),
      "ROLV" = filter(ROLV_rows, `Language Code` %in% details_lang_codes)
      # ,
      # "Ethnologue" = filter(Ethnologue_rows, `Language Code` %in% details_lang_codes)
    )

    nested_tables_nrows <- list(
      "WIP" = nested_tables_list$WIP %>% nrow(),
      "alt_names" = nested_tables_list$alt_names %>% nrow(),
      "shared_collections" = nested_tables_list$shared_collections |> nrow(),
      "SP" = nested_tables_list$SP %>% nrow(),
      "WCD" = nested_tables_list$WCD %>% nrow(),
      "ROLV" = nested_tables_list$ROLV %>% nrow()
      # ,
      # "Ethnologue" = nested_tables_list$Ethnologue %>% nrow()
    )

    nested_tables_name <- list(
      "WIP" = "Work in Progress",
      "alt_names" = "Alternate Language Names",
      "shared_collections" = "Shared Language Collections",
      "SP" = "Scripture Products",
      "WCD" = "World Christian Database",
      "ROLV" = "Registry of Language Varieties"
    )

    nested_tables_df <- tibble(
      id = names(nested_tables_name),
      tab_label = paste(nested_tables_name," (",nested_tables_nrows,")"),
      nrows = nested_tables_nrows,
      data = nested_tables_list)

    rm(list = c("nested_tables_list"))

    tabsetPanel(id = "RT_details",
                type = "pills",
                selected = "WIP",
                tabPanel(value = "WIP",
                         title = paste("Work In Progress (", nested_tables_nrows$WIP, ")"),
                         {
                           data = get_details_data("WIP", nested_tables_df)
                           rowCount = nrow(data)
                           if(rowCount != 0) {
                             get_DT_details_obj(data)

                           } else {
                             "No data"
                           }
                         }
                ),
                tabPanel(value = "alt_names",
                         title = paste("Alternate Language Names (", nested_tables_nrows$alt_names, ")"),
                         {
                           data = get_details_data("alt_names", nested_tables_df)
                           rowCount = nrow(data)
                           if(rowCount != 0) {
                             get_DT_details_obj(data)

                           } else {
                             "No data"
                           }
                         }
                ),
                tabPanel(value = "shared_colls",
                         title = paste("Shared Language Collections (", nested_tables_nrows$shared_collections, ")"),
                         {
                           data = get_details_data("shared_collections", nested_tables_df)
                           rowCount = nrow(data)
                           if(rowCount != 0) {
                             get_DT_details_obj(data)

                           } else {
                             "No data"
                           }
                         }
                ),
                tabPanel(value = "SP",
                         title = paste("Scripture Products (", nested_tables_nrows$SP, ")"),
                         {
                           data = get_details_data("SP", nested_tables_df) %>%
                             select(-`Language Code`)
                           rowCount = nrow(data)
                           if(rowCount != 0) {
                             get_DT_details_obj(data)
                           } else {
                             "No data"
                           }
                         }
                ),
                tabPanel(value = "WCD",
                         title = paste("World Christian Database (", nested_tables_nrows$WCD, ")"),
                        {
                          # data <- get_details_data("WCD", nested_tables_df)
                          data = nested_tables_df %>%
                            filter(id == "WCD") %>%
                            unnest(cols = c(data)) %>%
                            select(-id, -tab_label, -nrows, -`Language Code`)
                          rowCount = nested_tables_df %>% filter(id == "WCD") %>% pull(nrows)
                          if(rowCount != 0) {
                            get_DT_details_obj(data)
                          } else {
                            "No data"
                          }
                        }
                ),
                tabPanel(value = "ROLV",
                         title = paste("Registry of Language Varieties (", nested_tables_nrows$ROLV, ")"),
                         {
                           # data <- get_details_data("ROLV", nested_tables_df)
                           data <- nested_tables_df |>
                             filter(id == "ROLV") %>%
                             unnest(cols = c(data)) |>
                             rename(`Variety Name` = `Variety Name (ROLV)`,
                                    `Variety Code` = `Variety Code (ROLV)`,
                                    `Location (ROLV)` = `Location Name (ROLV)`) |>
                             select(`Location (ROLV)`, `Language Name (ROLV)`, `Language Code`,
                                    `Variety Name`, `Variety Code`) |>
                             arrange(`Location (ROLV)`, `Language Code`)
                           # select(-id, -tab_label, -nrows, -`Language Code`)
                           rowCount = nested_tables_df %>% filter(id == "ROLV") %>% pull(nrows)
                           if(rowCount != 0) {
                             get_DT_details_obj(data)
                           } else {
                             "No data"
                           }
                         }
                )
        )
  })

  output$export_RT <- downloadHandler(
      filename = "Mesa_research_table_export.xlsx",

      content = function(file) {
          selection_indexes <- getReactableState("research_table", name = "selected")
          if (length(selection_indexes) > 0) {
            selected_languages <- main_rows_reactive() %>%
                slice(selection_indexes) %>%
                pull(`Language Code`)
          } else {
            selected_languages <- main_rows_reactive() %>%
              pull(`Language Code`)
          }

          main_table <- main_rows_reactive() %>%
          # main_table <- table_data_reactive() %>%
            filter(`Language Code` %in% selected_languages) %>%
            mutate_if(is.character,
                      stringr::str_replace_all,
                      pattern = "</?b>",
                      replacement = "")

          wip <- WIP_rows %>%
              filter(`Language Code` %in% selected_languages)

          sp <- SP_rows %>%
              filter(`Language Code` %in% selected_languages)

          wcd <- WCD_rows %>%
              filter(`Language Code` %in% selected_languages)

          rolv <-  ROLV_rows %>%
              filter(`Language Code` %in% selected_languages)

          # eth <-  Ethnologue_rows %>%
          #     filter(`Language Code` %in% selected_languages)

          ss_list <- list("Main table" = main_table,
                          "Work in Progress" = wip,
                          "Scripture Products" = sp,
                          "World Christian Database" = wcd,
                          "Registry of Language Varieties" = rolv)
          # ,
          #                 "Ethnologue" = eth)

          wb <- openxlsx::buildWorkbook(ss_list,
                               file = file,
                               asTable = TRUE,
                               overwrite = TRUE,
                               closeButton = TRUE)

          openxlsx::saveWorkbook(wb, file, overwrite = TRUE, returnValue = FALSE)
          })

  output$export_map_table <- downloadHandler(
      filename = "Mesa_map_table_export.xlsx",

      content = function(file) {
        selection_indexes <- input$map_table_DT_rows_selected
        # selection_indexes <- getReactableState("map_table", name = "selected")
        if (length(selection_indexes) > 0) {
          selected_languages <- map_table_data() %>%
          # selected_languages <- main_rows_reactive() %>%
            slice(selection_indexes) %>%
            pull(`Language Code`)
        } else {
          selected_languages <- map_table_data() %>%
            pull(`Language Code`)
        }

        # selected_languages <- selectedLocations()$`Language Code`

        main_table <- map_table_data() %>%
          filter(`Language Code` %in% selected_languages) %>%
          mutate_if(is.character,
                    stringr::str_replace_all,
                    pattern = "</?b>",
                    replacement = "")

        wip <- WIP_rows %>%
            filter(`Language Code` %in% selected_languages)

        sp <- SP_rows %>%
          filter(`Language Code` %in% selected_languages)

        wcd <- WCD_rows %>%
            filter(`Language Code` %in% selected_languages)

        rolv <-  ROLV_rows %>%
            filter(`Language Code` %in% selected_languages)

        # eth <-  Ethnologue_rows %>%
        #     filter(`Language Code` %in% selected_languages)

        ss_list <- list("Main table" = main_table,
                        "Work in Progress" = wip,
                        "World Christian Database" = wcd,
                        "Registry of Language Varieties" = rolv)
        # ,
        #                 "Ethnologue" = eth)

        wb <- openxlsx::buildWorkbook(ss_list,
                             file = file,
                             asTable = TRUE,
                             overwrite = TRUE,
                             closeButton = TRUE)

        openxlsx::saveWorkbook(wb, file, overwrite = TRUE, returnValue = FALSE)
        })

    # *** observers related to field selection ***

  # observe({ # bound to input$selected_topic
  #   req(values$stored_fields)
  #   req(input$selected_topic)
  #   stored_fields <- values$stored_fields %>%
  #     filter(sub == input$selected_topic) %>% pull(fields)
  #
  #   updateCheckboxGroupInput(
  #     session = getDefaultReactiveDomain(),
  #     inputId = "selected_fields",
  #     # inputId = "chosen_fields",
  #     choices = field_choices(),
  #     selected = stored_fields,
  #     inline = TRUE)
  # }) %>% bindEvent(input$selected_topic, ignoreNULL = TRUE, ignoreInit = TRUE)

  # *** dirty update table button and store field choices appropriately on change ***
  # observe({ # bound to input$chosen_fields
  #   req(input$selected_topic)
  #
  #   # *** initialize or load temp store ***
  #   if (is.null(values$stored_fields)) { # load last_config_fields
  #       stored_fields <- get_default_config_fields()[[1]]
  #       # stored_fields <- default_fields
  #       # stored_fields <- last_config_fields
  #       # stored_fields <- stored_fields_default
  #       # values$stored_fields <- stored_fields
  #   } else { # load temp store values
  #       stored_fields <- values$stored_fields
  #   }
  #
  #   # *** capture current taxonomy, subgroup and field selections ***
  #   if (!is.null(input$chosen_fields)) { # rewrite subgroup's field choices
  #       stored_fields <- stored_fields %>%
  #       # stored_fields <- values$stored_fields %>%
  #         filter(!sub %in% input$selected_topic) %>%
  #         add_row(taxonomy = input$taxonomy,
  #                 sub = input$selected_topic,
  #                 fields = input$chosen_fields,
  #                 # fields_string = unique(glue_collapse(fields, sep = ", ")),
  #                 # fields_string = glue_collapse(unlist(fields), sep = ", "),
  #                 .before = 1)
  #   } else { # delete any prior field selections for current sub
  #     str(stored_fields)
  #       stored_fields <- stored_fields %>%
  #           filter(!sub %in% input$selected_topic)
  #   }
  #
  #   values$stored_fields <- stored_fields
  #
  #   # *** create or rewrite stored_fields_table ***
  #   subs <- stored_fields %>% pull(sub) %>% unique()
  #   fields_list <- map(subs, ~ stored_fields %>% filter(sub == .x) %>% pull(fields))
  #   stored_fields_table <- tibble(sub = subs, fields_list = fields_list)
  #
  #   values$stored_fields_table <- stored_fields_table}) %>%
  #     bindEvent(input$chosen_fields, ignoreNULL = FALSE, ignoreInit = FALSE)

  # *** initialize stored_fields_table from cookies ***
  # get_stored_countries_table <- reactive({
  #   partner_areas <- combined_partner_areas |>
  #     filter(Partner == input$selected_partner)
  #
  #   sel_areas <- get_cookie(
  #     cookie_name = "sel_areas",
  #     missing = NULL,
  #     session = getDefaultReactiveDomain()) |>
  #     str_split_1("@@")
  #
  #   sel_countries <- get_cookie(
  #     cookie_name = "sel_countries",
  #     missing = NULL,
  #     session = getDefaultReactiveDomain()) |>
  #     str_split_1("@@")
  #
  #   sel_areas |> map(\(x) {
  #     new_row <- tibble(
  #       area = x |> str_remove("sel_"),
  #       countries = partner_areas |>
  #         filter(Area == x) |>
  #         filter(`Country Code` %in% sel_countries) |>
  #         pull(Country) |>
  #         str_c(collapse = ", ")
  #     )
  #   }) |> list_rbind()
  # })

  get_stored_countries_table <- reactive({
#  print("@get_stored_countries_table()")
    partner_areas <- combined_partner_areas %>%
      filter(Partner == input$selected_partner) |>
      select(Area, Country, `Country Code`)

    sel_areas <- get_cookie(
      cookie_name = "sel_areas",
      missing = "Pacific",
      session = getDefaultReactiveDomain()) %>%
      str_split_1("@@")

#  print("sel_areas cookie retrieved")

    sel_countries <- get_cookie(
      cookie_name = "sel_countries",
      missing = "Vanuatu",
      session = getDefaultReactiveDomain()) %>%
      str_split_1("@@")

#  print("sel_counries cookie retrieved")

    # Combine filtering and use rowwise to iterate over selected areas
    out <- sel_areas %>%
      as_tibble() |>
      rowwise() |>
      mutate(
        count_sel_countries = partner_areas %>%
          filter(Area == value, `Country Code` %in% sel_countries) %>%
          pull(Country) %>% length(),
        count_area_countries = combined_partner_areas %>%
          filter(Partner == input$selected_partner) |>
          filter(Area == value) |>
          nrow()
      )

    out2 <- out %>%
      rowwise() %>%
      mutate(
        area = glue::glue("<b>{value}</b><br>
                          <span style='color:maroon'>({count_sel_countries} of {count_area_countries} countries selected)</span>"),
        countries = partner_areas %>%
          filter(Area == value, `Country Code` %in% sel_countries) %>%
          # filter(Area == area, `Country Code` %in% sel_countries) %>%
          # filter(Area %in% sel_areas, `Country Code` %in% sel_countries) %>%
          pull(Country) %>%
          str_c(collapse = ", ")
      ) %>%
      select(area, countries) %>%
      ungroup()
    return(out2)
  })



  output$gt_stored_countries <- render_gt({
    # values$stored_countries_table <- get_stored_countries_table()
    # stored_countries_table <- values$stored_countries_table
    # stored_countries_table <- isolate(get_stored_countries_table())
    stored_countries_table <- get_stored_countries_table()
    # values$stored_countries <- stored_countries_table$countries |>
    #   str_c(collapse = ", ") |>
    #   str_split_1(", ") |>
    #   stringi::stri_remove_empty()

    data <- stored_countries_table |>
      arrange(area)

      gt(data) %>%
        cols_align(align = "left",
                   columns = area) %>%
        cols_align(align = "left",
                   columns = countries) %>%
        tab_style(
            style = list(
                cell_text(size = px(12)),
                # cell_text(size = px(12),
                #           weight = "bold"),
                cell_borders(sides = "right", color = "#808080", style = "solid", weight = px(1))),
            locations = cells_body(
                columns = c("area"))) %>%
        tab_style(
            cell_text(size = px(12)),
            locations = cells_body(
                columns = c("countries"))) %>%
        tab_options(column_labels.hidden = TRUE,
                    table.align = "left",
                    data_row.padding = px(1)) |>
        fmt_markdown(columns = everything())
  })

  # get_stored_fields_table <- reactive({
  #   field_hierarchy |>
  #     filter(Topic != "Minimum") |>
  #     pull(Topic) |>
  #     unique() |>
  #     map(\(x) {
  #       cookie_names <- paste0("sel_", x)
  #     }) |> map(\(x) {
  #       value <- get_cookie(
  #         cookie_name = x,
  #         missing = "",
  #         # missing = NULL,
  #         session = getDefaultReactiveDomain()
  #       )
  #       new_row <- tibble(
  #         topic = x |> str_remove("sel_"),
  #         fields = value |> str_replace_all("@@", ", ")
  #       )
  #     }) |> list_rbind()
  # })

  get_stored_fields_list <- reactive({
    topic <- input$selected_topic
    name <- paste0("sel_", topic)
    value <- get_cookie(name,
                        missing = "",
                        session = session) |>
      str_c(collapse = ", ") |>
      str_split_1(", ") |>
      stringi::stri_remove_empty()
    return(value)
  })

  get_stored_fields_table <- reactive({
# print("get_stored_fields_table reactive")
    out <- field_hierarchy %>%
      filter(Topic != "Minimum") %>%
      pull(Topic) %>%
      unique() %>%
      map(\(x) {
        cookie_name <- paste0("sel_", x)
        value <- get_cookie(
          cookie_name = cookie_name,
          missing = "",
          session = getDefaultReactiveDomain()
        )
        # print(paste0("x = ", x))
        # print(paste0("value = ", value))
        # str(value)
        tibble(
          topic_count = field_hierarchy |> filter(Topic == x, Source == "Translation Status") |> nrow(),
          field_count = if_else(value == "", 0, str_count(value, "@@") + 1),
          # topic = x |> str_remove("sel_"),
          topic = glue::glue("<b>{str_remove(x, 'sel_')}</b><br>
                             <span style='color:maroon'>({field_count} of {topic_count} fields selected)</span>"),
          fields = value |> str_replace_all("@@", ", ")
        )
      }) %>%
      do.call(rbind, .)

    out <- out |>
      ungroup() |>
      arrange(topic) |>
      select(-topic_count, -field_count)
    return(out)
  })



  output$gt_stored_fields <- render_gt({
    # req(values$stored_fields_table)
    # stored_fields_table <- get_stored_fields_table() %>% isolate()
    stored_fields_table <- get_stored_fields_table()
    # values$stored_fields_table <- get_stored_fields_table()
    # stored_fields_table <- values$stored_fields_table
    stored_fields <- stored_fields_table$fields |>
      str_c(collapse = ", ") |>
      str_split_1(", ") |>
      stringi::stri_remove_empty()

    values$stored_fields <- stored_fields

    data <- stored_fields_table

    tbl <- gt(data) %>%
      cols_align(align = "left",
                 columns = topic) %>%
      cols_align(align = "left",
                 columns = fields) %>%
      tab_style(
          style = list(
              cell_text(size = px(12)),
              # cell_text(size = px(12),
              #           weight = "bold"),
              cell_borders(sides = "right", color = "#808080", style = "solid", weight = px(1))),
          locations = cells_body(
              columns = c("topic"))) %>%
      tab_style(
          cell_text(size = px(12)),
          locations = cells_body(
              columns = c("fields"))) %>%
      tab_options(column_labels.hidden = TRUE,
                  table.align = "left",
                  data_row.padding = px(1)) |>
      fmt_markdown(columns = everything())
    return(tbl)
  })

  # render and reactive functions for Language Map

    output$selectedLevels <- renderUI({
      # browser()
        checkboxGroupInput(inputId = "selected_status_levels",
                           label = "Select status levels to include:",
                           # label = "Status Levels",
                           choices = status_levels(),
                           selected = status_levels(),
                           width = NULL,
                           inline = TRUE)
    })

    status_type_selected <- reactive({
      req(input$selected_status_type)
      selected_type = input$selected_status_type
      return(selected_type)
    })

    status_levels <- reactive({
      req(input$selected_status_type)
      status_df <- maps_table %>%
          select(all_of(status_type_selected()))
      status_levels <- levels(status_df[[input$selected_status_type]])
      # status_levels <- levels(maps_table[[input$selected_status_type]])
      return(status_levels)
    })


    selected_status_data <- reactive({
      # req(input$selected_countries)
      req(status_type_selected())

      curr_rt_row_indices <- input$research_table_rows_all
      if (is.null(curr_rt_row_indices)) {
        curr_rt_df <- main_rows_reactive()
      } else {
        curr_rt_df <- main_rows_reactive()[curr_rt_row_indices, ]
      }
      curr_langs <- curr_rt_df$`Language Code`
      data <- maps_table |> filter(`Language Code` %in% curr_langs) |>
        filter(.data[[status_type_selected()]] %in% input$selected_status_levels) %>%

      # out <- maps_table %>%
      #   filter(.data[[status_type_selected()]] %in% input$selected_status_levels) %>%
      #   filter(`Country Code` %in% input$selected_countries)

      return(data)
      # return(out)
    })

    selected_status_poly_data <- reactive({
      req(status_type_selected())
      out <- eth_poly_status_df %>%
        filter(.data[[status_type_selected()]] %in% input$selected_status_levels)
      return(out)
    })

    selected_status_palette <- reactive({
      req(input$selected_status_type)
      out <- status_color_palettes %>%
          filter(status_type == input$selected_status_type)
      out2 <- out %>%
          unnest(cols = c(status_value, color)) %>%
          filter(status_value %in% input$selected_status_levels)
      return(out2)
    })

    selected_db_view_palette <- reactive({
      req(input$selected_marker_status)
      filter_on <- input$selected_marker_status
      # filter_on <- input$selected_db_view
      pal <- case_when(
        filter_on == "Vision 2025 Status" ~ "Translation Status",
        filter_on == "All Access Status" ~ "All Access Status",
        filter_on == "Is Remaining V2025 Need" ~ "Is Remaining V2025 Need",
        filter_on == "All Access Goal Not Met" ~ "On All Access List"
        # filter_on == "On All Access List" ~ "On All Access List"
      )

      out <- status_color_palettes %>%
        filter(status_type == pal)
      out2 <- out %>%
        unnest(cols = c(status_value, color))
      return(out2)
    })

    summary_map_palette <- reactive({
      filter_on <- values$vb_click_filter
      pal <- case_when(
        filter_on == "Vision 2025 Status" ~ "Translation Status",
        filter_on == "All Access Status" ~ "All Access Status",
        filter_on == "Is Remaining V2025 Need" ~ "Is Remaining V2025 Need",
        filter_on == "All Access Goal Not Met" ~ "On All Access List"
        # filter_on == "On All Access List" ~ "On All Access List"
      )
      out <- status_color_palettes %>%
        filter(status_type == pal)
      out2 <- out %>%
        unnest(cols = c(status_value, color))
      return(out2)
    })

    # list to store the selections for tracking
    data_of_click <- reactiveValues(clickedMarker = list())

    output$status_type <- renderUI({
      radioButtons(inputId="selected_status_type",
                   label="Select desired language marker status type:",
                   choices = status_var_names,
                   # choices = names(status_vars),
                   # choices = c("Translation Status", "All Access Status"),
                   selected = "Translation Status",
                   inline = TRUE,
                   width = NULL)
    })

    # output$dashboard_map <- renderLeaflet({
    #   req(input$selected_db_view)
    #   req(input$selected_countries)
    #
    #   df <- dashboard_df()
    #   dashboard <- input$selected_db_view
    #   countries <- df$Country %>% unique() %>% sort() %>% str_c(collapse = "; ")
    #   data <- get_map_data(df, dashboard, countries)
    #   # leaflet(data = data) %>%
    #   m <- leaflet(
    #     data = data,
    #     options = leafletOptions(preferCanvas = TRUE)) %>%
    #     addTiles(group = "Base features")
    #
    #   if (nrow(data) != 0) {
    #     m <- m %>%
    #       addCircleMarkers(
    #         group = "Language points",
    #         lng = ~ data$Longitude,
    #         lat = ~ data$Latitude,
    #         radius = 4,
    #         weight = 5,
    #         opacity = 0.9,
    #         label = data$`Language Name`,
    #         clusterOptions = markerClusterOptions()) %>%
    #       addMiniMap(
    #         width = 150,
    #         height = 150,
    #         zoomLevelOffset = -5,
    #         toggleDisplay = TRUE)
    #   }
    #   m
    # })

    pal_V2025_complete <- colorFactor(c("green"),
                                      levels = c("Vision 2025 Complete"),
                                      na.color = "white")

    pal_one_remaining <- colorFactor(c("red"),
                                     levels = c("1 remaining"),
                                     na.color = "white")

    pal_V2025_buckets <- colorFactor(c('#ffeda0','#feb24c','#f03b20'),
                               levels = c("2 to five", "Six to twenty", "Twenty-one or more"),
                               # levels = c("1 to 5", "6 to 20", "21 or more"),
                               reverse = FALSE,
                               na.color = "white")


    pal_AAG_all_met <- colorFactor(c("blue"),
                                      levels = c("All Access goals met"),
                                      na.color = "white")

    pal_one_unmet <- colorFactor(c("purple"),
                                     levels = c("1 unmet goal"),
                                     na.color = "white")

    pal_all_access_buckets <- colorFactor(c('#efedf5','#bcbddc','#756bb1'),
                               levels = c("2 to five", "Six to 20", "Twenty-one or more"),
                               reverse = FALSE,
                               na.color = "white")

    # ************* summary map ***************

    output$summary_map <- renderLeaflet({
      summary_map_reactive()
    })

    summary_map_reactive <- reactive({
      req(input$selected_countries)
      V2025_data <- v2025_summary_df() %>%
        mutate(
          V2025_bucket = case_when(
            `2 to 5 remaining` == "Yes" ~ "2 to five",
            `6 to 20 remaining` == "Yes" ~ "Six to twenty",
            `21 or more remaining` == "Yes" ~ "Twenty-one or more"),
          one_remaining = if_else(`1 remaining` == "Yes", "1 remaining", NA),
          V2025_complete = if_else(`Vision 2025 Complete` == "Yes", "Vision 2025 Complete", NA)
        )

      all_access_data <- all_access_summary_df() %>%
        mutate(
          all_access_bucket = case_when(
            `2 to 5 unmet goals` == "Yes" ~ "2 to five",
            `6 to 20 unmet goals` == "Yes" ~ "Six to 20",
            `21 or more unmet goals` == "Yes" ~ "Twenty-one or more"),
          `1 unmet goal` = if_else(`1 unmet goal` == "Yes", "1 unmet goal", NA),
          `All Access goals met` = if_else(`All Access goals met` == "Yes", "All Access goals met", NA)
        )

      labels_country <- V2025_data %>%
        left_join(all_access_data, by = "Country Code") %>%
        select(`Country Code`, Countries.x, `Remaining Translation Needs`, `Total unmet goals`) %>%
        rename(Countries = Countries.x) %>%
        mutate(country_labels = sprintf(
          "<strong>%s</strong><br/>
           %g Vision 2025 needs remaining<br/>
           %g All Access goal(s) unmet",
          Countries, `Remaining Translation Needs`, `Total unmet goals`)) %>%
        select(`Country Code`, country_labels)

      if (length(input$selected_countries) == 0) {
        selected_countries = main_rows$`Country Code` %>% unique()
      } else {
        selected_countries = input$selected_countries
      }

      if (length(input$selected_areas) == 0) {
        selected_areas = combined_partner_areas$Area %>% unique()
      } else {
        selected_areas = input$selected_areas
      }

      countries <- sf::read_sf("data/assets/world_medium.geojson") %>%
        # filter(iso_a2 %in% selected_countries)
        left_join(combined_partner_areas %>%
                    filter(Partner == input$selected_partner) %>%
                    select(Partner, Area, `Country Code`),
                  join_by(iso_a2 == `Country Code`)) %>%
        filter(Area %in% selected_areas)

      V2025_countries <- countries %>%
        left_join(V2025_data, join_by(iso_a2 == `Country Code`)) %>%
        left_join(labels_country, join_by(iso_a2 == `Country Code`))

      all_access_countries <- countries %>%
        left_join(all_access_data, join_by(iso_a2 == `Country Code`)) %>%
        left_join(labels_country, join_by(iso_a2 == `Country Code`))

      lang_markers_df <- main_rows %>%
        # filter(Area %in% selected_areas) %>%
        select(`Country Code`, `Language Code`, `Language Name`, `Translation Status`, `All Access Status`,
               `Is Remaining V2025 Need`, `On All Access List`,
               `EGIDS Group`, Longitude, Latitude) %>%
        filter(`Country Code` %in% selected_countries)

      labels_language <- sprintf(
        "<strong><font size='+1'>%s</font></strong><br/>
         <strong>Translation Status:</strong> %s<br/>
         <strong>All Access Status:</strong> %s<br/>
         <strong>EGIDS:</strong> %s",
        lang_markers_df$`Language Name`,
        lang_markers_df$`Translation Status`,
        lang_markers_df$`All Access Status`,
        lang_markers_df$`EGIDS Group`) %>%
        map(htmltools::HTML)

      labels_name_only <- sprintf(
        "<strong><font size='+1'>%s</font></strong>",
        lang_markers_df$`Language Name`) %>%
        map(htmltools::HTML)

      labels_name_only_small <- sprintf(
        "<span style='font-size:9px'>%s</span>",
        lang_markers_df$`Language Name`) %>%
        map(htmltools::HTML)

      m <- leaflet(countries) %>%
        addTiles(group = "Base - no glosses") %>%
        addProviderTiles(providers$OpenStreetMap.DE,
                         group = "Base features") %>%
        addPolygons(
          data = V2025_countries,
          group = "Vision 2025 Complete",
          fillColor = ~pal_V2025_complete(`V2025_complete`),
          weight = 2, opacity = 0.1, color = "white", dashArray = "3",
          fillOpacity = 0.7,
          label = ~map(V2025_countries$country_labels, HTML),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal",
                         padding = "3px 8px",
                         "background-color" = "tomato"),
            textsize = "15px", direction = "auto"))

      m <- m %>% addPolygons(
          data = V2025_countries,
          group = "1 remaining",
          fillColor = ~pal_one_remaining(`one_remaining`),
          weight = 2, opacity = 0.1, color = "white", dashArray = "3",
          fillOpacity = 0.7,
          label = ~map(V2025_countries$country_labels, HTML),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal",
                         padding = "3px 8px",
                         "background-color" = "tomato"),
            textsize = "15px", direction = "auto"))

      m <- m %>%
        addPolygons(
          data = V2025_countries,
          group = "Remaining by ranges",
          fillColor = ~pal_V2025_buckets(`V2025_bucket`),
          weight = 2, opacity = 0.1, color = "white", dashArray = "3",
          fillOpacity = 0.7,
          label = ~map(V2025_countries$country_labels, HTML),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal",
                         padding = "3px 8px",
                         "background-color" = "tomato"),
            textsize = "15px", direction = "auto"))

      m <- m %>%
        addPolygons(
          data = all_access_countries,
          group = "All Access goals met",
          fillColor = ~pal_AAG_all_met(`All Access goals met`),
          weight = 2, opacity = 0.1, color = "white", dashArray = "3",
          fillOpacity = 0.7,
          label = ~map(all_access_countries$country_labels, HTML),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal",
                         padding = "3px 8px",
                         "background-color" = "tomato"),
            textsize = "15px", direction = "auto"))

      m <- m %>%
        addPolygons(
          data = all_access_countries,
          group = "1 unmet goal",
          fillColor = ~pal_one_unmet(`1 unmet goal`),
          weight = 2, opacity = 0.1, color = "white", dashArray = "3",
          fillOpacity = 0.7,
          label = ~map(all_access_countries$country_labels, HTML),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal",
                         padding = "3px 8px",
                         "background-color" = "tomato"),
            textsize = "15px", direction = "auto"))

      m <- m %>%
        addPolygons(
          data = all_access_countries,
          group = "Unmet goals by ranges",
          fillColor = ~pal_all_access_buckets(`all_access_bucket`),
          weight = 2, opacity = 0.1, color = "white", dashArray = "3",
          fillOpacity = 0.7,
          label = ~map(all_access_countries$country_labels, HTML),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal",
                         padding = "3px 8px",
                         "background-color" = "tomato"),
            textsize = "15px", direction = "auto"))

      color_values <- selected_db_view_palette()$status_value
      color_palette <- selected_db_view_palette()$color
      marker_palette <- colorFactor(palette = color_palette,
                                    levels = color_values)
      # req(color_values)
      sel_mkr_status <- input$selected_marker_status
      selected_status <- case_when(
        sel_mkr_status == "Vision 2025 Status" ~ "Translation Status",
        sel_mkr_status == "All Access Status" ~ "All Access Status",
        sel_mkr_status == "Is Remaining V2025 Need" ~ "Is Remaining V2025 Need",
        sel_mkr_status == "All Access Goal Not Met" ~ "On All Access List"
        # sel_mkr_status == "On All Access List" ~ "On All Access List"
      )
      status_values <- lang_markers_df[[selected_status]]

      m <- m %>%
        addCircleMarkers(
          # layerId = "lang_markers", # beware! causes only one marker, the last, to be added!
          data = lang_markers_df,
          group = "Languages - clustered",
          lng = lang_markers_df$Longitude,
          lat = lang_markers_df$Latitude,
          popup = labels_language,
          label = labels_name_only,
          labelOptions = labelOptions(
            textOnly = TRUE,
            noHide = TRUE,
            # direction = "auto",
            direction = "bottom",
            style = list(
              "color" = "blue",
              "font-family" = "serif",
              "font-size" = "10px",
              "font-weight" = "bold")),
          radius = 2,
          weight = 5,
          opacity = 0.9,
          clusterOptions = markerClusterOptions()
          , color = ~marker_palette(status_values)
        )

      m <- m %>%
        addCircleMarkers(
          # layerId = "lang_markers", # beware! causes only one marker, the last, to be added!
          data = lang_markers_df,
          group = "Languages - markers only",
          lng = lang_markers_df$Longitude,
          lat = lang_markers_df$Latitude,
          label = labels_language,
          # labelOptions = labelOptions(
          #   textOnly = TRUE,
          #   noHide = TRUE,
          #   # direction = "auto",
          #   direction = "bottom",
          #   style = list(
          #     "color" = "blue",
          #     "font-family" = "serif",
          #     "font-size" = "12px",
          #     "font-weight" = "bold")),
          radius = 2,
          weight = 4,
          opacity = 0.9,
          clusterOptions = NULL
          , color = ~marker_palette(status_values)
          # , color = ~marker_palette(lang_markers_df[[input$selected_db_view]])
        )

      # m <- m %>%
      #   addCircleMarkers(
      #      data = lang_markers_df,
      #      group = "Languages - markers & labels",
      #      lng = lang_markers_df$Longitude,
      #      lat = lang_markers_df$Latitude,
      #      popup = labels_language,
      #      label = labels_name_only_small,
      #      labelOptions = labelOptions(
      #        textOnly = TRUE,
      #        noHide = TRUE,
      #        # direction = "auto",
      #        direction = "bottom",
      #        style = list(
      #          "color" = "blue",
      #          "font-family" = "sans-serif",
      #          # "font-size" = "1em",
      #          "font-size" = "9px",
      #          "font-weight" = "bold")),
      #      radius = 2,
      #      weight = 4,
      #      opacity = 0.9,
      #      clusterOptions = NULL,
      #      color = ~marker_palette(status_values))



      ## commented out. This feature is too slow when a large number of areas and countries are selected
      # m <- m |>
      #   addLabelOnlyMarkers(
      #     data = lang_markers_df,
      #     group = "Languages - names only",
      #     lng = lang_markers_df$Longitude,
      #     lat = lang_markers_df$Latitude,
      #     label = labels_name_only,
      #     labelOptions = labelOptions(
      #       textOnly = TRUE,
      #       noHide = TRUE,
      #       direction = "auto",
      #       # direction = "bottom",
      #       style = list(
      #         "color" = "blue",
      #         "font-family" = "serif",
      #         "font-size" = "2px",
      #         "font-weight" = "bold"))
      #   )

      m <- m %>%
        addLegend(pal = pal_V2025_buckets, values = ~V2025_countries$V2025_bucket, opacity = 0.7, title = NULL,
                  position = "bottomright",
                  group = "Remaining by ranges") %>%
        addLegend(pal = pal_one_remaining, values = ~V2025_countries$`one_remaining`, opacity = 0.7, title = NULL,
                  position = "bottomright",
                  group = "1 remaining") %>%
        addLegend(pal = pal_V2025_complete, values = ~V2025_countries$`V2025_complete`, opacity = 0.7, title = NULL,
                  position = "bottomright",
                  group = "Vision 2025 Complete") %>%

        addLegend(pal = pal_all_access_buckets, values = ~all_access_countries$`all_access_bucket`, opacity = 0.7, title = NULL,
                  position = "bottomright",
                  group = "Unmet goals by ranges") %>%
        addLegend(pal = pal_one_unmet, values = ~all_access_countries$`1 unmet goal`, opacity = 0.7, title = NULL,
                  position = "bottomright",
                  group = "1 unmet goal") %>%
        addLegend(pal = pal_AAG_all_met, values = ~all_access_countries$`All Access goals met`, opacity = 0.7, title = NULL,
                  position = "bottomright",
                  group = "All Access goals met") |>
        addLegend(pal = colorFactor(palette = color_palette,
                                    levels = color_values),
                  position = "bottomright",
                  values = color_values,
                  title = paste0("Markers - ", selected_status),
                  opacity = 0.9,
                  group = "Languages - clustered") |>
        addLegend(pal = colorFactor(palette = color_palette,
                                    levels = color_values),
                  position = "bottomright",
                  values = color_values,
                  title = paste0("Markers - ", selected_status),
                  opacity = 0.9,
                  group = "Languages - markers only")
      # |>
      #   addLegend(pal = colorFactor(palette = color_palette,
      #                               levels = color_values),
      #             position = "bottomright",
      #             values = color_values,
      #             title = paste0("Markers - ", selected_status),
      #             opacity = 0.9,
      #             group = "Languages - markers & labels")


      m <- m %>%
        addLayersControl(
          baseGroups = c("Base features", "Base - no glosses"),
          # overlayGroups = c("Vision 2025 Complete", "1 remaining", "Remaining by ranges",
          #                   "All Access goals met", "1 unmet goal", "Unmet goals by ranges",
          #                   "Languages - clustered", "Languages - markers only",
          #                   "Languages - markers & labels"),
          overlayGroups = c("Vision 2025 Complete", "1 remaining", "Remaining by ranges",
                            "All Access goals met", "1 unmet goal", "Unmet goals by ranges",
                            "Languages - clustered", "Languages - markers only"),
          # overlayGroups = c("Vision 2025 Complete", "1 remaining", "Remaining by ranges",
          #                   "All Access goals met", "1 unmet goal", "Unmet goals by ranges",
          #                   "Languages - clustered", "Languages - markers only", "Languages - names only"),
          option = layersControlOptions(collapsed = FALSE))

      m <- m %>%
        hideGroup(c("Vision 2025 Complete", "1 remaining",
                    "All Access goals met", "1 unmet goal", "Unmet goals by ranges",
                    "Languages - clustered", "Languages - markers only", "Languages - markers & labels"))
        # hideGroup(c("Vision 2025 Complete", "1 remaining",
        #             "All Access goals met", "1 unmet goal", "Unmet goals by ranges",
        #             "Languages - clustered", "Languages - markers only", "Languages - names only"))

      # m <- m |>
      #   groupOptions("Languages - names only", zoomLevels = 8:20)

      m <- m %>%
        fitBounds(lng1 = countries$label_x %>% max(),
                  lng2 = countries$label_x %>% min(),
                  lat1 = countries$label_y %>% max(),
                  lat2 = countries$label_y %>% min())
    })

    output$btn_send_to_langMap <- renderUI({
      actionButton("btn_send2langMap",
                   label = tagList(span("Send to Language Map"),
                                   span(" NEW!", style = "font-size: 12px; color: red; font-weight: bold;")),
                   # label = "Send to Language Map",
                   icon = icon("globe"))
    })

    output$btn_clear_reset_rt <- renderUI({
      actionButton("btn_clear_search",
                   label = "Clear search and reset Main Table",
                   icon = icon("broom"))
    })

    observe({
      updateTabsetPanel(session, "main_page_tabset", selected = "Language Map")

      curr_rt_row_indices <- input$research_table_rows_all
      curr_rt_df <- main_rows_reactive()[curr_rt_row_indices, ]

      curr_langs <- curr_rt_df$`Language Code`
      data <- maps_table |> filter(`Language Code` %in% curr_langs)

      color_values <- selected_status_palette()$status_value
      color_palette <- selected_status_palette()$color
      marker_palette <- colorFactor(palette = color_palette,
                                    levels = color_values)
      req(color_values)
      req(color_palette)

      leafletProxy("langMap1", session) |>
        clearGroup("Languages - clustered") |>
        addCircleMarkers(lng = data$Longitude,
                         lat = data$Latitude,
                         data = data,
                         # radius = 1,
                         radius = 4,
                         weight = 5,
                         opacity = 0.9,
                         popup = ~map(data$html_popup_text, HTML),
                         label = ~map(data$html_label_static, HTML),
                         labelOptions = labelOptions(
                           textOnly = TRUE,
                           noHide = TRUE,
                           # direction = "auto",
                           direction = "bottom",
                           style = list(
                             "color" = "blue",
                             "font-family" = "serif",
                             "font-size" = "12px",
                             "font-weight" = "bold")),
                         clusterOptions = TRUE,
                         # clusterOptions = markerClusterOptions(),
                         # layerId = data$rowID,
                         group = "Languages - clustered",
                         color = ~marker_palette(data[[status_type_selected()]])) %>%

        clearGroup("Languages - markers only") |>
        addCircleMarkers(lng = data$Longitude,
                         lat = data$Latitude,
                         data = data,
                         radius = 2,
                         weight = 5,
                         opacity = 0.9,
                         popup = ~map(data$html_popup_text, HTML),
                         label = ~map(data$html_label_static, HTML),
                         clusterOptions = NULL,
                         group = "Languages - markers only",
                         color = ~marker_palette(data[[status_type_selected()]]))

    }) |> bindEvent(input$btn_send2langMap)

    output$langMap1 <- renderLeaflet({
      langMap_reactive()
    })

    selected_map_rows <- reactive({
      rows_selected <- getReactableState("map_table", "selected")
      if (!is.null(rows_selected)) {
        rows_selected
      } else {
        NULL
      }
      return(rows_selected)
    })

    langMap_reactive <- reactive({
        curated_table <- selected_status_data()
        lng1 <- curated_table$Longitude %>% max(na.rm = TRUE) |> suppressWarnings()
        lng2 <- curated_table$Longitude %>% min(na.rm = TRUE) |> suppressWarnings()
        lat1 <- curated_table$Latitude %>% max(na.rm = TRUE) |> suppressWarnings()
        lat2 <- curated_table$Latitude %>% min(na.rm = TRUE) |> suppressWarnings()
        color_values <- selected_status_palette()$status_value
        color_palette <- selected_status_palette()$color
        marker_palette <- colorFactor(palette = color_palette,
                                      levels = color_values)
        req(color_values)
        req(color_palette)

        # limit markers to those language selected in main table, or all if none are selected.
        curated_table <- if (values$val_plot_selected_only) {
          langs_selected <- map_table_langs_selected()
          out <- curated_table %>%
            filter(`Language Name` %in% langs_selected)
          } else {
            out <- curated_table
          }

        map <- leaflet(
            # data = curated_table,
            options = leafletOptions(preferCanvas = FALSE)) %>%
            # options = leafletOptions(preferCanvas = TRUE)) %>%

            addTiles(group = "Base - no glosses") %>%
            addProviderTiles(providers$OpenStreetMap.DE,
                             group = "Base features") %>%
            # addProviderTiles(providers$OpenStreetMap.Mapnik,
            #                  group = "Base features") %>%
            addProviderTiles(providers$OpenTopoMap,
                             group = "Show topography") %>%
            addProviderTiles(providers$Esri.WorldGrayCanvas,
                             group = "Minimal features") %>%

          addPolygons(
            layerId = timezones_sf$objectid,
            group = "Time Zone",
            data = timezones_sf,
            fillColor = "white",
            stroke = TRUE,
            weight = 1,
            smoothFactor = 0.2,
            fillOpacity = 0,
            label = ~paste0(timezones_sf$name)) %>%

          hideGroup("Time Zone") %>%

          addPolygons(
            layerId = SLI_Polygons$fid,
            stroke = TRUE,
            weight = 1,
            fill = TRUE,
            fillColor = ~resource_lang_palette(SLI_Polygons$Resource.Level),
            fillOpacity = 0.3,
            dashArray = "4",
            group = "Resource Languages",
            data = SLI_Polygons,
            # data = SLI_Polygons$geometry,
            label = paste0(SLI_Polygons$Language.Name,
                           "\n",
                           SLI_Polygons$Resource.Level)
          ) %>%

          addLegend(
            title = "Resource Level",
            position = "bottomright",
            values = SLI_Polygons$Resource.Level,
            pal = resource_lang_palette,
            # suffix = c("0: not specified", "1 Regional LWC", "2 Official/Nat'l/Widespread", "3: Int'l/Regional", "4: Global"),
            opacity = 0.6,
            layerId = "sli_legend",
            group = "Resource Languages"
          ) %>%

          removeControl("sli_legend") %>%
          hideGroup("Resource Languages") %>%

          # addPolygons(
          #   layerId = aPG_polygons$PEID,
          #   stroke = TRUE,
          #   weight = 1,
          #   fill = TRUE,
          #   fillColor = ~groups_palette(aPG_polygons$LvlBible),
          #   fillOpacity = 0.3,
          #   dashArray = "4",
          #   group = "People Groups",
          #   data = aPG_polygons,
          #   # data = SLI_Polygons$geometry,
          #   label = paste0(aPG_polygons$`Group Name`,
          #                  "\n",
          #                  aPG_polygons$LvlBible)
          # ) %>%

          # addLegend(
          #   title = "People Group",
          #   position = "bottomright",
          #   values = aPG_polygons$LvlBible,
          #   pal = groups_palette,
          #   opacity = 0.6,
          #   layerId = "groups_legend",
          #   group = "People Groups"
          # ) %>%

          # removeControl("groups_legend") %>%
          # hideGroup("People Groups") %>%

          addCircleMarkers(lng = curated_table$Longitude,
                           lat = curated_table$Latitude,
                           data = curated_table,
                           # radius = 1,
                           radius = 4,
                           weight = 5,
                           opacity = 0.9,
                           popup = ~map(curated_table$html_popup_text, HTML),
                           label = ~map(curated_table$html_label_static, HTML),
                           labelOptions = labelOptions(
                              textOnly = TRUE,
                              noHide = TRUE,
                              # direction = "auto",
                              direction = "bottom",
                              style = list(
                                  "color" = "blue",
                                  "font-family" = "serif",
                                  "font-size" = "12px",
                                  "font-weight" = "bold")),
                           clusterOptions = TRUE,
                           # clusterOptions = markerClusterOptions(),
                           # layerId = curated_table$rowID,
                           group = "Languages - clustered",
                           color = ~marker_palette(curated_table[[status_type_selected()]])) %>%

          addCircleMarkers(lng = curated_table$Longitude,
                           lat = curated_table$Latitude,
                           data = curated_table,
                           radius = 2,
                           weight = 5,
                           opacity = 0.9,
                           popup = ~map(curated_table$html_popup_text, HTML),
                           label = ~map(curated_table$html_label_static, HTML),
                           # labelOptions = labelOptions(
                           #   textOnly = TRUE,
                           #   noHide = TRUE,
                           #   # direction = "auto",
                           #   direction = "bottom",
                           #   style = list(
                           #     "color" = "blue",
                           #     "font-family" = "serif",
                           #     "font-size" = "1em",
                           #     # "font-size" = "12px",
                           #     "font-weight" = "bold")),
                           clusterOptions = NULL,
                           # clusterOptions = markerClusterOptions(),
                           # layerId = curated_table$rowID,
                           group = "Languages - markers only",
                           color = ~marker_palette(curated_table[[status_type_selected()]])) |>

          # addCircleMarkers(lng = curated_table$Longitude,
          #                  lat = curated_table$Latitude,
          #                  data = curated_table,
          #                  radius = 2,
          #                  weight = 4,
          #                  opacity = 0.9,
          #                  popup = ~map(curated_table$html_popup_text, HTML),
          #                  label = ~map(curated_table$html_label_static, HTML),
          #                  labelOptions = labelOptions(
          #                    textOnly = TRUE,
          #                    noHide = TRUE,
          #                    # direction = "auto",
          #                    direction = "bottom",
          #                    style = list(
          #                      "color" = "blue",
          #                      "font-family" = "sans-serif",
          #                      # "font-size" = "1em",
          #                      "font-size" = "9px",
          #                      "font-weight" = "bold")),
          #                  clusterOptions = NULL,
          #                  # clusterOptions = markerClusterOptions(),
          #                  # layerId = curated_table$rowID,
          #                  group = "Languages - markers & labels",
          #                  color = ~marker_palette(curated_table[[status_type_selected()]])) |>

          addLegend(pal = colorFactor(palette = color_palette,
                                        levels = color_values),
                      position = "bottomright",
                      values = color_values,
                      title = paste0("Markers - ", status_type_selected()),
                      opacity = 0.9) %>%

          # addLayersControl(baseGroups = c("Languages - clustered",
          #                                 "Languages - markers only"),
          #                  position = "bottomleft",
          #                  options = layersControlOptions(collapsed = FALSE)) %>%

          addLayersControl(baseGroups = c("Base features",
                                          "Base - no glosses",
                                          "Show topography",
                                          "Minimal features"),
                           overlayGroups = c("Languages - clustered",
                                             "Languages - markers only",
                                             # "Languages - markers & labels",
                                             "Resource Languages",
                                             "Time Zone"
                                             ),
                           position = "topright",
                           options = layersControlOptions(collapsed = FALSE)) %>%

          hideGroup(c("Languages - markers only")) |>
          # hideGroup(c("Languages - markers only", "Languages - markers & labels")) |>

          addDrawToolbar(
              targetGroup='Selected',
              polylineOptions=FALSE,
              markerOptions = FALSE,
              polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0.1,
                                                                                fillColor = "yellow",
                                                                                color = 'white',
                                                                                weight = 3)),
              rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0.1,
                                                                                    fillColor = "yellow",
                                                                                    color = 'white',
                                                                                    weight = 3)),
              # circleOptions = drawCircleOptions(shapeOptions = drawShapeOptions(fillOpacity = 0.1,
              #                                                                   fillColor = "yellow",
              #                                                                   color = 'white',
              #                                                                   weight = 3)),
              circleOptions = FALSE,
              circleMarkerOptions = FALSE,
              editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions())) %>%

          # addSearchFeatures(targetGroups = c("Language"),
          #                   searchFeaturesOptions(propertyName = "label",
          #                                         initial = FALSE,
          #                                         openPopup = TRUE,
          #                                         autoCollapse = TRUE,
          #                                         zoom = 5)) %>%
          # addMiniMap(
          #   width = 190,
          #   height = 190,
          #   zoomLevelOffset = -6,
          #   toggleDisplay = TRUE,
          #   minimized = TRUE
          # ) %>%

          fitBounds(lng1 = lng1,
                    lng2 = lng2,
                    lat1 = lat1,
                    lat2 = lat2)

        map
    })


    langMap_for_export <- reactive({
      langMap_reactive() %>%
        addControl(# html = rr,
          html = tagList(
            div(
              style = "background-color: transparent",
              h1('For internal use only. Do not share publicly.'),
              div(style = "font-size: 10px; width: 80%",
                  # verbatimTextOutput("citation_text")
                  "Data provided by the following sources: ProgressBible™ on `r max(data_list$date_accessed`; Eberhard, David M., Gary F. Simons, and Charles D. Fennig (eds.). 2022. Ethnologue: Languages of the World. Twenty-fifth edition. Dallas, Texas: SIL International. Online version: http://www.ethnologue.com; Glottolog 4.4 edited by Hammarström, Harald & Forkel, Robert & Haspelmath, Martin & Bank, Sebastian; Alan Starling (ed.). 2021. Registry of Language Varieties. Online version (https://globalrecordings.net/en/rod)."
              )
            )
          ),
          position = "bottomleft")
          # position = "bottomleft",
          #          className = "map-citation-footer")
    })

    summary_map_for_export <- reactive({
      summary_map_reactive() %>%
        addControl(# html = rr,
          html = tagList(
            div(
              style = "background-color: transparent",
              h2('For internal use only. Do not share publicly.'),
              div(style = "font-size: 10px; width: 80%",
                  # verbatimTextOutput("citation_text")
                  "Data provided by the following sources: ProgressBible™ on `r max(data_list$date_accessed`; Eberhard, David M., Gary F. Simons, and Charles D. Fennig (eds.). 2022. Ethnologue: Languages of the World. Twenty-fifth edition. Dallas, Texas: SIL International. Online version: http://www.ethnologue.com; Glottolog 4.4 edited by Hammarström, Harald & Forkel, Robert & Haspelmath, Martin & Bank, Sebastian; Alan Starling (ed.). 2021. Registry of Language Varieties. Online version (https://globalrecordings.net/en/rod)."
              )
            )
          ),
          position = "bottomleft")
          # position = "bottomleft",
          #          className = "map-citation-footer")
    })


    output$download_langMap <- downloadHandler(
      filename = function() {
        paste0("mesa_map_export_", Sys.Date(), ".html")
        # paste0("mesa_map_export_", Sys.Date(), ".png")
        # paste0("mesa_map_export_", Sys.Date(), ".jpeg")
        # "language_map_export.html"
      },
      content = function(file) {
        id <- showNotification("Please wait. Preparing download...",
                         type = "warning", duration = NULL)
        url <- tempfile(pattern = "langMap_export",
                        tmpdir = tempdir(),
                        fileext = ".html")

        # graphic_file <- tempfile(pattern = "langMap_export",
        #                 tmpdir = tempdir(),
        #                 fileext = ".jpeg")
        #                 # fileext = ".png")
        #                 # fileext = ".pdf")

        # mapview::mapshot2(
        mapview::mapshot(
          langMap_for_export(),
          url = url,
          # file = graphic_file,
          selfcontained = TRUE,
          remove_controls = c("homeButton", "scaleBar", "drawToolbar", "easyButton"),
        )

        file.copy(url, file)
        # file.copy(graphic_file, file)
        removeNotification(id)
        showNotification("File saved to browser's downloads folder.",
                         type = "message", duration = 5)
      }
    )

    output$download_summary_map_html <- downloadHandler(
      filename = function() {
        paste0("mesa_summary_map_", Sys.Date(), ".html")
      },
      content = function(file) {
        id <- showNotification("Please wait. Preparing download...",
                         type = "warning", duration = NULL)
        url <- tempfile(pattern = "summary_map_export",
                        tmpdir = tempdir(),
                        fileext = ".html")

        # mapview::mapshot2(
        mapview::mapshot(
          summary_map_for_export(),
          url = url,
          # file = graphic_file,
          selfcontained = TRUE,
          remove_controls = c("homeButton", "scaleBar", "drawToolbar", "easyButton"),
        )

        file.copy(url, file)
        # file.copy(graphic_file, file)
        removeNotification(id)
        showNotification("File saved to browser's downloads folder.",
                         type = "message", duration = 5)
      }
    )

    output$download_summary_map_png <- downloadHandler(
      filename = function() {
        paste0("mesa_summary_map_", Sys.Date(), ".png")
      },
      content = function(file) {
        id <- showNotification("Please wait. Preparing download...",
                         type = "warning", duration = NULL)
        png_file <- tempfile(pattern = "summary_map_export",
                        tmpdir = tempdir(),
                        fileext = ".png")

        map <- summary_map_for_export()
        map$x$options = append(map$x$options, list("zoomControl" = FALSE))

        # mapview::mapshot2(
        mapview::mapshot(
          map,
          # summary_map_for_export(),
          # url = url,
          file = png_file,
          # selfcontained = TRUE,
          remove_controls = c("zoomControl", "homeButton", "scaleBar", "drawToolbar", "easyButton"),
        )

        file.copy(png_file, file)
        # file.copy(graphic_file, file)
        removeNotification(id)
        showNotification("File saved to browser's downloads folder.",
                         type = "message", duration = 5)
      }
    )

    # lang_coords_sf <- sf::st_as_sf(maps_table, coords = c("Longitude", "Latitude"), crs = 4326)
    # lang_coords <-  sp::SpatialPointsDataFrame(maps_table[,c('Longitude', 'Latitude')] , maps_table)

    # lang_coords_sf <- maps_table |>
    #   select(`Language Code`, Longitude, Latitude, rowID) |>
    #   sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) |>
    #   sf::st_as_sfc()
    # lang_coords <-  sp::SpatialPointsDataFrame(maps_table[,c('Longitude', 'Latitude')] , maps_table)

    # subset maps_table for findLocations/findIntersects
    # lang_coords_df <- reactive({
    #   out <- maps_table |>
    #     select(`Country Code`, `Language Code`, Longitude, Latitude, rowID) |>
    #     filter(`Country Code` %in% input$selected_countries)
    #
    #   return(out)
    # })


    # Event triggered when a new selection is created
    observeEvent(input$langMap1_draw_new_feature,{
      #Only add language codes for bounded locations
      shape <- input$langMap1_draw_new_feature
      # found_in_bounds <- findLocations(shape = shape,
      #                                  loc_coords_sf = lang_coords_sf,
      #                                  location_id_colname = "rowID")

      # lang_coords_df = lang_coords_df()

      found_in_bounds <- findIntersects(shape = shape,
                                        lang_coords_df = lang_coords_df,
                                        location_id_colname = "rowID")
      # found_in_bounds <- findLocations(shape = shape,
      #                                  lang_coords_df = lang_coords_df,
      #                                  location_id_colname = "rowID")

      for(id in found_in_bounds){
          if(id %in% data_of_click$clickedMarker){
              # don't add id
          } else {
              # add id
              data_of_click$clickedMarker <- append(data_of_click$clickedMarker, id, 0)
          }
      }
    })

    # Event triggered when an existing selection is deleted
    observeEvent(input$langMap1_draw_deleted_features,{
        # loop through list of one or more deleted features/ polygons

        for(feature in input$langMap1_draw_deleted_features$features){

            # get ids for locations within the bounding shape
            # bounded_layer_ids <- findLocations(shape = feature,
            #                                    loc_coords_sf = lang_coords_sf,
            #                                    location_id_colname = "rowID")

            bounded_layer_ids <- findIntersects(shape = feature,
                                              lang_coords_df = lang_coords_df,
                                              location_id_colname = "rowID")

            row_ids_to_delete <- subset(lang_coords_df, rowID %in% bounded_layer_ids)$rowID


            data_of_click$clickedMarker <- data_of_click$clickedMarker[!data_of_click$clickedMarker
                                                                       %in% row_ids_to_delete]
        }
    })

    # this resets the data_of_click cache whenever selected countries are changed
    observe({ # bound to input$selected_countries
      lang_coords_df <<- maps_table |>
        select(`Country Code`, `Language Code`, Longitude, Latitude, rowID) |>
        filter(`Country Code` %in% input$selected_countries)
      sel_countries_rowIDs <- lang_coords_df %>%
        select(`Country Code`, rowID)
      # sel_countries_rowIDs <- main_rows %>%
      #   filter(`Country Code` %in% input$selected_countries) %>%
      #   select(`Country Code`, rowID)
      data_of_click$clickedMarker <- data_of_click$clickedMarker[data_of_click$clickedMarker
                                                                 %in% sel_countries_rowIDs]
    }) %>% bindEvent(input$selected_countries)

    # this produces the list of languages to add to the table when enclosing shapes are added
    selectedLocations <- reactive({
      df <- main_rows_reactive()
      # df <- main_rows %>% filter(`Country Code` %in% input$selected_countries)
      selectedLocations <- subset(df, rowID %in% data_of_click$clickedMarker) |>
        select(-rowID)
      return(selectedLocations)
    })

    output$btn_copy_languages <- renderUI({
      rclipButton(
        inputId = "btn_clip_highlighted_languages",
        label = "Copy highlighted languages",
        clipText = selectedLocations() %>%
          pull(`Language Name`) %>% str_trim() %>% str_c(collapse = "; "),
        icon = icon("clipboard"))
    })

    output$highlighted_langs_list <- render_gt({
      req(selectedLocations())
      # input$selected_countries
      highlighted_names <- selectedLocations() %>%
        # filter(`Country Code` %in% input$selected_countries) %>%
        pull(`Language Name`) %>% str_trim() %>% str_c(collapse = "; ")
      highlighted_names_df <- tibble(
        `Highlighted Languages: ` = highlighted_names
      )
      table <- gt(highlighted_names_df)
      return(table)
    }, align = "left", height = px(200))

  map_table_data <- reactive({
    # req(values$stored_fields)
    fields <- values$stored_fields

    if (is.null(fields)) { # minimum fields are the default_fields
      fields <- default_fields
    }

    if (values$val_list_highlighted_only) {
      lang_names <- selectedLocations() %>%
        pull(`Language Name`)
    } else {
      lang_names <- main_rows_reactive() %>%
      # lang_names <- main_rows %>%
        pull(`Language Name`)
    }

    df <- main_rows %>%
      filter(`Language Name` %in% lang_names) %>%
      filter(`Country Code` %in% input$selected_countries) %>%
      select(Country, `Language Name`, `Language Code`, all_of(fields))

    return(df)
  })

  observeEvent(input$table_options, {
    showModal(map_options_modal())
  })

  observeEvent(input$plot_selected_only, {
    values$val_plot_selected_only <- input$plot_selected_only
  })

  observeEvent(input$list_highlighted_only, {
    values$val_list_highlighted_only <- input$list_highlighted_only
  })

  map_options_modal <- function() {
    modalDialog(
      checkboxInput("plot_selected_only",
        label = "On map, display only languages selected on table.  (Uncheck to display all languages in selected countries.)",
        value = values$val_plot_selected_only,
        width = "100%"),
      checkboxInput("list_highlighted_only",
        label = "In main table, include only languages highlighted on map. (Uncheck to include all languages)",
        value = values$val_list_highlighted_only,
        width = "100%"),
      title = "Map Table Options",
      footer = modalButton("Done"),
      size = c("m"),
      # size = c("m", "s", "l", "xl"),
      easyClose = TRUE,
      fade = TRUE
    )
  }

  output$map_table_DT <- renderDT({
    # df <- main_rows_reactive()
    df <- map_table_data()
    get_DT_main_obj(df)
  }, server = FALSE)

  observe({ # dependency on input$map_table_DT_rows_selected
    rows_selected <- input$map_table_DT_rows_selected
    if (!is.null(rows_selected)) {
      df <- map_table_data() %>%
        slice(rows_selected) %>% pull(`Language Name`)
    }
  })

  map_table_langs_selected <- reactive({
    rows_selected <- input$map_table_DT_rows_selected
    if (!is.null(rows_selected)) {
      out <- selected_status_data() %>%
        slice(rows_selected) %>% pull(`Language Name`)
    } else {
      out <- selected_status_data() %>% pull(`Language Name`)
    }
    return(out)
  })


  output$map_table_details <- renderUI({

    rows_selected <- input$map_table_DT_rows_selected
    # str(rows_selected)
    # rows_selected <- req(getReactableState("map_table", "selected"))

    main_df <- isolate(map_table_data())

    details_lang_codes <- if (!is.null(rows_selected)) {
      main_df %>%
      slice(rows_selected) %>%
      pull(`Language Code`)
    } else {
      #
      "none"
    }

    nested_tables_list <- list(
      "WIP" = filter(WIP_rows, `Language Code` %in% details_lang_codes),
      "SP" = filter(SP_rows, `Language Code` %in% details_lang_codes),
      "WCD" = filter(WCD_rows, `Language Code` %in% details_lang_codes),
      "ROLV" = filter(ROLV_rows, `Language Code` %in% details_lang_codes)
      # ,
      # "Ethnologue" = filter(Ethnologue_rows, `Language Code` %in% details_lang_codes)
    )

    nested_tables_nrows <- list(
      "WIP" = nested_tables_list$WIP %>% nrow(),
      "SP" = nested_tables_list$SP %>% nrow(),
      "WCD" = nested_tables_list$WCD %>% nrow(),
      "ROLV" = nested_tables_list$ROLV %>% nrow()
      # ,
      # "Ethnologue" = nested_tables_list$Ethnologue %>% nrow()
    )

    nested_tables_name <- list(
      "WIP" = "Work in Progress",
      "SP" = "Scripture Products",
      "WCD" = "World Christian Database",
      "ROLV" = "Registry of Language Varieties"
      # ,
      # "Ethnologue" = "Ethnologue"
    )

    nested_tables_df <- tibble(
      id = names(nested_tables_name),
      tab_label = paste(nested_tables_name," (",nested_tables_nrows,")"),
      nrows = nested_tables_nrows,
      data = nested_tables_list)

    rm(nested_tables_list)

    tabsetPanel(id = "RT_details",
                selected = NULL,
                type = c("pills"),
                tabPanel(value = "WIP",
                         title = paste("Work In Progress (", nested_tables_nrows$WIP, ")"),
                         {
                           # df = get_details_data("WIP", nested_tables_df)
                           data = nested_tables_df %>%
                             filter(id == "WIP") %>%
                             unnest(cols = c(data)) %>%
                             select(-id, -tab_label, -nrows)
                             # select(-id, -tab_label, -nrows, -`Language Code`)
                           rowCount = nrow(data)
                           # rowCount = nested_tables_df %>% filter(id == "WIP") %>% pull(nrows)
                           if(rowCount != 0) {
                             get_DT_details_obj(data)
                             # datatable(data = data)

                           } else {
                             "No data"
                           }
                         }
                ),
                tabPanel(value = "SP",
                         title = paste("Scripture Products (", nested_tables_nrows$SP, ")"),
                         {
                           # data <- get_details_data("SP", nested_tables_df)
                           data = nested_tables_df %>%
                             filter(id == "SP") %>%
                             unnest(cols = c(data)) %>%
                             select(-id, -tab_label, -nrows, -`Language Code`)
                           rowCount = nested_tables_df %>% filter(id == "SP") %>% pull(nrows)
                           if(rowCount != 0) {
                             get_DT_details_obj(data)
                             # datatable(data = nested_tables_df %>%
                             #             filter(id == "SP") %>%
                             #             unnest(cols = c(data)) %>%
                             #             select(-id, -tab_label, -nrows))
                             # reactable(data = filter(SP_rows, `Language Code` == lang_code),
                             #           fullWidth = TRUE)
                           } else {
                             "No data"
                           }
                         }
                ),

                tabPanel(value = "WCD",
                         title = paste("World Christian Database (", nested_tables_nrows$WCD, ")"),
                         {
                           # data <- get_details_data("WCD", nested_tables_df)
                           data = nested_tables_df %>%
                             filter(id == "WCD") %>%
                             unnest(cols = c(data)) %>%
                             select(-id, -tab_label, -nrows, -`Language Code`)
                           rowCount = nested_tables_df %>% filter(id == "WCD") %>% pull(nrows)
                           if(rowCount != 0) {
                             get_DT_details_obj(data)
                             # datatable(data = nested_tables_df %>%
                             #             filter(id == "WCD") %>%
                             #             unnest(cols = c(data)) %>%
                             #             select(-id, -tab_label, -nrows))
                             # reactable(data = filter(WCD_rows, `Language Code` == lang_code),
                             #           fullWidth = TRUE)
                           } else {
                             "No data"
                           }
                         }
                ),
                tabPanel(value = "ROLV",
                         title = paste("Registry of Language Varieties (", nested_tables_nrows$ROLV, ")"),
                         {
                           # data <- get_details_data("ROLV", nested_tables_df)
                           data <- nested_tables_df |>
                             filter(id == "ROLV") %>%
                             unnest(cols = c(data)) |>
                             rename(`Variety Name` = `Variety Name (ROLV)`,
                                    `Variety Code` = `Variety Code (ROLV)`,
                                    `Location (ROLV)` = `Location Name (ROLV)`) |>
                             select(`Location (ROLV)`, `Language Name (ROLV)`, `Language Code`,
                                    `Variety Name`, `Variety Code`) |>
                             arrange(`Location (ROLV)`, `Language Code`)
                           # select(-id, -tab_label, -nrows, -`Language Code`)
                           rowCount = nested_tables_df %>% filter(id == "ROLV") %>% pull(nrows)
                           if(rowCount != 0) {
                             get_DT_details_obj(data)
                           } else {
                             "No data"
                           }
                         }
                )
    )

  })

    lang_status_clickData <- reactive({
      req(input$selected_status_field)
      req(countries_selected()$`Country Code`)

      chosen_status <- input$selected_status_field
      countries <- countries_selected()$`Country Code` %>% unique()


      plot_data <- status_vars2 %>%
          select(Country, `Country Code`, `Language Name`, .data[[chosen_status]]) %>%
          filter(`Country Code` %in% countries) %>%
          group_by(.data[[chosen_status]], `Country`) %>%
          summarise(`Language count` = n()) %>%
          arrange(Country)

      country_list <- unique(as.character(plot_data$Country))
      status_list <- unique(as.character(plot_data[[1]])) %>%
          sort()

      currentEventData <- unlist(event_data(event = "plotly_click", source = "B", priority = "event"))

      req(currentEventData)
      target_country <- country_list[[currentEventData[[1]]+1]]
      target_status <- status_list[[currentEventData[[4]]]]

      table <- main_rows  %>%
          filter(Country == !!target_country,
                 .data[[chosen_status]] == !!target_status) %>%
          select(all_stored_fields())
      table

      return(table)
    })

    output$status_fields <- renderUI({
      radioButtons(inputId="selected_status_field",
                   label="Select desired language status field:",
                   choices = status_field_list,
                   selected = "Translation Status",
                   inline = TRUE,
                   width = NULL)
    })

    output$plot_status_dist <- renderPlotly({
        req(input$selected_status_field)
        req(countries_selected()$`Country Code`)
        chosen_status <- input$selected_status_field
        countries <- countries_selected()$`Country Code` %>% unique()

        plot_data <- status_vars2 %>%
            select(Country, `Country Code`, `Language Name`, .data[[chosen_status]]) %>%
            filter(`Country Code` %in% countries) %>%
            group_by(.data[[chosen_status]], `Country`) %>%
            summarise(`Language count` = n()) %>%
            arrange(Country)

        n_plot <- plot_data %>%
            ggplot2::ggplot(aes( y = plot_data[[chosen_status]], x = `Language count`, fill = Country), size = 6) +
            geom_col() +
            theme(axis.text.y = element_text(size = 11)) +
            labs(y = chosen_status)

        ggplotly(n_plot,
                 height = 400,
                 tooltip = c("Country", "Language count"),
                 source = "B") %>%
            plotly::config(displayModeBar=FALSE)

    })

    output$table_status_dist <- renderDT({
        # uses the same fields chosen for the Table Builder above.
        # fields <- fields_reactive()
        # table <- as_tibble(lang_status_clickData()) %>%
        #     select(!!fields)
      req(input$selected_status_field)
      req(countries_selected()$`Country Code`)
      table <- lang_status_clickData()

      # server = FALSE
      server = TRUE
      datatable(table,
                filter = list(position = 'top', clear = TRUE, plain = TRUE),
                escape = FALSE,
                class = 'display compact',
                # selection = NULL,
                selection = list(mode = "single", target = "cell"),
                extensions = 'Buttons',
                options = list(scrollY = '60vh',
                               scrollcollapse = FALSE,
                               # options = list(scrollY = '60vh',
                               #                scrollcollapse = TRUE,
                               dom = 'Blfrtip',
                               buttons = list('copy', 'print',
                                              list(extend = 'collection',
                                                   buttons = c('csv', 'excel', 'pdf'),
                                                   text = 'Download')
                               ),
                               lengthMenu = list(c(-1, 10, 25, 50), c("All", 10, 25, 50))
                )
      )
    })

    # write user log on stop
    onSessionEnded(function() {
        if(user == "Guest@partnerships.global") {
          # do nothing
        } else {
          user_log_entry <- tibble(
            user_name = user,
            full_name = user_info$full_name,
            start_date_time = start_date_time,
            end_date_time = Sys.time(),
            app = "Mesa")
          sheet_append(
            # ** GP Data > Mesa > data > assets > gp_app_users**
            ss = "1m37eThcH2BbCjRNv1NIQVzJ7e0Fu7KveDhs1NXwDZls",
            data = user_log_entry,
            sheet = "user_log"
          )
          print('Done!')
        }
    })

    # session$onSessionEnded(function() {
    #   print(paste(session$user, Sys.time()))
    # })
}

# Run the application
shinyApp(ui = ui |> add_cookie_handlers(),
         server = server
)


