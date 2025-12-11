# UKR DASHBOARD ####
# SET UP (Updated, faster) ####

## LIBRARIES ####
shhh <- suppressPackageStartupMessages
options(dplyr.summarise.inform = FALSE)

shhh(library(shiny))
shhh(library(tidyverse))
shhh(library(shinyjs))
shhh(library(bslib))
shhh(library(bsicons))
shhh(library(waiter))
shhh(library(data.table))
shhh(library(gridlayout))
shhh(library(shinyWidgets))
shhh(library(plotly))
shhh(library(shinydisconnect))
shhh(library(htmltools))
shhh(library(sf))
shhh(library(duckdb))
shhh(library(leaflet))
shhh(library(DT))
shhh(library(writexl))
shhh(library(shinyscreenshot))
shhh(library(googledrive))
shhh(library(jsonlite))
shhh(library(httr))
shhh(library(dotenv))
rm(shhh)

## TEXT DATA ####
about <- readr::read_csv(file.path("data/text", "about.csv"))
popup <- readr::read_csv(file.path("data/text", "popup_start.csv"))

## AUTH (unchanged) ####
if (grepl('Users/EdithD/Documents/git/ukraine-firearms-dashboard', getwd())) {
  dotenv::load_dot_env(file = '.env')
}
GOOGLE_TOKEN <- Sys.getenv('GOOGLE_TOKEN')
if (nzchar(GOOGLE_TOKEN)) {
  googledrive::drive_auth(path = GOOGLE_TOKEN, scopes = 'drive')
}
dir.create('googledrive-temp', showWarnings = FALSE)
suppressWarnings({
  id_users_file <- tryCatch(
    googledrive::drive_get("users.csv")$id,
    error = function(e) NA
  )
})

## DB CONNECTION ####
dir.create("data/database/tmp", recursive = TRUE, showWarnings = FALSE)
con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = "data/database/ukr_firearms_dashboard.duckdb"
)

# Small runtime boosts
DBI::dbExecute(con, "PRAGMA threads=4")
DBI::dbExecute(
  con,
  sprintf(
    "PRAGMA temp_directory='%s'",
    normalizePath("data/database/tmp", winslash = "/")
  )
)

## BASE TABLES (lazy) ####
firearm_table <- dplyr::tbl(con, "ukr_socialMedia") |>
  dplyr::filter(
    post_source == 'main cases' &
      !is.na(post_date) &
      !is.na(post_item_eng) &
      !is.na(post_oblast_eng)
  )
firearm_summary_table <- dplyr::tbl(con, "ukr_socialMedia_summary") |>
  filter(!grepl('None', post_item_eng))

## MATERIALIZED VIEWS (push splitting/aggregation into DuckDB) ####
DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE VIEW v_base AS
  SELECT
    post_id,
    CAST(post_date AS DATE) AS post_date,
    DATE_TRUNC('month', CAST(post_date AS DATE))::DATE AS post_date_month,
    TRIM(i) AS post_item,
    TRIM(o) AS post_oblast,
    post_link,
    post_author_eng,
    post_title_eng,
    post_content_eng
  FROM ukr_socialMedia,
       UNNEST(string_split(post_item_eng,   '; ')) AS t1(i),
       UNNEST(string_split(post_oblast_eng, '; ')) AS t2(o)
  WHERE post_source = 'main cases'
    AND post_date IS NOT NULL
    AND post_item_eng IS NOT NULL
    AND post_oblast_eng IS NOT NULL
    AND post_item_eng != 'None.'
"
)

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE VIEW v_items_month AS
  SELECT post_date_month, post_item, COUNT(*) AS post_mention
  FROM v_base
  GROUP BY 1,2
"
)

DBI::dbExecute(
  con,
  "
  CREATE OR REPLACE VIEW v_oblast_item AS
  SELECT post_oblast, post_item, COUNT(*) AS post_mention
  FROM v_base
  GROUP BY 1,2
"
)

## COLORS & CHOICES (cache once) ####
# distinct ENG/UKR items (collect once)
item_cols <- firearm_summary_table |>
  dplyr::select(dplyr::any_of(c("post_item_eng", "post_item_ukr"))) |>
  dplyr::distinct() |>
  dplyr::collect()

# a fixed palette recycled if needed
base_palette <- c(
  "#CF2734",
  "#A3082A",
  "#174574",
  "#19798B",
  "#EFB050",
  "#EC8744",
  "#E75546",
  "#F3D567",
  "#3F1E55",
  "#00A676",
  "#9C6ADE",
  "#3C99DC",
  "#F86624",
  "#6A994E"
)

# build a named color vector across both languages
all_items <- sort(unique(unlist(item_cols, use.names = FALSE)))

palette_color <- setNames(
  rep(base_palette, length.out = length(all_items)),
  all_items
)
palette_factor <- leaflet::colorFactor(
  palette = unname(palette_color),
  levels = names(palette_color)
)

# cached choices for filters
choices_eng_item <- firearm_summary_table %>%
  dplyr::distinct(post_item_eng) %>%
  dplyr::collect() %>%
  dplyr::pull() %>%
  sort()
choices_eng_oblast <- firearm_summary_table %>%
  dplyr::distinct(post_oblast_eng) %>%
  dplyr::collect() %>%
  dplyr::pull() %>%
  sort()
choices_ukr_item <- tryCatch(
  firearm_summary_table %>%
    dplyr::distinct(post_item_ukr) %>%
    dplyr::collect() %>%
    dplyr::pull() %>%
    sort(),
  error = function(e) choices_eng_item
)
choices_ukr_oblast <- tryCatch(
  firearm_summary_table %>%
    dplyr::distinct(post_oblast_ukr) %>%
    dplyr::collect() %>%
    dplyr::pull() %>%
    sort(),
  error = function(e) choices_eng_oblast
)

# date bounds (once)
date_min <- firearm_table %>%
  dplyr::distinct(post_date_month) %>%
  dplyr::collect() %>%
  dplyr::pull() %>%
  min()
date_max <- firearm_table %>%
  dplyr::distinct(post_date_month) %>%
  dplyr::collect() %>%
  dplyr::pull() %>%
  max()

## STATIC BUNDLE (if app.R expects this) ####
cache_static_data <- function(firearm_table, firearm_summary_table) {
  list(
    choices = list(
      eng = list(items = choices_eng_item, oblasts = choices_eng_oblast),
      ukr = list(items = choices_ukr_item, oblasts = choices_ukr_oblast)
    ),
    palette_color = palette_color,
    palette_factor = palette_factor,
    date_min = date_min,
    date_max = date_max
  )
}
