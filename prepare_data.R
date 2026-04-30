library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(scoringutils)
library(httr)
library(jsonlite)

base_raw <- "https://raw.githubusercontent.com/gongelaine/flu-metrocast/main"
base_api <- "https://api.github.com/repos/gongelaine/flu-metrocast"

locations  <- read_csv(paste0(base_raw, "/auxiliary-data/locations.csv"), show_col_types = FALSE)
saveRDS(locations, "data/locations.rds")

all_target <- read_csv(paste0(base_raw, "/target-data/latest-data.csv"), show_col_types = FALSE) |>
  mutate(target_end_date = as.Date(target_end_date))

ref_date            <- max(all_target$target_end_date)
current_season_year <- if (month(ref_date) >= 8) year(ref_date) else year(ref_date) - 1
season_start        <- as.Date(paste0(current_season_year, "-08-01"))

target_data <- all_target |>
  filter(target_end_date >= season_start)
saveRDS(target_data, "data/target_data.rds")

historical_data <- all_target |>
  filter(target_end_date < season_start) |>
  mutate(
    season_yr    = if_else(month(target_end_date) >= 8, year(target_end_date), year(target_end_date) - 1),
    season_label = paste0(season_yr, "-", season_yr + 1),
    aligned_date = as.Date(paste0(current_season_year, "-08-01")) +
      (target_end_date - as.Date(paste0(season_yr, "-08-01")))
  )
saveRDS(historical_data, "data/historical_data.rds")

get_github_csv_urls <- function(folder) {
  url      <- paste0(base_api, "/contents/", folder)
  response <- GET(url)
  items    <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
  
  model_dirs <- items$name[items$type == "dir"]
  
  bind_rows(lapply(model_dirs, function(model_name) {
    sub_url      <- paste0(base_api, "/contents/", folder, "/", model_name)
    sub_response <- GET(sub_url)
    sub_items    <- fromJSON(content(sub_response, as = "text", encoding = "UTF-8"))
    
    csv_files <- sub_items[sub_items$type == "file" & grepl("\\.csv$", sub_items$name), ]
    if (nrow(csv_files) == 0) return(NULL)
    
    bind_rows(lapply(csv_files$download_url, function(file_url) {
      read_csv(file_url, show_col_types = FALSE) |>
        mutate(model = model_name)
    }))
  }))
}

message("Fetching model output from GitHub...")
all_forecasts <- get_github_csv_urls("model-output") |>
  mutate(
    reference_date  = as.Date(reference_date),
    target_end_date = as.Date(target_end_date),
    output_type_id  = as.numeric(output_type_id)
  ) |>
  filter(output_type == "quantile")
saveRDS(all_forecasts, "data/all_forecasts.rds")

forecasts_wide <- all_forecasts |>
  mutate(q_col = paste0("q", output_type_id)) |>
  select(model, reference_date, target, horizon, target_end_date, location, q_col, value) |>
  pivot_wider(names_from = q_col, values_from = value)
saveRDS(forecasts_wide, "data/forecasts_wide.rds")

forecast_scores <- all_forecasts |>
  left_join(
    target_data |> select(target_end_date, location, target, observation),
    by = c("target_end_date", "location", "target")
  ) |>
  filter(!is.na(observation)) |>
  as_forecast_quantile(
    forecast_unit  = c("model", "reference_date", "target", "horizon", "target_end_date", "location"),
    predicted      = "value",
    observed       = "observation",
    quantile_level = "output_type_id"
  )

scored_q <- score(forecast_scores) |>
  select(model, reference_date, location, target, horizon, target_end_date, wis) |>
  rename(model_id = model)
saveRDS(scored_q, "data/scored_q.rds")

mwis_ref <- scored_q |>
  group_by(model_id) |>
  summarise(MWIS = mean(wis, na.rm = TRUE), .groups = "drop")
saveRDS(mwis_ref, "data/mwis_ref.rds")

message("Done — all .rds files updated at ", Sys.time())



