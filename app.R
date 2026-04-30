source("prepare_data.R")

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(lubridate)
library(scoringutils)
library(patchwork)
library(RColorBrewer)
library(jsonlite)

all_forecasts   <- readRDS("all_forecasts.rds")
historical_data <- readRDS("historical_data.rds")
target_data     <- readRDS("target_data.rds")
scored_q        <- readRDS("scored_q.rds")
mwis_ref        <- readRDS("mwis_ref.rds")
forecasts_wide  <- readRDS("forecasts_wide.rds")
locations       <- readRDS("locations.rds")

ref_date <- max(unique(all_forecasts$reference_date))
current_season_year <- if (month(ref_date) >= 8) year(ref_date) else year(ref_date) - 1

all_models    <- unique(all_forecasts$model)
all_horizons  <- sort(unique(scored_q$horizon))
all_ref_dates <- sort(unique(forecasts_wide$reference_date))
all_ref_dates <- all_ref_dates[all_ref_dates >= as.Date("2025-12-01")]

NATIONAL_KEY   <- "__ALL__"
NATIONAL_LABEL <- "All locations"

loc_choices <- c(
  setNames(NATIONAL_KEY, NATIONAL_LABEL),
  setNames(locations$location, locations$location_name)
)

region_choices <- sort(unique(locations$state))

ui <- fluidPage(
  titlePanel("Flu Forecast Explorer"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("tab_choice", "What do you want to look at?",
                  choices = c("Forecast by location",
                              "WIS by location",
                              "WIS over time by model",
                              "WIS by horizon by model",
                              "WIS by horizon")),
      conditionalPanel(
        condition = "input.tab_choice == 'Forecast by location' || input.tab_choice == 'WIS by location'",
        selectInput("selected_loc", "Location", choices = loc_choices)
      ),
      conditionalPanel(
        condition = "input.tab_choice == 'Forecast by location'",
        sliderInput(
          inputId    = "selected_ref_date",
          label      = "Reference Date",
          min        = min(all_ref_dates),
          max        = max(all_ref_dates),
          value      = min(all_ref_dates),
          step       = 7,
          timeFormat = "%Y-%m-%d",
          animate    = animationOptions(interval = 1200, loop = FALSE)
        )
      ),
      conditionalPanel(
        condition = "input.tab_choice == 'WIS over time by model'",
        selectInput("selected_model_ot", "Model", choices = all_models),
        selectInput("selected_model_state_ot", "State", choices = region_choices)
      ),
      conditionalPanel(
        condition = "input.tab_choice == 'WIS by horizon by model'",
        selectInput("selected_model_state_hz", "State", choices = region_choices),
        selectInput("selected_model_loc_hz", "Location", choices = NULL)
      ),
      conditionalPanel(
        condition = "input.tab_choice == 'WIS by horizon'",
        selectInput("selected_horizon", "Horizon (weeks ahead)", choices = all_horizons)
      ),
      hr(),
      p(paste("Data range:", min(all_ref_dates), "to", max(all_ref_dates)), style = "color: gray; font-size: 12px;")
    ),
    mainPanel(
      width = 9,
      plotOutput("main_plot", height = "800px")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$selected_model_state_hz, {
    locs_in_state <- locations |>
      filter(state == input$selected_model_state_hz) |>
      arrange(location_name)
    choices <- setNames(locs_in_state$location, locs_in_state$location_name)
    updateSelectInput(session, "selected_model_loc_hz", choices = choices)
  })
  
  output$main_plot <- renderPlot({
    if (input$tab_choice == "Forecast by location") {
      chosen_date  <- as.Date(input$selected_ref_date)
      nearest_date <- all_ref_dates[which.min(abs(all_ref_dates - chosen_date))]
      plot_forecast(input$selected_loc, nearest_date)
    } else if (input$tab_choice == "WIS by location") {
      plot_wis_location(input$selected_loc)
    } else if (input$tab_choice == "WIS over time by model") {
      req(input$selected_model_ot, input$selected_model_state_ot)
      plot_wis_over_time_model(input$selected_model_ot, input$selected_model_state_ot)
    } else if (input$tab_choice == "WIS by horizon by model") {
      req(input$selected_model_loc_hz)
      plot_wis_horizon_model(input$selected_model_loc_hz)
    } else if (input$tab_choice == "WIS by horizon") {
      plot_wis_horizon(as.integer(input$selected_horizon))
    }
  })
}

plot_forecast <- function(loc, selected_date = ref_date) {
  min_date <- as.Date(paste0(current_season_year, "-08-01"))
  max_date <- max(forecasts_wide$target_end_date)
  
  if (loc == NATIONAL_KEY) {
    target_type <- forecasts_wide |> pull(target) |> unique() |> first()
    
    current <- target_data |>
      filter(target == target_type,
             target_end_date >= min_date, target_end_date <= max_date) |>
      group_by(target_end_date) |>
      summarise(observation = mean(observation, na.rm = TRUE), .groups = "drop")
    
    historical <- historical_data |>
      filter(target == target_type,
             aligned_date >= min_date, aligned_date <= max_date) |>
      group_by(season_label, aligned_date) |>
      summarise(observation = mean(observation, na.rm = TRUE), .groups = "drop")
    
    fc <- forecasts_wide |>
      filter(reference_date == selected_date,
             target_end_date >= min_date, target_end_date <= max_date) |>
      group_by(model, target_end_date) |>
      summarise(across(starts_with("q"), \(x) mean(x, na.rm = TRUE)), .groups = "drop")
    
    loc_label <- NATIONAL_LABEL
  } else {
    if (!exists("target_data") || !exists("historical_data")) {
      return(ggplot() + annotate("text", x=1, y=1, label="Error: Data objects missing from server") + theme_void())
    }
    
    loc_label <- locations |> filter(location == loc) |> pull(location_name)
    if (length(loc_label) == 0) loc_label <- loc
    
    target_type <- forecasts_wide |>
      filter(location == loc) |>
      pull(target) |>
      unique() |>
      first()
    
    current <- target_data |>
      filter(location == loc, target == target_type,
             target_end_date >= min_date, target_end_date <= max_date)
    
    historical <- historical_data |>
      filter(location == loc, target == target_type,
             aligned_date >= min_date, aligned_date <= max_date)
    
    fc <- forecasts_wide |>
      filter(location == loc, reference_date == selected_date,
             target_end_date >= min_date, target_end_date <= max_date)
  }
  
  if (nrow(fc) == 0) {
    last_available <- forecasts_wide |>
      filter(if (loc == NATIONAL_KEY) TRUE else location == loc) |>
      pull(reference_date) |>
      max()
    return(ggplot() +
             annotate("text", x = 1, y = 1,
                      label = paste0("No forecast data for ", loc_label, " on ", selected_date,
                                     "\nLast available: ", last_available),
                      size = 5, hjust = 0.5) +
             theme_void())
  }
  
  n_hist <- n_distinct(historical$season_label)
  models <- sort(unique(fc$model))
  
  current_rep    <- bind_rows(lapply(models, function(m) mutate(current, model = m)))
  historical_rep <- bind_rows(lapply(models, function(m) mutate(historical, model = m)))
  
  ensemble <- fc |>
    group_by(target_end_date) |>
    summarise(ensemble_median = mean(q0.5, na.rm = TRUE), .groups = "drop")
  ensemble_rep <- bind_rows(lapply(models, function(m) mutate(ensemble, model = m)))
  
  p <- ggplot()
  
  if (nrow(historical_rep) > 0) {
    p <- p + geom_line(data = historical_rep,
                       aes(x = aligned_date, y = observation, group = season_label),
                       color = "grey75", linewidth = 0.35, alpha = 0.5)
  }
  
  p <- p +
    geom_line(data = current_rep, aes(x = target_end_date, y = observation), color = "black", linewidth = 0.6) +
    geom_point(data = current_rep, aes(x = target_end_date, y = observation), color = "black", size = 0.9)
  
  if (all(c("q0.025", "q0.975") %in% names(fc)))
    p <- p + geom_ribbon(data = fc, aes(x = target_end_date, ymin = q0.025, ymax = q0.975, fill = model), alpha = 0.2)
  
  if (all(c("q0.25", "q0.75") %in% names(fc)))
    p <- p + geom_ribbon(data = fc, aes(x = target_end_date, ymin = q0.25, ymax = q0.75, fill = model), alpha = 0.3)
  
  if ("q0.5" %in% names(fc))
    p <- p +
    geom_line(data = fc, aes(x = target_end_date, y = q0.5, color = model), linewidth = 0.8) +
    geom_point(data = fc, aes(x = target_end_date, y = q0.5, color = model), size = 1.8)
  
  y_label <- if (loc == NATIONAL_KEY) "ED visits (%) — national mean" else "ED visits (%)"
  cap_obs  <- if (loc == NATIONAL_KEY) "Black: current season mean, Grey: %d historical seasons mean, Dashed: ensemble median" else "Black: current season, Grey: %d historical seasons, Dashed: ensemble median"
  
  p <- p +
    geom_line(data = ensemble_rep, aes(x = target_end_date, y = ensemble_median),
              color = "black", linewidth = 0.7, linetype = "dashed") +
    geom_vline(xintercept = selected_date, linetype = "dotted", color = "gray50", alpha = 0.5) +
    scale_color_brewer(palette = "Set1", name = "Model") +
    scale_fill_brewer(palette = "Set1", name = "Model") +
    facet_wrap(~model, ncol = 2) +
    labs(title = loc_label,
         subtitle = paste0("Reference date: ", selected_date),
         x = "Date", y = y_label,
         caption = sprintf(cap_obs, n_hist)) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 13),
          strip.text = element_text(face = "bold", size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 7)) +
    scale_x_date(date_labels = "%b %d", date_breaks = "2 weeks")
  
  return(p)
}

plot_wis_location <- function(loc) {
  if (loc == NATIONAL_KEY) {
    loc_scored        <- scored_q
    plot_title_suffix <- NATIONAL_LABEL
  } else {
    loc_scored        <- scored_q |> filter(location == loc)
    plot_title_suffix <- loc
  }
  
  if (nrow(loc_scored) == 0) return(NULL)
  
  n_models_local <- length(unique(loc_scored$model_id))
  
  wis_time <- loc_scored |>
    group_by(model_id, reference_date) |>
    summarise(mean_wis = mean(wis, na.rm = TRUE), .groups = "drop") |>
    left_join(mwis_ref, by = "model_id")
  
  pA <- ggplot(wis_time, aes(x = reference_date, y = mean_wis, color = model_id)) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1.8) +
    geom_hline(aes(yintercept = MWIS), linetype = "dashed", color = "black", linewidth = 0.5, alpha = 0.7) +
    facet_wrap(~model_id, ncol = 3) +
    scale_color_manual(values = colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(n_models_local), guide = "none") +
    scale_x_date(date_labels = "%b %d", date_breaks = "3 weeks") +
    labs(title = paste("WIS over time -", plot_title_suffix),
         subtitle = "dashed line = overall MWIS across all locations for each model",
         x = "Reference date", y = "Mean WIS") +
    theme_minimal(base_size = 10) +
    theme(strip.text = element_text(face = "bold", size = 8),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
          plot.title = element_text(face = "bold"))
  
  mwis_loc <- loc_scored |>
    group_by(model_id) |>
    summarise(MWIS_local = mean(wis, na.rm = TRUE), .groups = "drop")
  
  loc_scored_joined <- left_join(loc_scored, mwis_loc, by = "model_id")
  x_max <- quantile(loc_scored$wis, 0.99, na.rm = TRUE)
  
  pB <- ggplot(loc_scored_joined, aes(x = wis, fill = model_id)) +
    geom_density(alpha = 0.4, color = NA) +
    geom_vline(aes(xintercept = MWIS_local), linetype = "solid", color = "black", linewidth = 0.6) +
    geom_vline(data = left_join(data.frame(model_id = unique(loc_scored$model_id)), mwis_ref, by = "model_id"),
               aes(xintercept = MWIS), linetype = "dashed", color = "black", linewidth = 0.5, alpha = 0.6) +
    facet_wrap(~model_id, ncol = 3, scales = "free_y") +
    scale_fill_manual(values = colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(n_models_local), guide = "none") +
    coord_cartesian(xlim = c(0, x_max)) +
    labs(title = paste("WIS distribution -", plot_title_suffix),
         subtitle = "solid = local/national MWIS, dashed = overall MWIS",
         x = "WIS (lower is better)", y = "Density") +
    theme_minimal(base_size = 10) +
    theme(strip.text = element_text(face = "bold", size = 8),
          plot.title = element_text(face = "bold"))
  
  return(pA / pB)
}

plot_wis_over_time_model <- function(model, selected_state) {
  locs_in_state      <- locations |> filter(state == selected_state) |> pull(location)
  loc_names_in_state <- locations |> filter(state == selected_state) |> select(location, location_name)
  
  model_scored <- scored_q |> filter(model_id == model, location %in% locs_in_state)
  if (nrow(model_scored) == 0) return(NULL)
  
  model_scored <- model_scored |> left_join(loc_names_in_state, by = "location")
  
  wis_time <- model_scored |>
    group_by(location_name, reference_date) |>
    summarise(mean_wis = mean(wis, na.rm = TRUE), .groups = "drop")
  
  n_locs <- length(locs_in_state)
  
  ggplot(wis_time, aes(x = reference_date, y = mean_wis, color = location_name)) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1.8) +
    facet_wrap(~location_name, ncol = 3) +
    scale_color_manual(values = colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(n_locs), guide = "none") +
    scale_x_date(date_labels = "%b %d", date_breaks = "3 weeks") +
    labs(title = paste("WIS over time -", model),
         subtitle = paste("state:", selected_state),
         x = "Reference date", y = "Mean WIS") +
    theme_minimal(base_size = 10) +
    theme(strip.text = element_text(face = "bold", size = 8),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
          plot.title = element_text(face = "bold"))
}

plot_wis_horizon_model <- function(loc) {
  loc_name <- locations |> filter(location == loc) |> pull(location_name)
  if (length(loc_name) == 0) loc_name <- loc
  
  loc_scored <- scored_q |> filter(location == loc)
  if (nrow(loc_scored) == 0) return(NULL)
  
  n_models_local <- length(unique(loc_scored$model_id))
  
  wis_by_horizon <- loc_scored |>
    group_by(model_id, horizon) |>
    summarise(mean_wis = mean(wis, na.rm = TRUE), .groups = "drop")
  
  ggplot(wis_by_horizon, aes(x = factor(horizon), y = mean_wis, fill = model_id)) +
    geom_col(position = "dodge", width = 0.7) +
    geom_text(aes(label = sprintf("%.2f", mean_wis)), position = position_dodge(width = 0.7),
              vjust = -0.4, size = 2.5) +
    scale_fill_manual(values = colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(n_models_local), name = "Model") +
    labs(title = paste("Mean WIS by horizon -", loc_name),
         subtitle = "all models",
         x = "Horizon (weeks ahead)", y = "Mean WIS") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"),
          legend.position = "right")
}

plot_wis_horizon <- function(horizon_val) {
  h_scored <- scored_q |> filter(horizon == horizon_val)
  if (nrow(h_scored) == 0) return(NULL)
  
  n_models_local <- length(unique(h_scored$model_id))
  
  wis_model_date <- h_scored |>
    group_by(model_id, reference_date) |>
    summarise(mean_wis = mean(wis, na.rm = TRUE), .groups = "drop")
  
  ggplot(wis_model_date, aes(x = reference_date, y = mean_wis, color = model_id)) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1.8) +
    facet_wrap(~model_id, ncol = 3) +
    scale_color_manual(values = colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(n_models_local), guide = "none") +
    scale_x_date(date_labels = "%b %d", date_breaks = "3 weeks") +
    labs(title = sprintf("Mean WIS over time at horizon %d weeks ahead", horizon_val),
         subtitle = "averaged across all locations",
         x = "Reference date", y = "Mean WIS") +
    theme_minimal(base_size = 10) +
    theme(strip.text = element_text(face = "bold", size = 8),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
          plot.title = element_text(face = "bold"))
}

shinyApp(ui = ui, server = server)
