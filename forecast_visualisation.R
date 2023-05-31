library(ggh4x)

library(tdcR)
library(tidyverse)
library(lubridate)
library(glue)
library(sf)
library(tmap)

library(scales)
library(raster)
library(animation)
library(gganimate)
library(crosstalk)
library(DT)
library(htmltools)
library(leaflet)
library(leafpop)

library(config)

source("netcdf.R")

# Clear outputs folder
unlink("outputs/*", recursive = TRUE)
unlink("outputs_rainfall/*", recursive = TRUE)

# Clear memory
rm(list = ls())
gc()

source("functions.R")

config <- config::get()

dt_frmt <- "%a %d %B %Y %I%p"

# Topo template
t_prefix <- "http://tiles-a.data-cdn.linz.govt.nz/services;key="
key <- config$linzkey
tMid <- "/tiles/v4/layer="
tSuffix <- "/EPSG:3857/{z}/{x}/{y}.png"
topo_50_template <- paste0(t_prefix, key, tMid, 50767, tSuffix)

# Catchments
catchments <- st_read("data/context.gpkg", layer = "catchments") %>%
  mutate(catchment = factor(catchment,
    ordered = TRUE,
    levels = c("Aorere", "Takaka", "Riwaka", "Motueka", "Marahau", "Moutere", "Waimea", "Nelson", "Buller")
  ))

# Rivers
rivers <- st_read("data/context.gpkg", layer = "rivers") %>%
  st_transform(crs = 4326)

# Sites
sites <- get_sites(collection = "AllRainfall", synonyms = TRUE) %>%
  mutate(
    longitude_ = longitude,
    latitude_ = latitude
  ) %>%
  st_as_sf(coords = c("longitude_", "latitude_"), crs = 4326) %>%
  st_transform(crs = 2193) %>%
  mutate(
    easting = st_coordinates(.)[, "X"],
    northing = st_coordinates(.)[, "Y"]
  ) %>%
  st_join(catchments, join = st_intersects) %>%
  replace_na(list(catchment = "Motueka")) %>%
  mutate(
    site_name = second_synonym
  )

start_date <- as.Date(now(), tz = "NZ") - days(3)
site_catchment <- dplyr::select(tibble(sites), c(site, catchment))
rainfall <- get_rainfall(format(start_date, "%Y%m%d"), "Now", site_catchment) # get recent rainfall data

rainfall_sites <- unique(rainfall$site)
sites <- sites %>% filter(site %in% rainfall_sites)

models <- c("ECMWF", "NCEP", "UKMO")
model_of_the_day <- readRDS(glue("processed/model_of_the_day.rds"))

rainfall_pred <- tibble()

for (model in models) {
  p <- readRDS(glue("processed/{model}_point_forecast.rds"))
  rainfall_pred_temp <- calculate_site_forecasts(p, sites) %>%
    mutate(model = model)
  rainfall_pred <- bind_rows(rainfall_pred, rainfall_pred_temp)
}

s_ecmwf <- readRDS(glue("processed/ECMWF_stack.rds"))
s_ncep <- readRDS(glue("processed/NCEP_stack.rds"))
s_ukmo <- readRDS(glue("processed/UKMO_stack.rds"))

catchments_wgs <- st_transform(catchments, crs(s_ecmwf))
sites_wgs <- st_transform(sites, crs(s_ecmwf))

e <- extent(catchments_wgs)
e_buffered <- extent(
  e[1] - 0.05 * diff(e[1:2]), e[2] + 0.05 * diff(e[1:2]),
  e[3] - 0.05 * diff(e[3:4]), e[4] + 0.05 * diff(e[3:4])
)

max_value <- plyr::round_any(max(maxValue(s_ecmwf), maxValue(s_ncep), maxValue(s_ukmo)), 10, f = ceiling)
breaks <- append(c(0, 1, 2, 5), seq(10, max_value, 5)) # 5 mm increments
max_n_layers <- max(nlayers(s_ecmwf), nlayers(s_ncep), nlayers(s_ukmo))

# Create a list of tmap objects
tmaps <- list()
mnames <- ifelse(models == model_of_the_day, paste0(models, "-MOD"), models) # update model name for graph labeling purposes

forecast_start <- now()

j <- 0
for (i in 1:max_n_layers) {
  r1 <- crop(s_ecmwf[[i]], e_buffered)
  dt <- names(r1)
  dt_label <- format(ymd_h(dt, tz = "NZ"), dt_frmt)

  if (is.na(dt_label)) next # account for day light savings
  if (ymd_h(dt, tz = "NZ") < forecast_start) next # only produce animation for future

  j <- j + 1

  if (!(dt %in% names(s_ncep) & dt %in% names(s_ukmo))) break # only consider timesteps in which all three models are available

  r2 <- crop(s_ncep[[dt]], e_buffered)
  r3 <- crop(s_ukmo[[dt]], e_buffered)

  if (j == 1) {
    r_ecmwf <- r1
    r_ncep <- r2
    r_ukmo <- r3
  } else {
    r_ecmwf <- r_ecmwf + r1
    r_ncep <- r_ncep + r2
    r_ukmo <- r_ukmo + r3

    if (format(ymd_h(dt, tz = "NZ"), "%Y-%m-%d %H") == format(forecast_start + hours(3), "%Y-%m-%d %H")) {
      rasters_list <- list(r_ecmwf, r_ncep, r_ukmo)
      names(rasters_list) <- mnames
      rasters_stack <- stack(rasters_list)

      r_3hrs <- stack(rasters_list)
    }
    if (format(ymd_h(dt, tz = "NZ"), "%Y-%m-%d %H") == format(forecast_start + hours(6), "%Y-%m-%d %H")) {
      rasters_list <- list(r_ecmwf, r_ncep, r_ukmo)
      names(rasters_list) <- mnames
      rasters_stack <- stack(rasters_list)

      r_6hrs <- stack(rasters_list)
    }
    if (format(ymd_h(dt, tz = "NZ"), "%Y-%m-%d %H") == format(forecast_start + hours(12), "%Y-%m-%d %H")) {
      rasters_list <- list(r_ecmwf, r_ncep, r_ukmo)
      names(rasters_list) <- mnames
      rasters_stack <- stack(rasters_list)

      r_12hrs <- stack(rasters_list)
    }
    if (format(ymd_h(dt, tz = "NZ"), "%Y-%m-%d %H") == format(forecast_start + hours(24), "%Y-%m-%d %H")) {
      rasters_list <- list(r_ecmwf, r_ncep, r_ukmo)
      names(rasters_list) <- mnames
      rasters_stack <- stack(rasters_list)

      r_24hrs <- stack(rasters_list)
    }
    if (format(ymd_h(dt, tz = "NZ"), "%Y-%m-%d %H") == format(forecast_start + hours(36), "%Y-%m-%d %H")) {
      rasters_list <- list(r_ecmwf, r_ncep, r_ukmo)
      names(rasters_list) <- mnames
      rasters_stack <- stack(rasters_list)

      r_36hrs <- stack(rasters_list)
    }
    if (format(ymd_h(dt, tz = "NZ"), "%Y-%m-%d %H") == format(forecast_start + hours(48), "%Y-%m-%d %H")) {
      rasters_list <- list(r_ecmwf, r_ncep, r_ukmo)
      names(rasters_list) <- mnames
      rasters_stack <- stack(rasters_list)

      r_48hrs <- stack(rasters_list)
    }
  }

  rasters_list <- list(r1, r2, r3)
  names(rasters_list) <- mnames
  rasters_stack <- stack(rasters_list)

  tmaps[[j]] <- tm_shape(rasters_stack) +
    tm_raster(breaks = breaks, palette = colorRampPalette(c("grey", "orange", "magenta"))(length(breaks)), title = glue("Rainfall (mm) {dt_label}")) +
    tm_facets(nrow = 1) +
    tm_shape(catchments_wgs) + tm_borders() +
    tm_shape(sites) +
    tm_dots(size = 0.02, label = "name", col = "white") +
    tm_text("site_name", just = "top", size = 0.2, col = "white") +
    tm_layout(
      title = glue("MetService Forecast"),
      legend.outside = TRUE
    )
}

# Rainfall until end of forecast
rasters_list <- list(r_ecmwf, r_ncep, r_ukmo)
names(rasters_list) <- mnames
rasters_stack <- stack(rasters_list)

now_to_end_forecast <- difftime(ymd_h(dt, tz = "NZ"), ymd_h(format(forecast_start, "%Y-%m-%d %H"), tz = "NZ"), units = "hours")

r_end <- stack(rasters_list)

rm(s_ecmwf, s_ncep, s_ukmo)

# Create the animation
tryCatch(
  {
    tmap_animation(tmaps, width = 4000, height = 1400, fps = 4.0, outer.margins = 0, filename = glue("outputs/{format(forecast_start, '%Y%m%d-%H')}_Animation.mp4"), dpi = 300)
    tmap_animation(tmaps, width = 4000, height = 1400, fps = 2.0, outer.margins = 0, filename = glue("outputs/{format(forecast_start, '%Y%m%d-%H')}_Animation.gif"), dpi = 300)
  },
  error = function(e) {
    print(paste("FFMPEG error:", e$message))
  }
)
  
# Create graph comparing forecasts for different times into the future
create_forecast_plot <- function(rs, t_label, breaks) {
  tm_shape(rs) +
    tm_raster(breaks = breaks, palette = colorRampPalette(c("grey", "orange", "magenta"))(length(breaks)), title = glue("Rainfall (mm) {t_label}")) +
    tm_facets(nrow = 1) +
    tm_shape(catchments_wgs) + tm_borders() +
    tm_shape(sites) +
    tm_dots(size = 0.01, label = "name", col = "white") +
    tm_text("site_name", just = "top", size = 0.1, col = "white") +
    tm_layout(
      title = glue("MetService Forecast Accumulated Rainfall"),
      legend.outside = TRUE
    )
}

# CUMULATIVE COMPARISON
max_value <- plyr::round_any(max(maxValue(r_end), maxValue(r_end), maxValue(r_end)), 10, f = ceiling)
breaks <- append(c(0, 1, 2, 5), seq(10, max_value, 20)) # 10 mm increments after initial bands

fs_frmt <- format(forecast_start, dt_frmt)
p_3hrs <- create_forecast_plot(r_3hrs, glue("3 hrs [{fs_frmt} - {format(forecast_start + hours(3), dt_frmt)}]"), breaks)
p_6hrs <- create_forecast_plot(r_6hrs, glue("6 hrs [{fs_frmt} - {format(forecast_start + hours(6), dt_frmt)}]"), breaks)
p_12hrs <- create_forecast_plot(r_12hrs, glue("12 hrs [{fs_frmt} - {format(forecast_start + hours(12), dt_frmt)}]"), breaks)
p_24hrs <- create_forecast_plot(r_24hrs, glue("24 hrs [{fs_frmt} - {format(forecast_start + hours(24), dt_frmt)}]"), breaks)
p_36hrs <- create_forecast_plot(r_36hrs, glue("24 hrs [{fs_frmt} - {format(forecast_start + hours(24), dt_frmt)}]"), breaks)
p_48hrs <- create_forecast_plot(r_48hrs, glue("48 hrs [{fs_frmt} - {format(forecast_start + hours(48), dt_frmt)}]"), breaks)
p_end <- create_forecast_plot(r_end, glue("{as.character(now_to_end_forecast)} hrs [{fs_frmt} - {format(forecast_start + now_to_end_forecast, dt_frmt)}]"), breaks)

tmap_save(p_24hrs, glue("temp/rainfall_next24hrs.jpeg"), dpi = 90, height = 4, width = 12)

tmap_arrange(p_3hrs, p_6hrs, p_12hrs, p_24hrs, p_48hrs, p_end, nrow = 6) %>%
  tmap_save(glue("outputs/{format(forecast_start, '%Y%m%d-%H')}_Cumulative.jpeg"), dpi = 600, width = 10000)

# 12 HOURS
r_12to24 <- r_24hrs - r_12hrs
r_24to36 <- r_36hrs - r_24hrs
r_36to48 <- r_48hrs - r_36hrs

max_value <- plyr::round_any(max(maxValue(r_12hrs), maxValue(r_12to24), maxValue(r_24to36), maxValue(r_36to48)), 10, f = ceiling)
breaks <- append(c(0, 1, 2, 5), seq(10, max_value, 20)) # 10 mm increments after initial bands

p_0to12hrs <- create_forecast_plot(r_12hrs, glue("[{fs_frmt} - {format(forecast_start + hours(12), dt_frmt)}]"), breaks)
p_12to24hrs <- create_forecast_plot(r_12to24, glue("[{format(forecast_start + hours(12), dt_frmt)} - {format(forecast_start + hours(24), dt_frmt)}]"), breaks)
p_24to36hrs <- create_forecast_plot(r_24to36, glue("[{format(forecast_start + hours(24), dt_frmt)} - {format(forecast_start + hours(36), dt_frmt)}]"), breaks)
p_36to48hrs <- create_forecast_plot(r_36to48, glue("[{format(forecast_start + hours(36), dt_frmt)} - {format(forecast_start + hours(48), dt_frmt)}]"), breaks)

tmap_arrange(p_0to12hrs, p_12to24hrs, p_24to36hrs, p_36to48hrs, nrow = 4) %>%
  tmap_save(glue("outputs/{format(forecast_start, '%Y%m%d-%H')}_12hour.jpeg"), dpi = 600, width = 10000)

# Clean up
rm(r_12to24, r_24to36, r_36to48)
rm(p_3hrs, p_6hrs, p_12hrs, p_24hrs, p_36hrs, p_48hrs, p_end, p_0to12hrs, p_12to24hrs, p_24to36hrs, p_36to48hrs)

generate_plot <- function(r_site, rainfall_summary) {
  print(r_site)
  r_actual <- rainfall %>%
    filter(site == !!r_site)

  forecast_start <- max(r_actual$datetime)
  observed_rainfall_total <- sum(r_actual$rainfall_total_mm) %>% round(1)
  observed_rainfall_peak <- max(r_actual$rainfall_total_mm) %>% round(1)

  r_pred_ecmwf <- rainfall_pred %>%
    filter(site == !!r_site & model == "ECMWF")
  r_pred_ncep <- rainfall_pred %>%
    filter(site == !!r_site & model == "NCEP")
  r_pred_ukmo <- rainfall_pred %>%
    filter(site == !!r_site & model == "UKMO")

  model_start <- tribble(
    ~model, ~model_start,
    "ECMWF", min(r_pred_ncep$datetime) - hours(1),
    "NCEP", min(r_pred_ecmwf$datetime) - hours(1),
    "UKMO", min(r_pred_ukmo$datetime) - hours(1)
  )
  mod_start_plot <- filter(model_start, model == !!model_of_the_day)$model_start

  # Rainfall forecast between model start and forecast_start - TODO functionalise
  rainfall_total_to_model_run <- r_actual %>%
    filter(datetime < mod_start_plot) %>%
    pull(rainfall_total_mm) %>%
    sum()

  r_pred_past_ecmwf <- r_pred_ecmwf %>%
    filter(datetime > mod_start_plot & datetime <= forecast_start) %>%
    mutate(
      rainfall_total_mm_hrly = rainfall_total_mm,
      rainfall_total_mm = if_else(row_number() == 1, rainfall_total_mm + rainfall_total_to_model_run, rainfall_total_mm)
    )
  r_pred_past_ncep <- r_pred_ncep %>%
    filter(datetime > mod_start_plot & datetime <= forecast_start) %>%
    mutate(
      rainfall_total_mm_hrly = rainfall_total_mm,
      rainfall_total_mm = if_else(row_number() == 1, rainfall_total_mm + rainfall_total_to_model_run, rainfall_total_mm)
    )
  r_pred_past_ukmo <- r_pred_ukmo %>%
    filter(datetime > mod_start_plot & datetime <= forecast_start) %>%
    mutate(
      rainfall_total_mm_hrly = rainfall_total_mm,
      rainfall_total_mm = if_else(row_number() == 1, rainfall_total_mm + rainfall_total_to_model_run, rainfall_total_mm)
    )

  r_pred_past <- bind_rows(r_pred_past_ecmwf, r_pred_past_ncep, r_pred_past_ukmo) %>%
    group_by(model) %>%
    mutate(
      rainfall_total_mm = cumsum((rainfall_total_mm)) / 10
    ) %>%
    ungroup() %>%
    mutate(
      model_raw = model,
      model = if_else(
        model == model_of_the_day, paste0(model, "*"), model
      )
    )

  # Rainfall forecast for the three models from now - TODO functionalise
  r_pred_future_ecmwf <- r_pred_ecmwf %>%
    filter(datetime > forecast_start) %>%
    mutate(
      rainfall_total_mm_hrly = rainfall_total_mm,
      rainfall_total_mm = if_else(row_number() == 1, rainfall_total_mm + observed_rainfall_total, rainfall_total_mm)
    )
  r_pred_future_ncep <- r_pred_ncep %>%
    filter(datetime > forecast_start) %>%
    mutate(
      rainfall_total_mm_hrly = rainfall_total_mm,
      rainfall_total_mm = if_else(row_number() == 1, rainfall_total_mm + observed_rainfall_total, rainfall_total_mm)
    )
  r_pred_future_ukmo <- r_pred_ukmo %>%
    filter(datetime > forecast_start) %>%
    mutate(
      rainfall_total_mm_hrly = rainfall_total_mm,
      rainfall_total_mm = if_else(row_number() == 1, rainfall_total_mm + observed_rainfall_total, rainfall_total_mm)
    )

  r_pred_future <- bind_rows(r_pred_future_ecmwf, r_pred_future_ncep, r_pred_future_ukmo) %>%
    group_by(model) %>%
    mutate(
      rainfall_total_mm = cumsum((rainfall_total_mm)) / 10
    ) %>%
    ungroup() %>%
    mutate(
      model_raw = model,
      model = if_else(
        model == model_of_the_day, paste0(model, "*"), model
      )
    )

  r_pred_future_summary <- r_pred_future %>%
    group_by(model_raw) %>%
    summarise(
      model = unique(model),
      rainfall_total_mm = max(rainfall_total_mm) * 10,
      rainfall_peak_mm = max(rainfall_total_mm_hrly)
    ) %>%
    mutate(rainfall_total_mm = round(rainfall_total_mm - observed_rainfall_total, 1)) %>%
    left_join(model_start, by = c("model_raw" = "model"))

  forecast_rainfall_total <- filter(r_pred_future_summary, model_raw == !!model_of_the_day)$rainfall_total_mm
  forecast_rainfall_peak  <- filter(r_pred_future_summary, model_raw == !!model_of_the_day)$rainfall_peak_mm %>% round(1)
  
  # observed and forecast rainfall (24 hours)
  observed_rt_24hrs <- r_actual %>%
    mutate(datetime = datetime) %>%
    filter(datetime > forecast_start - hours(24)) %>%
    pull(rainfall_total_mm) %>%
    sum() %>%
    round(1)

  forecast_rt_24hrs <- r_pred_future %>%
    mutate(datetime = datetime) %>%
    filter(datetime <= forecast_start + hours(24) & model_raw == !!model_of_the_day) %>%
    pull(rainfall_total_mm_hrly) %>%
    sum() %>%
    round(1)

  # add to summary tibble
  summary <- tribble(
    ~site, ~observed_rainfall_total, ~forecast_rainfall_total, ~observed_rt_24hrs, ~forecast_rt_24hrs, ~observed_rainfall_peak, ~forecast_rainfall_peak,
    r_site, observed_rainfall_total, forecast_rainfall_total,  observed_rt_24hrs, forecast_rt_24hrs, observed_rainfall_peak, forecast_rainfall_peak
  )

  max_hrly_ylimit <- ceiling(round(2 + max(r_actual$rainfall_total_mm, r_pred_future_summary$rainfall_total_mm / 10), 2))
  y_limit <- ceiling(max(max_hrly_ylimit * 10, sum(r_actual$rainfall_total_mm) + max(r_pred_future_summary$rainfall_total_mm))/10) + 2
  
  caption_txt <- paste(r_pred_future_summary$model, r_pred_future_summary$rainfall_total_mm, "mm", paste0("[Computed ", r_pred_future_summary$model_start, "]")) %>%
    knitr::combine_words()

  model_colours <- c("#ff7f00", "#984ea3", "#4daf4a") # standard colours for each model c("ECMWF", "NCEP", "UKMO")
  model_colours[grep(model_of_the_day, models)] <- "red"

  breaks <- seq(floor_date(min(r_actual$datetime), "day"), max(r_pred_future$datetime), by = "6 hours")
  date_labels <- c(sapply(breaks, function(x) {
    if (hour(x) == 0) {
      format(x, "%a %d %B-%H") 
    } else {
      format(x, "%H")
    }
  }))

  plot_site_summary <- ggplot() +
    geom_col(r_actual, mapping = aes(x = datetime - minutes(30), y = rainfall_total_mm), color = "blue", fill = "blue", alpha = 0.4) +
    geom_col(filter(r_pred_future, model_raw == !!model_of_the_day), mapping = aes(x = datetime - minutes(30), y = rainfall_total_mm_hrly), color = "red", fill = "red", alpha = 0.4) +
    geom_vline(xintercept = forecast_start) +
    geom_vline(xintercept = mod_start_plot, colour = "red", linetype = "dotted") +
    geom_line(r_actual, mapping = aes(x = datetime, y = cumsum((rainfall_total_mm)) / 10), size = 0.8, color = "magenta", linetype = "twodash", inherit.aes = FALSE) +
    geom_line(r_pred_past, mapping = aes(x = datetime, y = rainfall_total_mm, color = model), size = 0.8, alpha = 0.4, linetype = "twodash", inherit.aes = FALSE) +
    geom_line(r_pred_future, mapping = aes(x = datetime, y = rainfall_total_mm, color = model), size = 0.8, linetype = "twodash", inherit.aes = FALSE) +
    scale_x_datetime(breaks = breaks, date_labels = date_labels, minor_breaks = date_breaks("1 hour"), guide = "axis_minor") +
    scale_y_continuous(
      name = "Hourly Rainfall (mm) [bars]",
      limits = c(0, y_limit),
      sec.axis = sec_axis(~ . * 10, name = "Cumulative Rainfall (mm) [lines]"),
      expand = c(0, 0),
      breaks = seq(0, y_limit, 5)
    ) +
    scale_color_manual(values = model_colours) +
    geom_label(aes(x = median(r_actual$datetime), y = y_limit * 0.95, label = glue("Observed {observed_rainfall_total} mm")), color = "black", fill = "white", size = 5) +
    geom_label(aes(x = median(r_pred_future_ecmwf$datetime), y = y_limit * 0.95, label = glue("Forecast {forecast_rainfall_total} mm")), color = "black", fill = "white", size = 5) +
    geom_label(aes(x = mod_start_plot, y = y_limit * 0.85, label = glue("Model (MOD) Run\n{mod_start_plot}")), color = "Red", fill = "white", size = 3) +
    geom_label(aes(x = forecast_start, y = y_limit * 0.75, label = glue("Now\n{forecast_start}")), color = "Black", fill = "white", size = 3) +
    theme_bw() +
    labs(
      x = "Date-Hour (NZDT)", color = "Model", title = glue("{r_site} Rainfall"),
      caption = glue("{caption_txt} additional rainfall is forecast. \n* denotes model of the day. ECMWF - European Centre for Medium-range Weather Forecasts, NCEP - National Centers for Environmental Prediction, UKMO - UK Met Office.")
    ) +
    theme(
      axis.ticks.y.left = element_line(colour = "blue"),
      axis.title.y.left = element_text(colour = "blue"),
      axis.line.y.left = element_line(color = "blue"),
      axis.text.y.left = element_text(color = "blue"),
      axis.ticks.y.right = element_line(colour = "magenta"),
      axis.title.y.right = element_text(colour = "magenta", angle = 90),
      axis.line.y.right = element_line(color = "magenta"),
      axis.text.y.right = element_text(color = "magenta"),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      plot.caption = element_text(size = 6),
      panel.grid.minor = element_line(color = "#FAFAFA"),
      ggh4x.axis.ticks.length.minor = rel(0.5)
    )

  ggsave(glue("outputs/site_plots/{format(forecast_start, '%Y%m%d-%H')}_{r_site}_Rainfall_Forecast.png"), plot = plot_site_summary, dpi = 300, height = 8, width = 12)
  
  o <- r_actual %>% 
    dplyr::select(datetime, rainfall_total_mm, site) %>% 
    mutate(
      model = "observed",
      type = "observed"
    )
  
  f <- r_pred_future %>% dplyr::select(datetime, rainfall_total_mm = rainfall_total_mm_hrly, site, model) %>% 
      mutate(type = "forecast")
  
  r_data <- bind_rows(o, f)
  
  return(list(plot_site_summary, summary, r_data))
}

dir.create("outputs/site_plots")
p_all <- lapply(rainfall_sites, generate_plot, rainfall_summary)

sites_join <- sites %>%
  dplyr::select(site, site_name, catchment, latitude, longitude) %>%
  st_drop_geometry()

plots <- lapply(p_all, "[[", 1)
rainfall_summary <- lapply(p_all, "[[", 2) %>%
  bind_rows()
rainfall_data_export <- lapply(p_all, "[[", 3) %>%
  bind_rows()

rescale_vec <- function(x, max_val, new_min, new_max) {
  (x / max_val) * (new_max - new_min) + new_min
}

max_rainfall <- max(c(rainfall_summary$observed_rainfall_total, rainfall_summary$forecast_rainfall_total), na.rm = TRUE)
max_radius <- if (max_rainfall >= 200) {
  max_radius <- 50
} else if (max_rainfall >= 160) {
  max_radius <- 40
} else if (max_rainfall >= 120) {
  max_radius <- 30
} else if (max_rainfall >= 80) {
  max_radius <- 20
} else if (max_rainfall >= 40) {
  max_radius <- 10
} else {
  max_radius <- 5
}

max_val <- max(rainfall_summary$observed_rainfall_total, rainfall_summary$forecast_rainfall_total)

rainfall_summary <- rainfall_summary %>%
  left_join(sites_join, by = "site") %>%
  dplyr::select(site_name, catchment, observed_rainfall_total, observed_rt_24hrs, forecast_rt_24hrs, forecast_rainfall_total, latitude, longitude, observed_rainfall_peak, forecast_rainfall_peak) %>%
  mutate(radius_or = rescale_vec(observed_rainfall_total, max_val, 2, max_radius)) %>%
  relocate(radius_or, .after = longitude) %>%
  mutate(radius_fr = rescale_vec(forecast_rainfall_total, max_val, 2, max_radius)) %>%
  relocate(radius_fr, .after = radius_or)

rf_ct <- SharedData$new(rainfall_summary, key = ~site_name)

# Table
table_link <- "file:///M:/Datafiles/Data%20Tables/RainHour.html"
# PowerBi report: https://app.powerbi.com/view?r=eyJrIjoiNzM0YTY2YmQtMGQ4ZS00NTY5LWE4ODgtMzJmMzcwMzA1NzJlIiwidCI6IjFhNGMwYzk4LThkMmMtNDc4ZC1iM2QwLThiYjhmZmNkNDU1NCJ9

table <- rf_ct %>%
  datatable(
    caption = htmltools::tags$caption(
      style = "caption-side: top; text-align: left; color:black;  font-size:100% ;",
      htmltools::withTags(
        div(HTML(glue('<a href={table_link} target="_blank">Rainfall Past 24 Hours</a> at {max(rainfall$datetime)}
                      <br>Observed: {format(min(rainfall$datetime), "%A %d %B %Y %I %p")} to {format(max(rainfall$datetime), "%A %d %B %Y %I %p")} [{as.numeric(max(rainfall$datetime) - min(rainfall$datetime), "hours")} hours]
                      <br>Forecast: {format(max(rainfall$datetime), "%A %d %B %Y %I %p")} to {format(max(rainfall_pred$datetime), "%A %d %B %Y %I %p")} [{as.numeric(max(rainfall_pred$datetime) - max(rainfall$datetime), "hours")} hours]')))
      )
    ),
    rownames = FALSE,
    colnames = c("Site", "Catchment", "Observed RF", "Observed RF Past 24 Hours", "Forecast RF Next 24 Hours", "Forecast RF", "Lat", "Long", "R Observed", "R Forecast"),
    # filter = list(position = "bottom"),
    options = list(
      order = list(list(2, "desc")),
      searching = FALSE,
      pageLength = 15, lengthChange = FALSE, scrollX = TRUE, autoWidth = FALSE,
      columnDefs = list(
        list(targets = c(6, 7, 8, 9, 10, 11), visible = FALSE)
      )
    )
  )

catchment_centroids <- catchments_wgs %>% st_centroid()

pal1 <- colorNumeric(colorRampPalette(c("grey", "orange", "magenta"))(length(breaks)), values(r_end), na.color = "transparent")

initial_lng <- st_coordinates(catchment_centroids$geom)[, 1] %>% mean()
initial_lat <- st_coordinates(catchment_centroids$geom)[, 2] %>% mean() - 0.1597735
initial_zoom <- 9

# Map
map <- leaflet(height = 900) %>%
  addTiles(urlTemplate = topo_50_template, group = "NZ Topo50") %>%
  addTiles(group = "OSM") %>%
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  # add catchments
  addPolygons(
    group = "Catchments",
    data = catchments_wgs,
    fillColor = "blue",
    weight = 2,
    opacity = 1,
    color = "blue",
    fillOpacity = 0.1,
    labelOptions = labelOptions(noHide = FALSE, direction = "auto")
  ) %>%
  # add catchments
  addLabelOnlyMarkers(
    group = "Catchments",
    data = catchment_centroids,
    label = ~ as.character(catchment),
    labelOptions = labelOptions(
      noHide = TRUE, direction = "top", textOnly = TRUE,
      style = list(
        "color" = "blue",
        "font-family" = "serif",
        "font-style" = "bold",
        # "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
        "font-size" = "15px"
        # "border-color" = "rgba(0,0,0,0.5)"
      )
    )
  ) %>%
  # add rivers
  addPolylines(
    group = "Rivers",
    data = rivers,
    weight = 1
  ) %>%
  # add observed rainfall
  addCircleMarkers(
    group = "Observed Rainfall",
    data = rf_ct,
    radius = ~radius_or,
    color = "blue",
    stroke = TRUE,
    fillOpacity = 0.6,
    label = ~ paste(as.character(site_name), "<br>Observed", observed_rainfall_total, " mm <br>Peak", observed_rainfall_peak,  "mm/hr") %>% lapply(htmltools::HTML),
    labelOptions = labelOptions(noHide = FALSE, direction = "auto", style = list(
      "color" = "black",
      "font-family" = "serif",
      "font-size" = "15px"
    ))
  ) %>%
  # add forecast rainfall
  addCircleMarkers(
    group = "Forecast Rainfall",
    data = rainfall_summary,
    radius = ~radius_fr,
    color = "red",
    stroke = TRUE,
    fillOpacity = 0.6,
    label = ~ paste(as.character(site_name), "<br>Forecast", forecast_rainfall_total, " mm <br> Peak", forecast_rainfall_peak,  "mm/hr") %>% lapply(htmltools::HTML),
    labelOptions = labelOptions(noHide = FALSE, direction = "auto", style = list(
      "color" = "black",
      "font-family" = "serif",
      "font-size" = "15px"
    ))
  ) %>%
  addPopupGraphs(plots, group = "Observed Rainfall", width = 600, height = 500, dpi = 300) %>%
  addPopupGraphs(plots, group = "Forecast Rainfall", width = 600, height = 500, dpi = 300) %>%
  addRasterImage(r_end[[paste0(model_of_the_day, ".MOD")]], group = "Forecast Rainfall Raster", colors = pal1, opacity = 0.8) %>%
  addLegend(
    pal = pal1, values = values(r_end), position = "bottomright", group = "Forecast Rainfall Raster",
    title = "Forecast Rainfall (MOD)"
  ) %>%
  # Layers control
  addLayersControl(
    baseGroups = c("Toner Lite", "NZ Topo50", "OSM", "Toner"),
    overlayGroups = c("Catchments", "Rivers", "Observed Rainfall", "Forecast Rainfall", "Forecast Rainfall Raster"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  hideGroup(c("Forecast Rainfall", "Forecast Rainfall Raster")) %>%
  setView(lng = initial_lng, lat = initial_lat, zoom = initial_zoom)

p <- crosstalk::bscols(
  widths = c(6, 6),
  div(table, style = css(width = "100%", height = "100%")),
  div(map, style = css(width = "100%", height = "100%"))
)

htmltools::save_html(p, file = glue("outputs/{format(forecast_start, '%Y%m%d-%H')}_Overview.html"))

# Export rainfall timeseries 
rainfall_data_export %>% 
  mutate(datetime = format(datetime, "%d/%m/%Y %H:%M:%S")) %>% 
  write_csv(glue("outputs/{format(forecast_start, '%Y%m%d-%H')}_combined_rainfall_data.csv"))
  