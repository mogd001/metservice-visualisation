library(lubridate)
library(ncdf4)
library(raster)
library(rgdal)
library(ggplot2)
library(ggmap)
library(tidyverse)
library(sf)
library(glue)
library(tmap)

source("functions.R")

unlink("processed-nz/*")
unlink("outputs-nz/*")
#K:/ND Files/Environmental Management/Environmental Monitoring/MetSerivce Forecasts

nz_coastline <- st_read("data/lds-nz-coastlines-and-islands-polygons-topo-150k-GPKG/nz-coastlines-and-islands-polygons-topo-150k.gpkg", layer = "nz_coastlines_and_islands_polygons_topo_150k") %>% 
  select(geom)

nz <- st_read("data/statsnzregional-council-2023-generalised-GPKG/regional-council-2023-generalised.gpkg", layer = "regional_council_2023_generalised") %>% 
  filter(REGC2023_V1_00_NAME != "Area Outside Region") %>% 
  transmute(region = REGC2023_V1_00_NAME_ASCII, geom) %>% 
  st_intersection(st_as_sf(nz_coastline)) %>% 
  mutate(area = as.numeric(st_area(.))) %>% 
  filter(area > 10000000) %>% 
  select(region, geom) %>% 
  group_by(region) %>% 
  summarize(geometry = st_union(geom))

nz_buff <- nz %>%
  st_union() %>%
  st_buffer(100000) %>%
  st_transform(4326)

#nz_buff %>% write_sf("data/nz.gpkg", layer = "temp")
target_fp <- file.path("data/metservice_forecasts_local")

msf_files <- list.files(target_fp)
txt_files <- str_subset(msf_files, ".txt$")
mdl_files <- str_subset(msf_files, ".nc$")

# Read model of the day
if (length(txt_files) > 0) {
  model_of_the_day <- get_most_recent_model_of_the_day_target_fp(txt_files, target_fp)
} else {
  model_of_the_day <- "ECMWF" # default to ECMWF if no model of the day is declared yet
}
saveRDS(model_of_the_day, file = glue("processed-nz/model_of_the_day.rds"))

models <- c("ECMWF", "NCEP", "UKMO")

for (m in models) {
  model <- get_most_recent_model(mdl_files, m)
  model_shortname <- gsub(".{3}$", "", str_extract(model, glue(m, "_(.*?).nc")))

  # Get relevant data from nc file
  nc_data <- nc_open(file.path(target_fp, model))

  lon <- ncvar_get(nc_data, "longitude")
  lat <- ncvar_get(nc_data, "latitude")
  time <- ncvar_get(nc_data, "time") #  hours since 1970-01-01 00:00:00.0 UTC
  precip <- ncvar_get(nc_data, "precipitation_amount")

  nc_close(nc_data)

  # Adjust time formatting
  time <- convert_t_datetime(time)

  # Create empty tibble for saving point forecast
  p <- tibble()

  # Create raster stack
  s <- stack()
  for (i in 2:length(time)) { # TODO need to significantly improve performance

    print(i)
    t <- time[i]
    rainfall_mm <- precip[, , i] - precip[, , i - 1] # rainfall estimated to accumulate in that hour

    pts <- tibble(lon = c(lon), lat = c(lat), rainfall_mm = c(rainfall_mm)) %>%
      st_as_sf(coords = c("lon", "lat"))
    st_crs(pts) <- 4326

    # Filter points buffered to nelson/tasman region +100 km buffer
    pts <- st_filter(pts, nz_buff) %>%
      mutate(
        lon = st_coordinates(.)[, "X"],
        lat = st_coordinates(.)[, "Y"]
      )

    pts_nztm <- pts %>%
      st_transform(2193) %>%
      mutate(
        easting = st_coordinates(.)[, "X"],
        northing = st_coordinates(.)[, "Y"],
        model = m,
        datetime = force_tz(ymd_h(t), "NZ")
      ) %>%
      dplyr::select(model, datetime, easting, northing, rainfall_mm) %>%
      st_set_geometry(NULL)

    p <- bind_rows(p, pts_nztm)

    # Rasterise and fill any gaps
    r <- raster(crs = crs(pts), vals = 0, resolution = c(0.1, 0.1), ext = extent(c(-180, 180, -90, 90))) %>%
      rasterize(pts, .) %>%
      subset("rainfall_mm") %>%
      focal(w = matrix(1, nrow = 3, ncol = 3), fun = mean, NAonly = TRUE, na.rm = TRUE)

    names(r) <- t
    s <- stack(s, r)
  }

  # writeRaster(s, file=glue("processed/{m}_stack.tif"), overwrite = TRUE)
  saveRDS(p, file = glue("processed-nz/{m}_point_forecast.rds"))
  saveRDS(s, file = glue("processed-nz/{m}_stack.rds"))
  saveRDS(time, file = glue("processed-nz/{m}_times.rds"))
}
rm(p, s, time)

# Visualise results
model_of_the_day <- readRDS(glue("processed-nz/model_of_the_day.rds"))
s_ecmwf <- readRDS(glue("processed-nz/ECMWF_stack.rds"))
s_ncep <- readRDS(glue("processed-nz/NCEP_stack.rds"))
s_ukmo <- readRDS(glue("processed-nz/UKMO_stack.rds"))

e <- extent(st_transform(nz, crs(s_ecmwf)))
e_buffered <- extent(
  e[1] - 0.05 * diff(e[1:2]), e[2] + 0.05 * diff(e[1:2]),
  e[3] - 0.05 * diff(e[3:4]), e[4] + 0.05 * diff(e[3:4])
)

max_value <- plyr::round_any(max(maxValue(s_ecmwf), maxValue(s_ncep), maxValue(s_ukmo)), 10, f = ceiling)
breaks <- append(c(0, 1, 2, 5), seq(10, max_value, 5)) # 5 mm increments
max_n_layers <- max(nlayers(s_ecmwf), nlayers(s_ncep), nlayers(s_ukmo))

# forecast_start  <- c(names(s_ecmwf[[1]]), names(s_ecmwf[[1]]), names(s_ukmo[[1]])) %>% 
#   ymd_h(tz = "NZ") %>% 
#   max()

model_start <- ymd_h(names(s_ecmwf)[[1]], tz = "NZ") - hours(1)
forecast_start <- ymd_hms("20230213 000000" , tz = "NZ")

# Create a list of tmap objects
tmaps <- list()
mnames <- ifelse(models == model_of_the_day, paste0(models, "-MOD"), models) # update model name for graph labeling purposes

j <- 0
for (i in 1:max_n_layers) {
  r1 <- crop(s_ecmwf[[i]], e_buffered)
  dt <- names(r1)
  dt_label <- format(ymd_h(dt, tz = "NZ"), "%A %d %B %Y %I%p")
  
  if (ymd_h(dt, tz = "NZ") < forecast_start) next # only produce animation for future
  if (!(dt %in% names(s_ncep) & dt %in% names(s_ukmo))) {
   print(glue("skiping {i}"))
    next # only consider timesteps in which all three models are available
  }
  j <- j + 1
  
  r2 <- crop(s_ncep[[dt]], e_buffered)
  r3 <- crop(s_ukmo[[dt]], e_buffered)
  
  if (j == 1) {
    print(glue("start of forecast: {ymd_h(dt, tz = 'NZ')}"))
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
    tm_raster(breaks = breaks, palette = colorRampPalette(c("grey","orange", "magenta"))(length(breaks)), title = glue("Rainfall (mm) {dt_label}")) +
    tm_facets(nrow = 1) +
    tm_shape(nz) + tm_borders() +
    tm_layout(
      title = glue("MetService Forecast\nModel Run {format(model_start, dt_frmt)}"),
      legend.outside = TRUE
    )
}

rasters_list <- list(r_ecmwf, r_ncep, r_ukmo)
names(rasters_list) <- mnames
rasters_stack <- stack(rasters_list)

now_to_end_forecast <- difftime(ymd_h(dt, tz = "NZ"), ymd_h(format(forecast_start, "%Y-%m-%d %H"), tz = "NZ"), units = "hours")
r_end <- stack(rasters_list)

rm(s_ecmwf, s_ncep, s_ukmo)

tmap_animation(tmaps, width = 4000, height = 1400, fps = 2.0, outer.margins = 0, filename = glue("outputs-nz/{format(forecast_start, '%Y%m%d-%H')}_model_forecasts_comparison.gif"), dpi = 300)

# Create graph comparing forecasts for different times into the future
create_forecast_plot <- function(rs, t_label) {
  tm_shape(rs) +
    tm_raster(breaks = breaks, palette = colorRampPalette(c("grey","orange", "magenta"))(length(breaks)), title = glue("Rainfall (mm) {t_label}"), legend.hist = TRUE) +
    tm_facets(nrow = 1) +
    tm_shape(nz) + tm_borders() +
    tm_layout(
      title = glue("MetService Forecast Accumulated Rainfall\nModel Run {format(model_start, dt_frmt)}"),
      legend.outside = TRUE
    ) 
}


breaks <- seq(0, 500, 100) # 10 mm increments after initial bands #c(0, 1, 2, 5),

dt_frmt <- "%Y-%m-%d %I %p"
fs_frmt <- format(forecast_start, dt_frmt)
p_3hrs <- create_forecast_plot(r_3hrs, glue("3 hrs [{fs_frmt} - {format(forecast_start + hours(3), dt_frmt)}]"))
p_6hrs <- create_forecast_plot(r_6hrs, glue("6 hrs [{fs_frmt} - {format(forecast_start + hours(6), dt_frmt)}]"))
p_12hrs <- create_forecast_plot(r_12hrs, glue("12 hrs [{fs_frmt} - {format(forecast_start + hours(12), dt_frmt)}]"))
p_24hrs <- create_forecast_plot(r_24hrs, glue("24 hrs [{fs_frmt} - {format(forecast_start + hours(24), dt_frmt)}]"))
p_36hrs <- create_forecast_plot(r_24hrs, glue("36 hrs [{fs_frmt} - {format(forecast_start + hours(36), dt_frmt)}]"))
p_48hrs <- create_forecast_plot(r_48hrs, glue("48 hrs [{fs_frmt} - {format(forecast_start + hours(48), dt_frmt)}]"))
p_end <- create_forecast_plot(r_end, glue("{as.character(now_to_end_forecast)} hrs [{fs_frmt} - {format(forecast_start + now_to_end_forecast, dt_frmt)}]"))

tmap_save(p_24hrs, glue("outputs-nz/p_24.jpeg"), dpi = 600, height = 4, width = 12)
tmap_save(p_36hrs, glue("outputs-nz/p_36.jpeg"), dpi = 600, height = 4, width = 12)

tmap_arrange(p_3hrs, p_6hrs, p_12hrs, p_24hrs, p_48hrs, p_end, nrow = 6) %>%
  tmap_save(glue("outputs-nz/{format(forecast_start, '%Y%m%d-%H')}_model_forecasts_comparison_accumulation.jpeg"), dpi = 600, width = 10000)

# Distribution of rainfall by region

r_36hrs_nztm <- projectRaster(r_36hrs, crs = 2193)

df <- tibble()
ms <- sapply(mnames, gsub, pattern = '-', replacement =".") 

for (model in ms) {
  raster_values <- raster::extract(r_36hrs_nztm[[model]], nz) # extract values by region
  for (i in 1:length(raster_values)) {
    temp_df <- tibble(rainfall_mm = unlist(raster_values[i]),   region = nz[i,]$region, model = model)
    df <- bind_rows(df, temp_df)
  }
}

df <- df %>% 
  group_by(region) %>% 
  mutate(median_by_region = median(rainfall_mm)) %>% 
  ungroup()

plot_by_region <- ggplot(df, aes(x = reorder(region, -median_by_region), y = rainfall_mm)) +
  geom_boxplot() +
  theme_bw() + 
  labs(x= "Region", y = "Rainfall (mm) - 24 hr total", title = glue("Forecast rainfall forecast summary for: {forecast_start} to {forecast_start + hours(36)}")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_wrap(~model, nrow = 3)

ggsave("outputs-nz/plot_by_region.png", plot = plot_by_region, dpi = 600, width = 12, height = 8)
