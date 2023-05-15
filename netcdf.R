library(lubridate)
library(ncdf4)
library(raster)
library(rgdal)
library(ggplot2)
library(ggmap)
library(tidyverse)
library(sf)
library(glue)

source("functions.R")

# https://rpubs.com/boyerag/297592

nelsontasman_buff <- st_read("data/context.gpkg", layer = "nelsontasman") %>%
  st_union() %>%
  st_buffer(100000) %>%
  st_transform(4326)

nelsontasman_buff %>% write_sf("data/nelsontasman.gpkg", layer = "temp")

# MetService_Forecasts from ftp
fp <- "K:/ND Files/Environmental Management/Environmental Monitoring/MetSerivce Forecasts"

# Find latest model, firstly identify directory containing most recent files
msf_months <- list.dirs(fp, full.names = FALSE)[-1]

latest_month <- sub("[/].*", "", msf_months) %>%
  unique() %>%
  as.numeric() %>%
  max()

msf_days <- list.dirs(file.path(fp, latest_month), full.names = FALSE)[-1]

latest_day <- msf_days %>%
  unique() %>%
  as.numeric() %>%
  max()

latest_day <- sprintf("%02d", latest_day)

rm(msf_months, msf_days)
target_fp <- file.path(fp, latest_month, latest_day)

msf_files <- list.files(target_fp)

txt_files <- str_subset(msf_files, ".txt$")
mdl_files <- str_subset(msf_files, ".nc$")

# Read model of the day
if (length(txt_files) > 0) {
  model_of_the_day <- get_most_recent_model_of_the_day(txt_files, fp, latest_month, latest_day)
} else {
  model_of_the_day <- "ECMWF" # default to ECMWF if no model of the day is declared yet
}
saveRDS(model_of_the_day, file = glue("processed/model_of_the_day.rds"))

models <- c("ECMWF", "NCEP", "UKMO")

start_time <- Sys.time()

for (m in models) {
  print(m)
  
  model <- get_most_recent_model(mdl_files, m)
  model_shortname <- gsub(".{3}$", "", str_extract(model, glue(m, "_(.*?).nc")))

  # Get relevant data from nc file
  nc_data <- nc_open(file.path(fp, latest_month, latest_day, model))

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
    pts <- st_filter(pts, nelsontasman_buff) %>%
      mutate(
        lon = st_coordinates(.)[, "X"],
        lat = st_coordinates(.)[, "Y"],
        rainfall_mm = replace(rainfall_mm, rainfall_mm > 1e4, NA) # handle anomalies in the rainfall values
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
  saveRDS(p, file = glue("processed/{m}_point_forecast.rds"))
  saveRDS(s, file = glue("processed/{m}_stack.rds"))
  saveRDS(time, file = glue("processed/{m}_times.rds"))
}

end_time <- Sys.time()
end_time - start_time # for evaluation purposes
