library(interp)
library(dplyr)


#' Convert hours since epoch (1970-01-01 0:00:00.0) to NZ datetime.
#'
#' @param t time in hours since epoch t
#' @return datetime in NZDT
convert_epoch_datetime <- function(t) {
  with_tz(ymd_hms("1970-01-01 00:00:00") + hours(t), "NZ") %>% # Convert from UTC to NZDT
    format("dt.%Y%m%d.%H")
}


#' Get the latest model of the day.
#'
#' @param txt_fs model of the day text files
#' @param fp target filepath to MetService Forecasts folder
#' @return model of the day
get_latest_mod <- function(txt_fs, fp) {
  x <- strsplit(txt_fs, split = "_", fixed = TRUE) %>%
    unlist() %>%
    str_subset(".txt") %>% 
    gsub(".{4}$", "", .) %>%
    as.numeric() %>%
    max()

  f <- str_subset(txt_fs, glue("{x}.txt"))

  readLines(file.path(fp, f), warn = FALSE)[1] 
}


#' Get the latest model nc file for a given model and model resolution.
#'
#' @param mdl_files model nc files
#' @param m model
#' @param m_res resolution (4k or 8k)
#' @return filename for most recent model
get_latest_model <- function(mdl_files, m, m_res) {
  
  # Find hourly precip files for the model and resolution
  specific_mdl_files <- str_subset(mdl_files, glue("hourly(.*?)", m_res, "mN-", m, "(.*?).nc"))
  
  # There may be more than one file for different timestamps, return the most recent.
  latest_dt <- strsplit(specific_mdl_files, split = "_", fixed = TRUE) %>%
    unlist() %>%
    str_subset(".nc") %>% 
    gsub(".{3}$", "", .) %>%
    as.numeric() %>%
    max()

  str_subset(specific_mdl_files, glue(m, "(.*?){latest_dt}.nc"))
}


#' Process MetService nc data.
#'
#' @param m model 
#' @param m_res model resolution
process_metservice_nc_data <- function(m, m_res) {
  
  print(glue("{m}-{m_res}"))
  
  model <- get_latest_model(mdl_files, m, m_res)
  
  if (grepl("-SIGMA", model)) {
    model_shortname <- gsub(".{3}$", "", str_extract(str_remove(model, "-SIGMA"), glue(m, "_(.*?).nc")))
  } else {
    model_shortname <- gsub(".{3}$", "", str_extract(model, glue(m, "_(.*?).nc")))
  }
  
  # Get data from nc file
  nc_data <- nc_open(file.path(target_fp, model))
  lon <- ncvar_get(nc_data, "longitude")
  lat <- ncvar_get(nc_data, "latitude")
  time <- ncvar_get(nc_data, "time") %>% 
    convert_epoch_datetime()
  precip <- ncvar_get(nc_data, "precipitation_amount")
  nc_close(nc_data)
  
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
  
  saveRDS(p, file = glue("processed/{m}_{m_res}_point_forecast.rds"))
  saveRDS(s, file = glue("processed/{m}_{m_res}_stack.rds"))
  saveRDS(time, file = glue("processed/{m}_{m_res}_times.rds"))
  
}


#' Get Rainfall
#'
#' @param from start date
#' @param to end date
#' @param site_catchment site to catchment tibble
#' @return rainfall
get_rainfall <- function(from = "", to = "", site_catchment) {
  get_data_collection(
    collection = "AllRainfall", method = "Total",
    from = from, to = to, interval = "1 hour"
  ) %>%
    rename(rainfall_total_mm = value) %>%
    dplyr::select(site, datetime, rainfall_total_mm) %>%
    group_by(site) %>%
    arrange(datetime) %>% 
    mutate(
      datetime = with_tz(datetime, "NZ"),
      rainfall_total_mm = round(rainfall_total_mm, digits = 2)
    ) %>%
    filter(!is.na(rainfall_total_mm)) %>%
    mutate(
      site_name = substring(site, 4)
    ) %>%
    left_join(site_catchment, by = "site")
}


#' Calculate site forecasts
#'
#' @param p tibble of point forecasts
#' @param sites tibble of sites with easting and northing
#' @return site forecasts
calculate_site_forecasts <- function(p, sites) {
  dt_vec <- unique(p$datetime)
  site_forecasts <- tibble()

  for (i in 1:length(unique(p$datetime))) {
    dt <- dt_vec[i]

    p_temp <- p %>% 
      filter(datetime == !!dt) %>%
      group_by(model, datetime, easting, northing) %>% # ensure unique rows (account for day light savings)
      summarise(rainfall_mm = mean(rainfall_mm)) %>% 
      ungroup()

    site_forecast_temp <- interp::interpp(p_temp$easting, p_temp$northing, p_temp$rainfall_mm, sites$easting, sites$northing) %>%
      as_tibble() %>%
      mutate(
        site = sites$site,
        datetime = dt,
      ) %>%
      rename(
        easting = x,
        northing = y,
        rainfall_total_mm = z
      )

    site_forecasts <- bind_rows(site_forecasts, site_forecast_temp)
  }

  site_forecasts
}
