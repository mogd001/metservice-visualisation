library(interp)
library(dplyr)

convert_t_datetime <- function(t) {
  # t : time in hours since epoch t convert to datetime
  with_tz(ymd_hms("1970-01-01 00:00:00") + hours(t), "NZ") %>% # Convert from UTC to NZDT
    format("dt.%Y%m%d.%H")
}


get_most_recent_model_of_the_day <- function(txt_files, fp, latest_month, latest_day) {
  # txt_files : list of text files in directory corresponding to model of the day
  x <- strsplit(txt_files, split = "_", fixed = TRUE) %>%
    unlist() %>%
    str_subset(".txt")
  x <- gsub(".{4}$", "", x) %>%
    as.numeric() %>%
    max()

  # Return filename for most recent model
  f <- str_subset(txt_files, glue("{x}.txt"))

  readLines(file.path(fp, latest_month, latest_day, f), warn = FALSE)[1]
}


get_most_recent_model_of_the_day_target_fp <- function(txt_files, target_fp) {
  # txt_files : list of text files in directory corresponding to model of the day
  x <- strsplit(txt_files, split = "_", fixed = TRUE) %>%
    unlist() %>%
    str_subset(".txt")
  x <- gsub(".{4}$", "", x) %>%
    as.numeric() %>%
    max()
  
  # Return filename for most recent model
  f <- str_subset(txt_files, glue("{x}.txt"))
  
  readLines(file.path(target_fp, f), warn = FALSE)[1]
}


get_most_recent_model <- function(mdl_files, m) {
  # mdl_files : list of model files in directory
  # m : target model
  specific_mdl_files <- str_subset(mdl_files, glue(m, "_(.*?).nc"))
  x <- strsplit(specific_mdl_files, split = "_", fixed = TRUE) %>%
    unlist() %>%
    str_subset(".nc")
  x <- gsub(".{3}$", "", x) %>%
    as.numeric() %>%
    max()

  # Return filename for most recent model
  str_subset(specific_mdl_files, glue(m, "_{x}.nc"))
}


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


calculate_site_forecasts <- function(p, sites) {
  # p : tibble of point forecast estimates
  # sites : tibble of sites with easting and northing
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
