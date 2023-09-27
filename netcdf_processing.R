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

# MetService_Forecasts downloaded from ftp
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

# latest_day <- sprintf("%02d", latest_day)
latest_day <- formatC(latest_day, width = 2, flag = "0")

rm(msf_months, msf_days)
target_fp <- file.path(fp, latest_month, latest_day)

msf_files <- list.files(target_fp)

txt_files <- str_subset(msf_files, ".txt$")
mdl_files <- str_subset(msf_files, ".nc$")

# Read model of the day
if (length(txt_files) > 0) {
  mod <- get_latest_mod(txt_files, target_fp)
} else {
  mod <- "ECMWF" # default to ECMWF if no model of the day is declared yet
}
saveRDS(mod, file = glue("processed/mod.rds"))

models <- c("ECMWF", "NCEP", "UKMO")
m_ress <- c("4k", "8k")

combinations <- expand.grid(models, m_ress) # Combinations of model and model resolution

# Process the metservice files
apply(combinations, 1, function(row) { 
  process_metservice_nc_data(row[1], row[2])
})