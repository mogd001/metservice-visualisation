.libPaths(c( "C:/Apps/Hydro/library"))  #.libPaths()
library(glue)
print(glue("lib path: {.libPaths()[1]}"))

#library(remotes)
#Sys.unsetenv("GITHUB_PAT")
#options(download.file.method = "wininet")
#devtools::install_github("mogd001/tdcR")

library(tidyverse)

library(Microsoft365R)
library(blastula)
library(lubridate)

main_start_time <- Sys.time()

print(glue("Beginning MetService rainfall forecast visualisation script {main_start_time}."))

setwd("C:/Apps/Hydro/metservice-visualisation")

source("forecast_visualisation.R")

mfr <- rainfall_summary %>%
  arrange(desc(forecast_rainfall_total)) %>%
  slice(1)

max_forecast_rainfall_site <- mfr$site_name
max_forecast_rainfall <- mfr$forecast_rainfall_total
next_hours <- as.numeric(max(rainfall_pred$datetime) - max(rainfall$datetime), "hours")

src_dir <- "outputs"
dst_dir <- "\\\\tsrvfiles\\HYDROLOGY\\Processing\\MetService_Visualisation"
arch_dir <- "\\\\tsrvfiles\\HYDROLOGY\\Processing\\MetService_Visualisation_Archive"
documentation <- "https://tasmandc.sharepoint.com/:p:/r/sites/TechLibrary/TechLibrary/Instruction%20MetService%20Forecast%20Visualisation.pptx?d=wb1657cd5add84211913c241d30c5088e&csf=1&web=1&e=TFIp38"

# Delete all existing output files
unlink(glue("{dst_dir}/*"), recursive = TRUE)

files <- list.files(src_dir)
file.copy(paste0(src_dir, "/", files), dst_dir, recursive = TRUE)

d <- now()

if (max_forecast_rainfall > 100) {

  # Archive
  arch_dirname <- format(d, "%Y%m%d-%H")
  dir.create(paste0(arch_dir, "/", arch_dirname))

  file.copy(paste0(src_dir, "/", files), paste0(arch_dir, "/", arch_dirname), recursive = TRUE)
}


main_end_time <- Sys.time()
print(glue("Finished script {main_end_time}."))
