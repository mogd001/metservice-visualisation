library(glue)
library(Microsoft365R)
library(blastula)
library(lubridate)

main_start_time <- Sys.time()

print(glue("Beginning MetService rainfall forecast visualisation script {main_start_time}."))

setwd("C:/Users/matto/OneDrive - Tasman District Council/Desktop/Working/R/metservice-visualisation")

source("forecast_visualisation.R")

mfr <- rainfall_summary %>%
  arrange(desc(forecast_rainfall_total)) %>%
  slice(1)

max_forecast_rainfall_site <- mfr$site_name
max_forecast_rainfall <- mfr$forecast_rainfall_total
next_hours <- as.numeric(max(rainfall_pred$datetime) - max(rainfall$datetime), "hours")

src_dir <- "outputs"
dst_dir <- "M:/Processing/MetService_Visualisation"
arch_dir <- "M:/Processing/MetService_Visualisation_Archive"
documentation <- "https://tasmandc.sharepoint.com/:p:/r/sites/TechLibrary/TechLibrary/Instruction%20MetService%20Forecast%20Visualisation.pptx?d=wb1657cd5add84211913c241d30c5088e&csf=1&web=1&e=TFIp38"

# Delete all existing output files
unlink(glue("{dst_dir}/*"), recursive = TRUE)

files <- list.files(src_dir)
file.copy(paste0(src_dir, "/", files), dst_dir, recursive = TRUE)

# Email team upon completion
# my_outlook <- get_personal_outlook() # if business outlook is not working

d <- now()

my_outlook <- get_business_outlook()

em_body <- glue("## MetService forecast visualisation updated

Script completed {d}. Maximum forecast rainfall is at {max_forecast_rainfall_site} with {max_forecast_rainfall} mm forecast over the next {next_hours} hours.

Results available: [{dst_dir}]({dst_dir}).

Explanation of outputs: [documentation]({documentation}).

Next 24 hours:
![Rainfall forecast next 24 hours](temp/rainfall_next24hrs.jpeg)

")

email <- compose_email(
  body = md(em_body),
  footer = md("TDC Evironmental Data")
)

if (max_forecast_rainfall > 100) {
  em <- my_outlook$create_email(email, subject = "MetService Visualisation", to = c("matt.ogden@tasman.govt.nz", "hydroteam@tasman.govt.nz"))

  # Archive
  arch_dirname <- format(d, "%Y%m%d-%H")
  dir.create(paste0(arch_dir, "/", arch_dirname))

  file.copy(paste0(src_dir, "/", files), paste0(arch_dir, "/", arch_dirname), recursive = TRUE)
} else {
  em <- my_outlook$create_email(email, subject = "MetService Visualisation", to = c("matt.ogden@tasman.govt.nz"))
}

em$send()

# Upload to sharepoint
site <- get_sharepoint_site(site_name = "Environmental Monitoring")
site$get_drive("Reports and Analyses")$upload_file(glue("outputs/{files[1]}"), "R Outputs/model_forecasts_comparison.mp4")
site$get_drive("Reports and Analyses")$upload_file(glue("outputs/{files[2]}"), "R Outputs/model_forecasts_comparison_accumulation.jpeg")
site$get_drive("Reports and Analyses")$upload_file(glue("outputs/{files[3]}"), "R Outputs/summary.html")

# Delete existing image folders
tryCatch(
  expr = {
    site$get_drive("Reports and Analyses")$delete_item("R Outputs/lib/image-Observed Rainfall-0.0.1")
  },
  error = function(e) {
    print("No image-Observed Rainfall folder to delete.")
  }
)
tryCatch(
  expr = {
    site$get_drive("Reports and Analyses")$delete_item("R Outputs/lib/image-Forecast Rainfall-0.0.1")
  },
  error = function(e) {
    print("No image-Forecast Rainfall folder to delete.")
  }
)
tryCatch(
  expr = {
    site$get_drive("Reports and Analyses")$delete_item("R Outputs/site_plots")
  },
  error = function(e) {
    print("No site_plots folder to delete.")
  }
)

upload_image_sharepoint <- function(x, target_folder) {
  site$get_drive("Reports and Analyses")$upload_file(glue("outputs/{target_folder}/{x}"), glue("R Outputs/{target_folder}/{x}"))
}

observed_files <- list.files("outputs/lib/image-Observed Rainfall-0.0.1")
map(observed_files, upload_image_sharepoint, target_folder = "lib/image-Observed Rainfall-0.0.1")

forecast_files <- list.files("outputs/lib/image-Forecast Rainfall-0.0.1")
map(forecast_files, upload_image_sharepoint, target_folder = "lib/image-Forecast Rainfall-0.0.1")

site_plot_files <- list.files("outputs/site_plots")
map(site_plot_files, upload_image_sharepoint, target_folder = "site_plots/")

main_end_time <- Sys.time()

print(glue("Finished script {main_end_time}."))
