library(tidyverse)
library(lubridate)
library(glue)
dt_frmt <- "%a %d %B %Y %I %p"

# Back analysis of model of the day used by MetService
fp <- "K:/ND Files/Environmental Management/Environmental Monitoring/MetSerivce Forecasts"

text_files <- list.files(fp, recursive = TRUE, pattern = "\\.txt$")

t_file <- text_files[1]

read_mod_text_file <- function(f) {
  m <- readLines(file.path(fp,f))[1]
  dt <- strsplit(f, split = "_", fixed = TRUE) %>%
    unlist() %>%
    str_subset(".txt") %>% 
    stringr::str_remove(".txt") %>% 
    ymd_hm(tz  = "NZ")
  
  tibble(model = m, datetime_start = dt)
}

res <- lapply(text_files, read_mod_text_file) %>% 
  bind_rows()

res2 <- res %>%
  mutate(datetime_start = round(datetime_start, units = "hours"))

start_time <- min(res2$datetime_start)
end_time <- max(res2$datetime_start)
hour_seq <- seq.POSIXt(start_time, end_time, by = "hour")

complete_df <- tibble(datetime_start = hour_seq)

# Left join the original dataframe with the complete sequence
result <- complete_df %>%
  left_join(res2, by = "datetime_start") %>% 
  fill(model, .direction = "down") %>% 
  rename(datetime = datetime_start)

View(result)

# plot results
result_summary <- result %>%
  group_by(model) %>% 
  summarise(percentage = n() / nrow(result) * 100)

ggplot(result_summary) +
  geom_col(aes(model, percentage)) + 
  geom_text(aes(model, percentage, label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5), color = "white") +
  labs(x = "Model", "%", title = glue("{format(start_time,dt_frmt)}  to  {format(end_time,dt_frmt)}")) +
  theme_bw() 

# save mod hourly results to csv
result %>% 
  mutate(datetime = format(datetime, "%d/%m/%Y %H:%M:%S")) %>% 
  write_csv("outputs/mod_historic.csv")

ggplot(result, aes(datetime, model)) + 
  geom_point()

# Extract the hour from the datetime column
result$hour <- format(result$datetime, "%H")

# Group by the hour and model, and count occurrences
hourly_model_counts <- result %>%
  group_by(hour, model) %>%
  summarise(count = n())

# Plot the hourly model counts as a bar chart
ggplot(hourly_model_counts, aes(x = hour, y = count, fill = model)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Hour of the Day") +
  ylab("Count") +
  scale_fill_discrete(name = "Model")

library(tdcR)

site <- "HY Anatoki at Happy Sams"
from <- "20230401"
to <- "Now"

rf_data <- get_data_site_measurement(site = site, measurement = "Rainfall", method =  "Total", interval = "1 hour", from = from, to = to) %>% 
  mutate(datetime = with_tz(datetime, tz = "NZ"))

rf_data <- rf_data %>% 
  mutate(
    average_hrly = sum(value)/as.numeric((max(datetime) - min(datetime)), "hours"),
    single_mass =  value - average_hrly,
    sm_cum_sum = cumsum(single_mass)
  )

ggplot(rf_data, aes(datetime, sm_cum_sum)) + 
  geom_line() +
  labs(title = site) +
  theme_bw() 

rf_model_combined <- rf_data %>% 
  rename(rainfall_mm = value) %>% 
  left_join(result, by = "datetime")

ggplot(rf_model_combined, aes(datetime, rainfall_mm )) + 
  geom_col(aes(fill = model)) +
  labs(title = site) +
  theme_bw() 