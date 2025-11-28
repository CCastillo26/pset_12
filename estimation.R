library(tidyverse)

#` Estimate arrival rates
#' @description Estimates arrival rates using NHPP
#' @param bike_df data frame
#' @return data frame with four columns
bike_df <- read.csv("/Users/charlycastillo/Downloads/sample_bike.csv")

# Script from EdStem
estimate_arrival_rates <- function(bike_df) {
  bike_df <- bike_df %>%
    mutate(
      start_time = as.POSIXct(start_time, format = "%Y-%m-%d %H:%M:%S"),
      end_time = as.POSIXct(end_time, format = "%Y-%m-%d %H:%M:%S")
    )
  
  # Compute average number of trips per hour between pairs
  x_hat <- bike_df %>%
    mutate(hour = hour(start_time)) %>%
    filter(start_station != "R", end_station != "R") %>%
    group_by(start_station, end_station, hour) %>%
    summarize(avg_trips = n() / n_distinct(as_date(start_time)), 
              .groups = "drop") 
  
  # Pivot longer to get change in count 
  bike_df$end_station <- as.character(bike_df$end_station)
  trips_long <- bike_df %>%
    pivot_longer(cols = c("start_station", "start_time", 
                          "end_station", "end_time"),
                 names_to = c("type", ".value"),   
                 names_pattern = "(start|end)_(.*)") %>%
    mutate(change = ifelse(type == "start", -1, 1),
           hour = hour(time)) %>%
    select(station, time, hour, change)
  
  # Add hour markers for cumulative time
  dates <- unique(as_date(trips_long$time))
  hours <- c(seq(0, 23, 1), seq(0, 23, 1) + 0.9999999)
  stations <- unique(trips_long$station)
  hr_pts <- expand.grid(time = dates, hour = hours, 
                        station = stations) %>%
    mutate(time = as.POSIXct(time) + hour * 60 * 60,
           hour = hour(time))
  hr_pts$change <- 0
  trips_long <- rbind(trips_long, hr_pts)
  
  # Find average availability 
  alpha_hat <- trips_long %>%
    group_by(station) %>%
    filter(station != "R") %>%
    arrange(time) %>% 
    mutate(count = cumsum(change),
           date = as_date(time)) %>%
    group_by(station, hour, date) %>%
    summarize(time_avail = 
                sum(difftime(time, lag(time), units="hours") * (count > 0), 
                    na.rm = TRUE)) %>%
    summarize(avg_avail = mean(time_avail)) %>%
    mutate(avg_avail = round(as.numeric(avg_avail), digits = 4)) %>%
    ungroup()
  
  # Compute arrival rates
  mu_hat <- x_hat %>%
    left_join(alpha_hat, by = c("start_station" = "station", "hour")) %>%
    mutate(mu_hat = ifelse(avg_avail > 0, avg_trips / avg_avail, NA))
  
  return(mu_hat) # Corresponds with lambda
}

# Estimate arrival rates
arrival_rates <- estimate_arrival_rates(bike_df)
