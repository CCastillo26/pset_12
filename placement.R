library(tidyverse)
source("simulation.R")

#` Calculate simulation metrics
#' @description Calculates simulation metrics for failed trips, unused bikes
#' @param trip_log data frame
#' @param final_inventory numeric vector
#' @return data frame with three columns
calculate_metrics <- function(trip_log, final_inventory) {
  stations <- sort(unique(arrival_rates$start_station))
  
  failed_trips <- length(stations)
  unused_bikes <- length(stations)
  
  for (i in 1:length(stations)) {
    s <- stations[i]
    
    failed_trips[i] <- sum(trip_log$start_station == s &
                             trip_log$success == 0) # Calculate failed trips
    
    if (s %in% names(final_inventory)) {
      unused_bikes[i] <- final_inventory[s] 
    } else { 
      unused_bikes[i] <- 0 # Calculate bikes left at station
    }
  }
  
  metrics <- data.frame(station = stations, failed_trips = failed_trips,
                        unused_bikes = unused_bikes)
  
  return(metrics)
}


#` Optimize bike placement
#' @description Optimizes bike placement based on successes, failures
#' @param metrics data frame
#' @param total_bikes numeric
#' @return data frame with two columns

optimize_placement <- function(metrics, total_bikes) {
  stations <- metrics$station
  num_stations <- length(stations)
  
  base_bikes <- floor(total_bikes / num_stations)
  recommended_bikes <- rep(base_bikes, num_stations)
  
  remaining_bikes <- total_bikes - sum(recommended_bikes)
  
  need_score <- metrics$failed_trips - metrics$unused_bikes
  
  if (remaining_bikes > 0) {
    station_order <- order(need_score, decreasing = TRUE)
    position <- 1
    
    while (remaining_bikes > 0) {
      station_index <- station_order[position]
      recommended_bikes[station_index] <- recommended_bikes[station_index] + 1
      remaining_bikes <- remaining_bikes - 1
      
      position <- position + 1
      if (position > length(station_order)) {
        position <- 1
      }
    }
  }
  
  placement <- data.frame(station = stations, 
                          recommended_bikes = recommended_bikes)
  
  return(placement)
}
