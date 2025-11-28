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
#' @description Optimizes bike placement
#' @param trip_log data frame
#' @param final_inventory numeric vector
#' @return data frame with three columns

