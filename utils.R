source("estimation.R")
source("simulation.R")
source("placement.R")

set.seed(123) # Ensure reproducibility

arrival_rates <- estimate_arrival_rates(bike_df)
arrivals <- simulate_arrival_times(arrival_rates, day = 24)

stations <- sort(unique(arrival_rates$start_station))
num_stations <- length(stations)

fleet_sizes <- c(100, 200, 500) # Example fleet sizes for table

# Store placements
final_placement <- data.frame(fleet_size = numeric(0), station = character(0), 
                              recommended_bikes = numeric(0))

#` Calculate simulation metrics
#' @description Calculates simulation metrics for failed trips, unused bikes
#' @param trip_log data frame
#' @param final_inventory numeric vector
#' @return data frame with three columns
for (total_bikes in fleet_sizes) {
  base_bikes <- floor(total_bikes / num_stations) # From placement.R
  recommended_bikes <- rep(base_bikes, num_stations) # From placement.R
  names(recommended_bikes) <- stations
  
  simulation <- simulate_days(arrivals, recommended_bikes)
  metrics <- calculate_metrics(simulation$trip_log, 
                               simulation$final_inventory)
  placement <- optimize_placement(metrics, total_bikes)
  
  placement$fleet_size <- total_bikes # Add column in table
  
  # Reorder columns
  placement <- placement[, c("fleet_size", "station", "recommended_bikes")]
  
  final_placement <- rbind(final_placement, placement)
}

print(final_placement)