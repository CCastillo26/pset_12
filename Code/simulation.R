library(tidyverse)
source("estimation.R")

#` Simulate arrival times
#' @description Simulates arrival times using lambda_max and thinning
#' @param arrival_rates data frame
#' @param day numeric
#' @return data frame with two columns for start_station and time
simulate_arrival_times <- function(arrival_rates, day = 24) {
  lambda_max <- max(arrival_rates$mu_hat)
  
  stations <- sort(unique(arrival_rates$start_station))
  
  all_arrivals <- data.frame(
    start_station = character(0),
    time = numeric(0)) # Store values in data frame
  
  for (i in stations) {
    station_rates <- arrival_rates[arrival_rates$start_station == i, ]
    station_rates <- station_rates[order(station_rates$hour), ]
    
    t_i <- 0
    arrival_times <- numeric(0)
    
    while (t_i < day) {
      e_j <- rexp(1, rate = lambda_max) # Calculate e_j
      t_i <- t_i + e_j # Calculate sum t_i
       
      if (t_i >= day) {
        break
      }
      
      h_i <- floor(t_i) # Correspond to hour of arrival
      lambda_row <- station_rates[station_rates$hour == h_i, ]
      
      if (nrow(lambda_row) > 0) {
        lambda_ti <- lambda_row$mu_hat[1]
        
        if (lambda_ti > 0) {
          p_i <- lambda_ti / lambda_max # Calculate acceptance prob p_i
          u_i <- runif(1) # Draw u_i ~ Unif(0, 1)
          
          if (u_i <= p_i) {
            arrival_times <- c(arrival_times, t_i)
          }
        }
      }
    }
    
    if (length(arrival_times) > 0) {
     station_df <- data.frame(
       start_station = rep(i, length(arrival_times)),
       time = arrival_times
      )
      all_arrivals <- rbind(all_arrivals, station_df)
    }
  }

  return(all_arrivals)
}
  

#` Simulate bike use over a day
#' @description Simulates bike use over a day
#' @param arrivals data frame
#' @param initial_bikes numeric vector
#' @return list with two elements

simulate_days <- function(arrivals, initial_bikes) {
  arrivals <- arrivals[order(arrivals$time), ]
  
  inventory <- initial_bikes
  
  n_trips <- nrow(arrivals)
  success <- rep(0, n_trips) # Let 1 = success, 0 = failure
  
  if (n_trips > 0) {
    for (i in 1:n_trips) {
      s_i <- arrivals$start_station[i]
      
      if (inventory[s_i] > 0) {
        success[i] <- 1
        inventory[s_i] <- inventory[s_i]- 1 # Remove bike if successful
      } else {
      }
    }
  }
  arrivals$success <- success
  
  return(list(trip_log = arrivals, final_inventory = inventory))
}
