library(testthat)
source("estimation.R")
source("simulation.R")
source("placement.R")

# TEST 1: simulate_arrival_times()
toy_arrival_rates <- data.frame(start_station = "5", 
                                end_station = "10", hour = 0, avg_trips = 1,
                                avg_avail = 1, mu_hat = 1)

toy_arrivals <- simulate_arrival_times(toy_arrival_rates, day = 24)

# Runs successfully without error
expect_true(all(toy_arrivals$time >= 0 & toy_arrivals$time <= 24))
expect_true(all(toy_arrivals$start_station == "10"))



# TEST 2: simulate_days()
arrivals <- data.frame(start_station = c("5", "5"), time = c(1, 2))
initial_bikes <- c("5" = 1)

result <- simulate_days(arrivals, initial_bikes)

trip_log <- result$trip_log
final_inventory <- result$final_inventory

# Runs successfully without error
expect_true(sum(final_inventory) <= sum(initial_bikes))
expect_equal(length(trip_log$success), nrow(arrivals))



# TEST 3: calculate_metrics()




# TEST 4: optimize_placement()