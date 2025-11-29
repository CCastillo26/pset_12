library(testthat)
source("estimation.R")
source("simulation.R")
source("placement.R")

# TEST 1: simulate_arrival_times()
toy_rates <- data.frame(start_station = "5", 
                                end_station = "10", hour = 0, avg_trips = 1,
                                avg_avail = 1, mu_hat = 1)

arrivals <- simulate_arrival_times(toy_rates, day = 24)

expect_true(all(arrivals$time >= 0 & arrivals$time <= 24))
expect_true(all(arrivals$start_station == "5"))



# TEST 2: simulate_days()
toy_trips <- data.frame(start_station = c("5", "5"), time = c(1, 2))

toy_bikes <- c("5" = 1)

days <- simulate_days(toy_trips, toy_bikes)

days_log <- days$trip_log
days_inventory <- days$final_inventory

expect_true(sum(days_inventory) <= sum(toy_bikes))
expect_equal(length(days_log$success), nrow(toy_trips))



# TEST 3: calculate_metrics()
toy_log <- data.frame(start_station = c("5", "5", "10"), time = c(1, 2, 3), 
                       success = c(1, 0, 0))

toy_inventory <- c("5" = 1, "10" = 0) # Test failed trips
metrics <- calculate_metrics(toy_log, toy_inventory)

expect_equal(nrow(metrics), 2)

expect_equal(metrics$failed_trips[metrics$station == "5"], 1)
expect_equal(metrics$unused_bikes[metrics$station == "5"], 1)

expect_equal(metrics$failed_trips[metrics$station == "10"], 1)
expect_equal(metrics$unused_bikes[metrics$station == "10"], 0)



# TEST 4: optimize_placement()
toy_metrics <- data.frame(station = c("5", "10"), failed_trips = c(3, 0),
                          unused_bikes = c(0, 0))

placement <- optimize_placement(toy_metrics, total_bikes = 10)

expect_equal(sum(placement$recommended_bikes), 10)

bikes_5  <- placement$recommended_bikes[placement$station == "5"]
bikes_10 <- placement$recommended_bikes[placement$station == "10"]
expect_true(bikes_5 >= bikes_10) # Since Station 5 has more failed trips