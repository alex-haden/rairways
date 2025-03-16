library(ggplot2)
suppressPackageStartupMessages(library(plotly))

cities <- read.csv("csvs/cities.csv")
flights <- read.csv("csvs/flights.csv") %>% mutate(date = as.Date(date))
passengers <- read.csv("csvs/passengers.csv") %>% mutate(date = as.Date(paste(year, month, "01", sep = "-")))

flights_joined <- flights %>% left_join(cities[,c("id", "city")], by = c("origin_city" = "city")) %>%
  left_join(cities[,c("id", "city")], by = c("destination_city" = "city"), suffix = c("_origin", "_destination"))

flight_pricing <- function(flight_id, num_passengers_int, cabin_class, is_new_customer_bool, luggage_weight_int) {
  
  #Sys.sleep(5)
  
  flight_index <- which(flights_joined$flight_id == flight_id)
  
  # Variables
  cabin_class_multipler <- ifelse(cabin_class == 'first', 5, ifelse(cabin_class == 'business', 2.5, 1))
  demand_coefficient <- 0.5 + flights_joined$popularity[flight_index] / 10
  is_mon_or_fri <- ifelse(flights_joined$day[flight_index] %in% c("monday", "friday"), 25, 0)
  luggage_cost <- 1.5 * ceiling(luggage_weight_int / 10) * 10
  does_have_discount <- ifelse(is_new_customer_bool, 0.95, 1)
  
  fare1_passenger <- (0.15 * flights_joined$distance_miles[flight_index] * cabin_class_multipler * demand_coefficient) +
    is_mon_or_fri + luggage_cost
  total_fare <- fare1_passenger * num_passengers_int * does_have_discount
  ceiling(total_fare)
  
}

generate_passenger_trajectory_plot <- function() {
  #plot(passengers$date, passengers$passengers)
  ggplot(passengers, aes(x = date, y = passengers)) + geom_line() + geom_point() +
    labs(title = "Monthly Passengers Over Time", x = "Date", y = "Number of Passengers") + theme_minimal() +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}