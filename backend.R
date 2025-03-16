library(plumber)

suppressPackageStartupMessages(library(dplyr))
library(grid)
library(future)
future::plan("multisession")

source("algorithms.R")

internal_endpoints <- c("/passenger-numbers", "/plotly", "/passenger-data", "/passenger-report")

#counter <- 0

#* Set CORS policy
#* @filter cors
function(req, res) {
  
  res$setHeader("Access-Control-Allow-Origin", "18.175.223.13:8000") # exact domain
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, PUT")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization, API_KEY")
  
  # If the request is a preflight (OPTIONS), return a successful response
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return() # Respond with an empty body
  }
  
  forward()
  
}

#* Store geolocation in req object
#* @filter geolocation
function(req) {
  # Use req$REMOTE_ADDR value and plug into a third-party geolocation tool
  location <- list(latitude = runif(1, min = -90, max = 90), longitude = runif(1, min = -180, max = 180))
  req$location <- location
  forward()
}

#* Log incoming requests
#* @filter logger
function(req) {
  
  #counter <<- counter + 1
  
  cat(as.character(Sys.time()), "-",
      req$REQUEST_METHOD, req$PATH_INFO, "-",
      req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "Coordinates: ", req$location$latitude, " / ", req$location$longitude, "\n")
  plumber::forward()
}

#* Authorise access to internal-only endpoints
#* @filter internal-auth
function(req, res) {
  
  #print(counter)
  
  if (req$PATH_INFO %in% internal_endpoints) {
    if (is.null(req$HTTP_API_KEY) || req$HTTP_API_KEY != "abc123") {
      res$status <- 401
      return(jsonlite::unbox("Invalid API Key"))
    }
  }
  forward()
}

#* Return the classic Hello World statement
#* @serializer unboxedJSON
#* @get /hello-world
function(){
  "Hello, World!"
}

#* Echo back the input
#* @param msg:str The message to echo
#* @serializer unboxedJSON
#* @get /echo
function(msg=""){
  list(msg = paste0("The message is: '", msg, "'"))
}

#* Return the standard meal the customer will receive
#* @param flight_num:str The customer's flight number
#* @param cabin_class:str The customer's flight class (economy, business or first)
#* @serializer unboxedJSON
#* @get /standard-meal
function(res, flight_num, cabin_class = 'economy'){
  
  if (missing(flight_num)) {
    res$status <- 400
    return("Parameter flight_num must have a value")
  }
  
  # Verify that flight_num is a real flight number...
  
  if (!cabin_class %in% c("economy", "business", "first")) {
    res$status <- 400
    return("cabin_class parameter should be one of either 'economy', 'business' or 'first'")
  }
  
  # Logic to extract the meal goes here:
  meals <- c("Fish and Chips", "Full English Breakfast", "Pasta")
  return(sample(meals, 1))
}

#* Return whether the given seat is available or not for the customer's flight
#* @param booking_ref:str The customer's booking reference
#* @param seat_num:str The requested seat number
#* @serializer unboxedJSON
#* @get /seat-availability
function(res, booking_ref, seat_num){
  
  if (missing(booking_ref)) {
    res$status <- 400
    return("Parameter booking_ref must have a value")
  }
  
  if (missing(seat_num)) {
    res$status <- 400
    return("Parameter seat_num must have a value")
  }
  
  # Check if booking_ref is valid
  # Check if seat_num is valid
  
  # Logic to check if seat is available:
  is_available <- c("Seat is available", "Seat is not available")
  return(sample(is_available, 1))
  
}

#* Return the number of loyalty points a customer has
#* @param miles:numeric Total customer mileage with us
#* @serializer unboxedJSON
#* @get /loyalty-points
function(res, miles) {
  
  if (missing(miles)) {
    res$status <- 400
    return("Parameter miles must have a value")
  }
  
  miles_numeric <- as.numeric(miles)
  
  if (is.na(miles_numeric)) {
    res$status <- 400
    return("Parameter miles is not numeric")
  }
  
  # Loyalty points calculation:
  loyalty_points <- round(miles_numeric/100)
  loyalty_points
  
}

#* Return the number of flights arriving or departing at a city on a particular date
#* @param city:str The city to see flights for
#* @param date:str The date to see flights for. Should be ISO 8601 formatted string, i.e. '2025-12-31'
#* @param is_arrival:bool Whether we are interested in arrivals (as opposed to departures)
#* @serializer json
#* @get /flight-frequency-count
function(res) {
  res$status <- 301
  res$setHeader("Location", "/flight-frequency")
}

#* Return the flights arriving or departing at a city on a particular date
#* @param city:str The city to see flights for
#* @param date:str The date to see flights for. Should be ISO 8601 formatted string, i.e. '2025-12-31'
#* @param is_arrival:bool Whether we are interested in arrivals (as opposed to departures)
#* @serializer json
#* @get /flight-frequency
function(res, city, date = format(Sys.Date(), '%Y-%m-%d'), is_arrival = TRUE) {
  
  if (missing(city)) {
    res$status <- 400
    return("Parameter city must have a value")
  }
  
  if (!tolower(city) %in% tolower(cities$city)) {
    res$status <- 400
    return("City name is either not a real city or we currently do not serve that city")
  }
  
  is_arrival_bool <- as.logical(is_arrival)
  date_as_date <- as.Date(date)
  
  if (is.na(is_arrival_bool)) {
    res$status <- 400
    return("Parameter is_arrival is not a boolean")
  }
  
  if (is.na(date_as_date)) {
    res$status <- 400
    return("Parameter date is not a valid date")
  }
  
  # Logic to retrieve relevant flights:
  if (is_arrival_bool) {
    flights_filtered <- flights %>% filter(tolower(destination_city) == tolower(city), date == date_as_date)
  } else {
    flights_filtered <- flights %>% filter(tolower(origin_city) == tolower(city), date == date_as_date)
  }
  
  #nrow(flights_filtered)
  flights_filtered
  
}

#* Return a description about a city
#* @serializer unboxedJSON
#* @get /cities/<name>
function(res, name) {
  
  decodedName <- URLdecode(name)
  
  if (!tolower(decodedName) %in% tolower(cities$city)) {
    res$status <- 400
    return("City name is either not a real city or we currently do not serve that city")
  }
  
  # Output the description
  description <- cities$description[tolower(cities$city) == tolower(decodedName)]
  description
  
}

#* Return the number of flights between two cities
#* @param days_of_the_week:[str] The days of the week, in text
#* @serializer unboxedJSON
#* @post /flights/<origin_id:int>/to/<destination_id:int>
function(origin_id, destination_id, days_of_the_week) {
  
  #print(typeof(origin_id))
  
  # Extract all matching flights:
  flights_filtered <- flights_joined %>% filter(id_origin == origin_id, id_destination == destination_id)
  if (!missing(days_of_the_week)) {
    flights_filtered <- flights_filtered %>% filter(day %in% tolower(days_of_the_week))
  }
  
  nrow(flights_filtered)
  
}

#* Return the price of a single flight
#* @param num_passengers:int The number of passengers
#* @param cabin_class:str The cabin class of all passengers
#* @param is_new_customer:bool Is the lead passenger a new customer
#* @param luggage_weight:int The weight allowance for all passengers, in kg
#* @serializer unboxedJSON
#* @get /price/<flight_id:int>
function(res, flight_id, num_passengers, cabin_class = 'economy', is_new_customer = FALSE, luggage_weight = 0) {
  
  # Empty param validation:
  if (missing(num_passengers)) {
    res$status <- 400
    return("Parameter num_passengers is empty")
  }
  
  promises::future_promise({
    
    # Type casting:
    num_passengers_int <- as.integer(num_passengers)
    is_new_customer_bool <- as.logical(is_new_customer)
    luggage_weight_int <- as.integer(luggage_weight)
    
    # Casting validation
    if (is.na(num_passengers_int)) {
      res$status <- 400
      return("Parameter num_passengers is not an integer")
    }
    if (is.na(is_new_customer_bool)) {
      res$status <- 400
      return("Parameter is_new_customer is not a boolean")
    }
    if (is.na(luggage_weight_int)) {
      res$status <- 400
      return("Parameter luggage_weight is not an integer")
    }
    
    # Value validation
    if (!flight_id %in% flights_joined$flight_id) {
      res$status <- 400
      return("Parameter flight_id is not a real flight id")
    }
    if (!cabin_class %in% c("economy", "business", "first")) {
      res$status <- 400
      return("cabin_class parameter should be one of either 'economy', 'business' or 'first'")
    }
    if (num_passengers_int < 1 || num_passengers_int > 10) {
      res$status <- 400
      return("Parameter num_passengers must be between 1 and 10 inclusive")
    }
    if (luggage_weight_int < 0 || luggage_weight_int > 40) {
      res$status <- 400
      return("Parameter luggage_weight must be non-negative, but no more than 40(kg)")
    }
    
    # Pricing model:
    flight_pricing(flight_id, num_passengers_int, cabin_class, is_new_customer_bool, luggage_weight_int)
    
  })
  
}

#* Return all the destinations that there are direct flights to from this city
#* @param city:str The city of interest
#* @serializer json
#* @get /destinations-from
function(res, city) {
  
  if (missing(city)) {
    res$status <- 400
    return(jsonlite::unbox("Parameter city must have a value"))
  }
  
  if (!tolower(city) %in% tolower(cities$city)) {
    res$status <- 400
    return(jsonlite::unbox("City name is either not a real city or we currently do not serve that city"))
  }
  
  # Logic to extract all destinations:
  filtered_flights <- flights_joined %>% filter(tolower(origin_city) == tolower(city))
  unique(filtered_flights$destination_city)
  
}

#* Return the HTML for the about-us page
#* @serializer html
#* @get /about-us
function(){
  cities <- cities$city
  paste0("<html><h1>Rairways</h1><h2 style='color:red'> We fly to: ", paste(cities, collapse = ", "), " </h2></html>")
}

#* Return an image of a plot of passenger numbers over time
#* @serializer png list(width = 1600, height = 1000, bg = 'pink')
#* @get /passenger-numbers
function() {
  print(generate_passenger_trajectory_plot())
}

#* Serve an interactive Plotly plot
#* @serializer htmlwidget
#* @get /plotly
function() {
  ggplotly(generate_passenger_trajectory_plot())
}

#* Return the passengers data in CSV format
#* @serializer csv
#* @get /passenger-data
function() {
  passengers %>% select(year, month, passengers)
}

#* Return a passenger report in PDF format
#* @serializer contentType list("application/pdf")
#* @get /passenger-report
function() {
  temp_pdf <- tempfile()
  pdf(temp_pdf)
  grid.text("Rairways number of passengers 2021-2024", 
            x = 0.5, y = 0.5, gp = gpar(fontsize = 14, fontface = "bold"))
  grid.text("Passenger numbers have been increasing rapidly year-on-year, with 
  the trajectory also showing a significant seasonal affect, with numbers 
  highest in September and October, and lower in February and March. In 
  October 2024 we reached a peak of 83k passengers.",
            x = 0.5, y = 0.15, gp = gpar(fontsize = 12))
  print(generate_passenger_trajectory_plot())
  dev.off()
  
  readBin(temp_pdf, "raw", n = file.info(temp_pdf)$size)
}

#* Return all matching flights
#* @param origin:str The city to fly from
#* @param dates:[str] The dates to see flights for. Should be an ISO 8601 formatted string, i.e. '2025-12-31'
#* @serializer json
#* @post /flight-search
function(res, origin, dates) {
  
  if (missing(origin)) {
    res$status <- 400
    return(jsonlite::unbox("Parameter origin does not have a value"))
  }
  if (missing(dates)) {
    res$status <- 400
    return(jsonlite::unbox("Parameter dates does not have a value"))
  }
  
  if (length(dates) > 100) {
    res$status <- 400
    return("Parameter dates has too many values")
  }
  
  if (!tolower(origin) %in% tolower(flights_joined$origin_city)) {
    res$status <- 400
    return(jsonlite::unbox("Parameter origin is either not a real city or we currently do not fly from that city"))
  }
  
  dates_as_date <- as.Date(dates)
  
  if (any(is.na(dates_as_date))) {
    res$status <- 400
    return(jsonlite::unbox("At least one value in parameter dates is not a valid ISO 8601 string"))
  }
  
  # Logic to extract flights:
  flights_filtered <- flights_joined %>% filter(tolower(origin_city) == tolower(origin), date %in% dates_as_date) %>%
    select(flight_id, date, origin_city, destination_city, duration_hours, day)
  flights_filtered
  
}

#* Submit user passport details to be stored
#* @param email:str The user's email address
#* @param surname:str The user's last name
#* @param nationality:str The user's passport country of origin
#* @param passport_num:str The user's passport number
#* @param passport_expiry_month:int The user's passport expiry month
#* @param passport_expiry_year:int The user's passport expiry year
#* @serializer unboxedJSON
#* @post /save-passport
function(res, email, details) {
  
  surname <- details$surname
  nationality <- details$nationality
  passport_num <- details$passport_num
  passport_expiry_month <- details$passport_expiry$month
  passport_expiry_year <- details$passport_expiry$year
  
  if (missing(email) || missing(surname) || missing(nationality) || missing(passport_num) || 
      missing(passport_expiry_month) || missing(passport_expiry_year)){
    res$status <- 400
    return("Endpoint should contain a parameter for each of email, surname, nationality, passport number, expiry month and expiry year")
  }
  
  # Casting validation
  if (is.na(passport_expiry_month) || is.na(passport_expiry_year)) {
    res$status <- 400
    return("Either passport expiry month or year are not a valid integer")
  }
  
  # Logic ...
  is_successful <- TRUE
  
  if (is_successful) {
    res$status <- 201
    list(surname = surname, nationality = nationality, passport_num = passport_num, passport_expiry_month = passport_expiry_month,
         passport_expiry_year = passport_expiry_year)
  } else {
    res$status <- 500
    "Error in remembering your details, please try again"
  }
  
}

#* Submit user check-in status for their next flight
#* @serializer unboxedJSON
#* @post /check-in/<user_id:int>/<check_in_status:bool>
#* @put /check-in/<user_id:int>/<check_in_status:bool>
function(req, res, user_id, check_in_status) {
  
  # Verify that user_id is a valid user id
  
  # Logic for storing data
  is_successful <- TRUE
  
  if (is_successful) {
    if (req$REQUEST_METHOD == "POST") {
      res$status <- 201
    }
    paste("User id", user_id, "has successfully updated their check-in status to", check_in_status)
  } else {
    res$status <- 500
    "Error in updating check-in status, please try again"
  }
  
}

#* Update user currency
#* @param currency:str The user's preferred currency
#* @serializer unboxedJSON
#* @put /users/<user_id:int>/preferences/currency
function(res, user_id, currency) {
  
  if (missing(currency)) {
    res$status <- 400
    return("Parameter currency must have a value")
  }
  
  # Verify that user_id is a valid user id
  # Verify that currency is a valid currency
  
  # Logic for updating data
  is_successful <- TRUE
  
  if (is_successful) {
    paste("User id", user_id, "has successfully updated their preferred currency to", currency)
  } else {
    res$status <- 500
    "Error in updating currency, please try again"
  }
  
}

#* Delete a booking
#* @serializer unboxedJSON
#* @delete /bookings/<booking_id:int>
function(res, booking_id) {
  
  # Verify that booking_id is a valid booking id
  
  # Logic for deleting data
  is_successful <- TRUE
  
  if (is_successful) {
    res$status <- 204
    paste("Booking id", booking_id, "has been successfully deleted")
  } else {
    res$status <- 500
    "Error in deleting booking, please try again"
  }
  
}

#* Book a flight and return confirmation number. Requires a details object which contains 3 attributes
#* @param email:str An email address to provide confirmation
#* @param num_passengers:int The number of passengers
#* @param cabin_class:str The cabin class of all passengers
#* @param luggage_weight:int The weight allowance for all passengers, in kg
#* @serializer unboxedJSON
#* @post /book/<flight_id:int>
function(req, res, flight_id, email, details) {
  
  # print(ls(req))
  # print("")
  # print(ls(req$argsBody))
  
  num_passengers <- details$num_passengers
  cabin_class <- details$cabin_class
  luggage_weight <- details$luggage_weight
  
  # Empty param validation:
  if (missing(num_passengers) || missing(email)) {
    res$status <- 400
    return("Endpoint should contain a parameter for each of email and num_passengers")
  }
  
  # Type casting:
  num_passengers_int <- as.integer(num_passengers)
  luggage_weight_int <- as.integer(luggage_weight)
  
  # Casting validation
  if (is.na(num_passengers_int)) {
    res$status <- 400
    return("Parameter num_passengers is not an integer")
  }
  if (is.na(luggage_weight_int)) {
    res$status <- 400
    return("Parameter luggage_weight is not an integer")
  }
  
  # Value validation
  if (!flight_id %in% flights_joined$flight_id) {
    res$status <- 400
    return("Parameter flight_id is not a real flight id")
  }
  if (!cabin_class %in% c("economy", "business", "first")) {
    res$status <- 400
    return("cabin_class parameter should be one of either 'economy', 'business' or 'first'")
  }
  if (num_passengers_int < 1 || num_passengers_int > 10) {
    res$status <- 400
    return("Parameter num_passengers must be between 1 and 10 inclusive")
  }
  if (luggage_weight_int < 0 || luggage_weight_int > 40) {
    res$status <- 400
    return("Parameter luggage_weight must be non-negative, but no more than 40(kg)")
  }
  
  # Logic for storing booking...
  is_successful <- TRUE
  booking_id <- sample(1:1000000, 1)
  
  # Send confirmation email to email address
  
  if (is_successful) {
    res$status <- 201
    list(booking_id = booking_id, num_passengers = num_passengers, cabin_class = cabin_class, luggage_weight = luggage_weight)
  } else {
    res$status <- 500
    "Your booking could not be completed, please try again"
  }
  
}