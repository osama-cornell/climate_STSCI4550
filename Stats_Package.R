
library(dplyr)
library(maps)
library(fields)
library(sf)

---------------------------------------------------
#A function for extracting the time series for a specific station by station id.
#It should have optional arguments for the starting date and ending date of
#the time series.
time_series <- function(weather_data, station_id, start_date = NULL, end_date = NULL) {
  ts_data <- filter(weather_data, WBANNO == station_id)
  # If there are start and end dates, apply them
  if (!is.null(start_date)) {
    ts_data <- subset(ts_data, LST_DATE >= start_date)
  }
  if (!is.null(end_date)) {
    ts_data <- subset(ts_data, LST_DATE <= end_date)
  }
  return(ts_data)
}

# A function for estimating the yearly cycle for one station.
# A yearly cycle is simply the expected temperature on each day of the year.
# The function should return a data frame with row for each day,
# a column for day number (1-365),
# and a column for the expected average temperature on each day.
yearly_cycle <- function(weather_data, station_id) {
  daily_station_data <- weather_data %>%
    filter(WBANNO == station_id) %>%
    mutate(DAY = as.numeric(format(dates, "%j"))) %>%
    select(DAY, T_DAILY_AVG) %>%
    arrange(DAY)
  daily_station_data$DAY <- rank(daily_station_data$DAY, ties.method = "min")
}

# A function for estimating the trend of temperatures over time,
# in units of degrees Celsius per year.
temp_trends <- function(weather_data) {
  trend_model <- lm(TEMP ~ as.numeric(LST_DATE), data = weather_data)
  return(coef(trend_model)[2])
}

# A function for creating a grid of points that fall within the contiguous USA.
# You may consider using external packages to get map data
# and find points inside a polygon.
# Your function should have argument(s) for controlling the resolution of the grid.
grid_points <- function(resolution = 0.1) {
  # Get the borders of the contiguous USA
  usa <- map("state", plot = FALSE, fill = TRUE)

  # Define the grid within the bounding box of the contiguous USA
  x <- seq(min(usa$range[1, ]), max(usa$range[1, ]), by = resolution)
  y <- seq(min(usa$range[2, ]), max(usa$range[2, ]), by = resolution)

  # Create a grid of points
  grid_points <- expand.grid(x = x, y = y)

  # Convert grid points to a matrix
  grid_matrix <- cbind(grid_points$x, grid_points$y)

  # Use inpoly() to check if points are within the polygon
  inside_usa <- inpoly(grid_matrix, usa$x, usa$y)

  # Subset grid points that fall within the USA
  grid_points <- grid_points[inside_usa, ]

  return(grid_points)
}

# A function for interpolating data from the stations to a grid points
# within the contiguous USA.
station_grid_points <- function(weather_data, resolution = 0.1,
                                variable = "T_DAILY_AVG", method = "idw") {
  # Get grid points
  usa_grid <- grid_points(resolution)

  # Subset weather data to stations within the contiguous USA
  usa <- map("state", plot = FALSE, fill = TRUE)
  usa_stations <- subset(weather_data, st_within(st_geometry(weather_data), usa))

  # Extract longitude and latitude of stations
  lon <- usa_stations$LONGITUDE
  lat <- usa_stations$LATITUDE

  # Extract values of the variable to interpolate
  values <- usa_stations[[variable]]

  # Perform interpolation
  interp_result <- interp(lon, lat, values, xi = cbind(usa_grid$x, usa_grid$y), method = method)

  return(interp_result)
}


# A function for plotting the gridded interpolations on a map.
plot_interpolations2 <- function(interp_result, grid_points, variable_name) {
  # Convert interpolated results to an sf object
  interp_sf <- st_as_sf(interp_result$z, coords = c("x", "y"))

  # Convert grid points to an sf object
  grid_sf <- st_as_sf(grid_points, coords = c("x", "y"))

  # Create a ggplot object
  p <- ggplot() +
    geom_sf(data = interp_sf, aes(fill = .value)) +
    geom_sf(data = grid_sf, color = "black", fill = NA) +
    scale_fill_viridis_c(name = variable_name, na.value = "transparent") +
    theme_minimal() +
    labs(title = paste("Gridded Interpolation of", variable_name))

  # Plot the ggplot object
  print(p)
}

inter_plot <- function(inter){
  ggplot(df, aes(x = df[, 1], y = df[, 2],
  color = df[, 3])) + geom_point(
  x = long, y = lat, color = inter)
}



