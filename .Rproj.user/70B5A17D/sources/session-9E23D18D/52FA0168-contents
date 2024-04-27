#' A function for extracting the time series for a specific station by station id.
#'
#' Get a dataframe for a specific station with optional arguments regarding
#' the date.
#'
#' @param station_id WBAN ID of the weather station
#'
#' @return a dataframe for a specific weather station. For information about
#' each column please refer to the weather_data documentation
#'
#' @examples
#' #Get data regarding station ID 3047
#' time_series(3047)
#'
#' #Get data regarding station ID 3047 from 2003-05-22 to 2003-05-30
#' time_series(3047,"2003-05-22","2003-05-30")
#' @export
  time_series <- function(station_id, start_date = NULL, end_date = NULL) {
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

