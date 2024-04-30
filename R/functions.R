library(dplyr)
library(maps)
library(fields)
library(sf)
data("weather_data")

#' A function for extracting the time series for a specific station by station id.
#'
#' Get a dataframe for a specific station with optional arguments regarding
#' the date.
#'
#' @param station_id WBAN ID of the weather station
#' @param start_date Start date of time series
#' @param end_date End date of time series
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

  #---------------------------------------------------------------------------

  #' A function to estimate the yearly cycle for a single station
  #'
  #' Returns the estimated expected temperature for each day of the year.
  #' Uses a second order regression model
  #'
  #' @param station_id WBAN ID of the weather station
  #'
  #' @return a dataframe for a specific weather station. It has the following
  #' columns.
  #' \itemize{
  #'     \item \code { @context } metadata
  #'     \item \code {day} Each day of the year (1-365)
  #'     \item \code {expected_temp} Expected daily temperature
  #'}
  #' @examples
  #' #Get data regarding station ID 3047
  #' yearly_cycle(3047)
  #'
  #' @export
    yearly_cycle <- function(station_id){
      df <- weather_data %>%
        filter(WBANNO == station_id) %>%
          mutate(day = as.numeric(format(LST_DATE, "%j")))
      d <- df$day
      d2 <- (df$day)^2
      lm <- lm(df$T_DAILY_AVG ~ d + d2)

      d_pred <- sort(unique(df$day))
      d2_pred <- (d_pred)^2
      pred_df <- as.data.frame(cbind(d_pred, d2_pred))
      pred <- lm$coefficients[1]+lm$coefficients[2]*d_pred+
        lm$coefficients[3]*d2_pred
      result <- cbind(temp = pred, day = d_pred)
      return(result)
    }


  #---------------------------------------------------------------------------

  #' A function to estimate weather trends over time
  #'
  #' This function will use a Seasonal Auto regressive Integrated Moving Average.
  #'
  #' @param station_id WBAN ID of the weather station
  #'
  #' @return a dataframe for a specific weather station. It has the following
  #' columns.
  #' \itemize{
  #'     \item \code { @context } metadata
  #'     \item \code {day_cum} Each day of the year (1-365)
  #'     \item \code {expected_temp} Expected daily temperature
  #'}
  #' @examples
  #' #Get data regarding station ID 3047
  #' yearly_cycle(3047)
  #'
  #' @export
