library(dplyr)
library(maps)
library(fields)
library(sf)
library(sp)
library(ggplot2)
library(GpGp)
library(ggplot2)
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
  #' Uses a second order regression model.
  #'
  #' @param station_id WBAN ID of the weather station
  #'
  #' @return a dataframe for a specific weather station. It has the following
  #' columns.
  #' \itemize{
  #'     \item \code { @context } metadata
  #'     \item \code {day} Each day of the year (1-365)
  #'     \item \code {temp} Expected daily temperature
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
      cos12 <- cos(2*pi*d/365.25)
      sin12 <- sin(2*pi*d/365.25)
      lm <- lm(df$T_DAILY_AVG ~ d + d2 + cos12 + sin12)

      d_pred <- 1:365
      d2_pred <- (d_pred)^2
      cos12_pred <- cos(2*pi*d_pred/365.25)
      sin12_pred <- sin(2*pi*d_pred/365.25)
      pred_df <- as.data.frame(cbind(d_pred, d2_pred,cos12_pred,sin12_pred))
      pred <- lm$coefficients[1]+lm$coefficients[2]*d_pred+
        lm$coefficients[3]*d2_pred + lm$coefficients[4]*cos12_pred +
        lm$coefficients[5]*sin12_pred
      result <- cbind(temp = pred, day = d_pred)
      return(as.data.frame(result))
    }


  #---------------------------------------------------------------------------

  #' A function to estimate look at temperature trend over time per year.
  #'
  #' Using a simple linear model to estimate trend.
  #'
  #' @param station_id WBAN ID for a specific station
  #' @param type The temperature type
  #'
  #' @return a vector containing the trend estimate, p-value, and standard error.
  #'
  #' @examples
  #' #Get data regarding station ID 3047
  #' yearly_trend(3047,"T_DAILY_AVG")
  #'
  #' @export
    yearly_trend <- function(station_id,type = "T_DAILY_AVG"){
      df <- weather_data %>%
        filter(WBANNO == station_id)
      d <- as.numeric(df$LST_DATE)
      cos12 <- cos(2*pi*d/365.25)
      sin12 <- sin(2*pi*d/365.25)
      cos6 <- cos(2*pi*d/182.625)
      sin6 <- sin(2*pi*d/182.625)
      cos4 <- cos(2*pi*d/121.75)
      sin4 <- sin(2*pi*d/121.75)
      temp <- df[[type]]
      lm <- lm(temp ~ d + cos12 + sin12 + cos6 + sin6 + cos4 + sin4)
      return(c(slope = lm$coefficients[2],p_value =
                 summary(lm)$coefficients[2,4],
             se = summary(lm)$coefficients[2,2]))
    }

  #---------------------------------------------------------------------------

  #' Insert Here
  #'
  #' Insert Here
  #'
  #' @param resolution Resolution of graphic, default is 0.1
  #'
  #' @return Insert Here
  #'
  #' @examples
  #' Insert Here
  #'
  #' @export
  grid_points <- function(resolution = 0.1) {
    map <- map_data("usa")
    map <- filter(map, region == "main")
    x.points <- seq(min(map$long), max(map$long), by = resolution)
    y.points <- seq(min(map$lat), max(map$lat), by = resolution)
    grid <- expand.grid(x.points,y.points)

    inside <- point.in.polygon(grid$Var1, grid$Var2, map$long, map$lat)
    inside.logical <- ifelse(inside, TRUE, FALSE)
    return(grid[inside.logical, ])
  }

  #---------------------------------------------------------------------------

  #' Insert Here
  #'
  #' Insert Here
  #'
  #' @param name Insert Here
  #'
  #' @return Insert Here
  #'
  #' @examples
  #' Insert Here
  #'
  #' @export
  station_grid_points <- function(df, grid_points, param = "T_DAILY_AVG") {
    full <- df %>%
      select(param, LONGITUDE, LATITUDE)
    clean_df <- na.omit(full)
    df <- as.data.frame(clean_df)


    model <- fit_model(y = df[[param]],locs = df[,c("LONGITUDE", "LATITUDE")],
                       covfun_name = "matern_sphere",silent = TRUE,
                       max_iter = 25)

    X <- as.data.frame(rep(1,dim(grid_points)[1]))
    preds <- predictions(fit = model,locs_pred = grid_points, X_pred = X)
    names(grid_points) <- c("LONGITUDE","LATITUDE")
    result <- cbind(grid_points,AVERAGE = preds)
    return(result)
  }

  #---------------------------------------------------------------------------

  #' Insert Here
  #'
  #' Insert Here
  #'
  #' @param name Insert Here
  #'
  #' @return Insert Here
  #'
  #' @examples
  #' Insert Here
  #'
  #' @export
    plot_interpolations <- function(df){
      ggplot(df,aes(x = LONGITUDE, y = LATITUDE, color = AVERAGE)) +
        geom_point() +
        scale_color_gradient(low = "lightblue", high = "darkred") +
        labs(x = "Longitude", y = "Latitude",
             color = "Average Daily Temperature (Celsius)")
    }

