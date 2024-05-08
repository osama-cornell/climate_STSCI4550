library(dplyr)
library(maps)
library(fields)
library(sf)
library(sp)
library(ggplot2)
library(GpGp)
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
  ts_data <- dplyr::filter(weather_data, WBANNO == station_id)
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
#' @return a dataframe with the following columns:
#' \describe{
#'     \item{day}{Day of the year (1-365)}
#'     \item{temp}{Estimate temperature for the day}
#'     }
#' @examples
#' #Get data regarding station ID 3047
#' yearly_cycle(3047)
#'
#' @export
yearly_cycle <- function(station_id){
  #Get day of year
  df <- weather_data %>%
    filter(WBANNO == station_id) %>%
    mutate(day = as.numeric(format(LST_DATE, "%j")))
  #Fit regression model
  d <- df$day
  d2 <- (df$day)^2
  cos12 <- cos(2*pi*d/365.25)
  sin12 <- sin(2*pi*d/365.25)
  lm <- lm(df$T_DAILY_AVG ~ d + d2 + cos12 + sin12)

  #Make predictions
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
#' yearly_trend(3047)
#'
#' @export
yearly_trend <- function(station_id,type = "T_DAILY_AVG"){
  df <- weather_data %>%
    filter(WBANNO == station_id)
  #Fit regression model
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

#' Get Grid of Continuous USA
#'
#' This function makes a map of the USA using grid points. All grid points
#' fall within the continuous USA
#'
#' @param resolution Resolution of graphic, default is 0.1
#'
#' @return dataframe with the following columns:
#' \describe{
#'     \item{Var1}{Longitude}
#'     \item{Var2}{Latitude}
#'     }
#'
#' @examples
#' #Make grid points with default resolution
#' grid_points()
#'
#' @export
grid_points <- function(resolution = 0.1) {
  #Get map of continuous
  map <- ggplot2::map_data("usa")
  map <- filter(map, region == "main")
  #Make points depending on resolution
  x.points <- seq(min(map$long), max(map$long), by = resolution)
  y.points <- seq(min(map$lat), max(map$lat), by = resolution)
  #Create grid
  grid <- expand.grid(x.points,y.points)
  #Make it inside polygon
  inside <- point.in.polygon(grid$Var1, grid$Var2, map$long, map$lat)
  inside.logical <- ifelse(inside, TRUE, FALSE)
  return(grid[inside.logical, ])
}

#---------------------------------------------------------------------------

#' Interpolates values given grid points
#'
#' This function is predicts a target variable, given grid points using a
#' spatial model
#'
#' @param df dataframe with the following components
#' \describe{
#'     \item{LONGITUDE}{Longitude for training the model}
#'     \item{LATITUDE}{Latitude for training the model}
#'     \item{.}{A target variable of choice}
#'     }
#' @param grid_points dataframe with the following components:
#' \describe{
#'     \item{.}{Longitude for predictions}
#'     \item{.}{Latitude for predictions}
#'     }
#' @param param Optional parameter for the target variable
#'
#' @return a dataframe with the following components:
#' \describe{
#'     \item{LONGITUDE}{Longitude from grid_points}
#'     \item{LATITUDE}{Latitude from grid_points}
#'     \item{AVERAGE}{Predictions from the model}
#'     }
#'
#' @examples
#' #Using default target
#' df <- weather_data[1:10000,]
#' grid <- grid_points(0.75)
#' station_grid_points(df,grid)
#'
#' @export
station_grid_points <- function(df, grid_points, param = "T_DAILY_AVG") {
  #Select specific columns and clean data
  full <- df %>%
    select(param, LONGITUDE, LATITUDE)
  clean_df <- na.omit(full)
  df <- as.data.frame(clean_df)
  colnames(grid_points) <- c("LONGITUDE", "LATITUDE")

  #Fit spatial model
  model <- fit_model(y = df[[param]],locs = as.matrix(df[,c("LONGITUDE", "LATITUDE")]),
                     covfun_name = "matern_sphere",silent = TRUE,
                     max_iter = 25)
  #Make predictions
  X <- as.matrix(rep(1,dim(grid_points)[1]))
  preds <- predictions(fit = model,locs_pred = as.matrix(grid_points), X_pred = X)
  names(grid_points) <- c("LONGITUDE","LATITUDE")
  result <- cbind(grid_points,AVERAGE = preds)
  return(result)
}

#---------------------------------------------------------------------------

#' Plot interpolation
#'
#' This function uses ggplot2 to make plots of spatial data
#'
#' @param df dataframe with following components
#' \describe{
#'     \item{LONGITUDE}{Longitude from grid_points}
#'     \item{LATITUDE}{Latitude from grid_points}
#'     \item{.}{Variable for color}
#'     }
#' @param col1 Variable for color
#' @param type type of plot needed. 1 is for continuous color variables,
#'  and 2 is for continuous variables the are very small. (Optional)
#' @param col2 Variable for size (Optional)
#' @param Title Title of plot
#' @param size_name Title size on the legend
#'
#' @return A plot of spatial data
#'
#' @examples
#' plot_interpolations(df = preds, col1 = preds$AVERAGE)
#'
#' @export
plot_interpolations <- function(df,col1,type = 1,col2 = NULL,
                                Title = NULL, size_name = NULL){
  #Plot of continuous variable
  if(type == 1){
    ggplot(df,aes(x = LONGITUDE, y = LATITUDE, color = col1)) +
      geom_point() +
      scale_color_gradient(low = "lightblue", high = "darkred") +
      labs(x = "Longitude", y = "Latitude",
           color = Title)
  }

  #Plot of granular continuous variable (very small values)
  else if(type == 2){
    ggplot(df,aes(x = LONGITUDE, y = LATITUDE, color = col1, size = col2,)) +
      geom_point() +
      scale_color_gradientn(colors = rainbow(10)) +
      labs(x = "Longitude", y = "Latitude",
           color = Title, size = size_name)
  }
}
