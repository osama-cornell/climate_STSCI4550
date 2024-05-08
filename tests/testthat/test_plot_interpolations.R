library(dplyr)
library(maps)
library(fields)
library(sf)
library(sp)
library(ggplot2)
library(GpGp)

test_that("graph of plots interpolated does not give an error", {
  df <- weather_data %>%
    select(LONGITUDE, LATITUDE, T_DAILY_AVG)
  df <- na.omit(df)
  expect_no_error(plot_interpolations(df, col1 = "T_DAILY_AVG"))
})
