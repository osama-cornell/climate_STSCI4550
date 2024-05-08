library(dplyr)
library(maps)
library(fields)
library(sf)
library(sp)
library(ggplot2)
library(GpGp)

test_that("grid points interpolated looks ok for daily avg", {
  df <- weather_data[1:10000,]

  grid <- grid_points(0.75)
  info <- station_grid_points(df,grid)
  expect_equal(
    colnames(info),
    c("LONGITUDE", "LATITUDE", "AVERAGE")
  )
  expect_equal(
    dim(info),
    c(1434,3)
  )
})



