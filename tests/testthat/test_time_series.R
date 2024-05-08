library(dplyr)
library(maps)
library(fields)
library(sf)
library(sp)
library(ggplot2)
library(GpGp)

test_that("weather data for specific station looks ok", {
  info <- time_series(53878,"2020-09-02","2020-09-03")
  expect_equal(
    info[,4],
    c(as.Date("2020-09-02"),as.Date("2020-09-03"))
  )
  expect_equal(
    dim(info),
    c(2,13)
  )

})
