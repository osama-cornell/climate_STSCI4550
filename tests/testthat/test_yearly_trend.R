library(dplyr)
library(maps)
library(fields)
library(sf)
library(sp)
library(ggplot2)
library(GpGp)

test_that("yearly trend of type daily max for single station looks ok", {
  info <- yearly_trend(3047)
  expect_equal(
    names(info),
    c("slope.d","p_value","se")
  )
  expect_equal(
    length(info),
    3
  )
})

test_that("yearly trend of type daily avg for single station looks ok", {
  info2 <- yearly_trend(53878)
  expect_equal(
    names(info2),
    c("slope.d","p_value","se")
  )
  expect_equal(
    length(info2),
    3
  )
})
