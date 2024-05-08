library(dplyr)
library(maps)
library(fields)
library(sf)
library(sp)
library(ggplot2)
library(GpGp)
test_that("yearly cycle for single station looks ok", {
  info <- yearly_cycle(53878)
  expect_equal(
    colnames(info),
    c("temp","day")
  )
  expect_equal(
    dim(info),
    c(365,2)
  )
})
