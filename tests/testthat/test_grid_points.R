library(dplyr)
library(maps)
library(fields)
library(sf)
library(sp)
library(ggplot2)
library(GpGp)
test_that("grid points of default resolution looks ok", {
  info <- grid_points()
  expect_equal(
    colnames(info),
    c("Var1", "Var2")
  )
  expect_equal(
    dim(info),
    c(81513,2)
  )
})

# Not sure what to compare to
test_that("grid points of different resolution looks ok", {
  info2 <- grid_points(0.2)
  expect_equal(
    colnames(info2),
    c("Var1", "Var2")
  )
  expect_equal(
    dim(info2),
    c(20399,2)
  )
})

