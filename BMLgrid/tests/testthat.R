library(testthat)
library(BMLgrid)

test_check("BMLgrid")
context("Errors")
test_that("There is an error", {
  expect_error(createBMLGrid(-2,3,c(2,3)),"Row and column of grid should be bigger than 0!\n")
  expect_error(createBMLGrid(2,3,c(-1,3)),"Number of cars should be bigger than 0!\n")
  expect_error(createBMLGrid(2,3,c(4,3)),"Number of cars should be no bigger than the demension of the grid!\n")
  })