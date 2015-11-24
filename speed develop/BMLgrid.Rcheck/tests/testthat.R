library(BMLgrid)

#Test whether the input is in the right form in the "createBMLGrid" function
test_input = function(){
test_that("There is an error", {
  expect_error(createBMLGrid(-2,3,c(2,3)),"Row and column of grid should be bigger than 0!\n")
  expect_error(createBMLGrid(2,3,c(-1,3)),"Number of cars should be bigger than 0!\n")
  expect_error(createBMLGrid(2,3,c(4,3)),"Number of cars should be no bigger than the demension of the grid!\n")
  })
}

#Test whether the cars move in the right position after 1 or 2 steps.
#First test the "runBMLGrid" funtion move right or not, then test whether "crunBMLGrid" have
#the same result
test_move = function ()
{
  cat("Running additional test...\n")
  #first five the initial grid and calcuate the grid that move one time steps and move two timesteps
  grid = matrix(c("", "red", "", "blue", "", "", "", "blue", "", "", "", "red"), nrow = 3)
  grid_1 = matrix(c("", "red", "", "", "", "blue", "blue", "", "", "", "", "red"), nrow = 3) #after one step
  grid_2 = matrix(c("", "", "red", "", "red", "blue", "blue", "", "", "", "", ""), nrow = 3) #after two step
  
  #See whether the move function have the right move in R 
  expect_equal(c(runBMLGrid(grid, 1)[[1]]), c(grid_1)) #R have the right move
  expect_equal(c(runBMLGrid(grid, 2)[[1]]), c(grid_2))
  
  #See whether the crunBMLGrid have the right answer
  expect_equal(c(crunBMLGrid(grid, 1)[[1]]), c(grid_1)) #C have the right move and right calculate of 
  expect_equal(c(crunBMLGrid(grid, 2)[[1]]), c(grid_2)) #C have the right move
}
  
#Test the plot function whether it give the right plot
test_plot = function()
{
  cat("There is no car in the grid \n")
  grid1 = createBMLGrid(2, 3, 0)
  plot(grid1)
  
  cat("There is only blue car in whole the grid \n")
  grid2 = createBMLGrid(2, 3, c(3, 0))
  plot(grid2)
  
  cat("There is only blue car in whole the grid \n")
  grid3 = createBMLGrid(2, 3, c(0, 3))
  plot(grid3)
}

#Test the summary function in the package
test_summary = function()
{
  grid = matrix(c("", "red", "", "blue", "", "", "", "blue", "", "", "", "red"), nrow = 3)
  grid_2 = matrix(c("", "", "red", "", "red", "blue", "blue", "", "", "", "", ""), nrow = 3) #after two step
  class(grid) = c("BML",class(grid))
  #check the initial grid
  expect_equal(c(summary(grid, 2)[[1]]), c(grid))
  
  #check the final grid after 2 steps
  expect_equal(c(summary(grid, 2)[[2]]), c(grid_2))
  
  #check the average velocity
  expect_equal(summary(grid, 2)[[3]], 1/3)
}
  
  
  
  