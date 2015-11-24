createBMLGrid = function (r, c , ncars, prop = 0.5 ) # dim the grid dimention and carprop is the car proportion , the prop is the red:blue, if it is c(a,b), a is number of red car and b is num of blue car
{
  if (r<=0||c<=0)
    stop("Row and column of grid should be bigger than 0!\n")
  if (any(ncars < 0))
    stop("Number of cars should be bigger than 0!\n")
  if (sum(ncars)>r*c)
    stop("Number of cars should be no bigger than the demension of the grid!\n")
  if (sum(ncars)==r*c)
    warning("There are full of cars in the grid!\n")
  if ( length(ncars) == 1 && ncars < 1 ) #if input is proportion of the cars
    ncars = round(rep(r*c*ncars*prop, 2)) # one for the blue cars number and the other for the red car number
  grid = matrix("", r, c) 
  pos = sample(1:(r*c),sum(ncars))
  grid[pos] = sample(rep(c("red", "blue"), c(ncars[1],ncars[2])))
  class(grid) = c("BML",class(grid))
  grid
}

move = function (grid, time) #v=velocity
{
  #move cars
  if(time%%2) #in order to decrease the determine times, put the if out of for loop
  { colormove = "blue"
    curposition=which(grid == "blue", arr.ind = TRUE)
    nexposition=cbind(r = ifelse (curposition[,"row"] == 1L, nrow(grid), curposition[,"row"] - 1L), c = curposition[,"col"]) # blue car move upwards  ###need {}?????????????????
  }else
  { colormove = "red"
    curposition=which(grid == "red", arr.ind = TRUE)
    nexposition=cbind(r = curposition[,"row"], c = ifelse (curposition[,"col"] == ncol(grid), 1L, curposition[,"col"]+ 1L))
  } # red car move rightwards
  
  #check the position
  find=(grid[nexposition] == "")
  movenum=sum(find) # the number of cars moves
  grid[nexposition[find, , drop = FALSE]] = colormove
  grid[curposition[find, , drop = FALSE]] = ""
  
  list(grid=grid, v=movenum/nrow(curposition), carmove=movenum) # should I calculate the carove now??????????? 
}

plot.BML = function (grid, main="BML plot", ...)
{
  g=t(grid)[,nrow(grid):1] #because of the image funtion, in order to make the matric and the image same
  matchcol = matrix(match(g, c("", "blue", "red")), nrow(g))
  
  if(length(which(grid==""))==0) {color=c("blue", "red")
  }else if(!"red" %in% grid) {
    color=c("white", "blue")
  }else if(!"blue" %in% grid){
    color = c("white","red")
  }else{
    color= c("white", "blue", "red")
  }
  
  image(matchcol, col = color,axes = FALSE, main = main, xlab = "", ylab = "" , ... )
  box()
}

summary.BML = function (grid, numSteps, ...)
{
  initial=grid
  num_red=sum(grid == "red")
  num_blue=sum(grid == "blue")
  prop=(num_red + num_blue) / length(grid)
  result=runBMLGrid(grid, numSteps)
  final=result$grid
  ave_velocity=mean(result$vbystep)
  car_move=mean(result$movestep)  #########the last step or all the step??????
  if (numSteps%%2)
  {block=num_blue-car_move
  }else
  {block=num_red-car_move}
  F_result=list(initial=initial,final=final,prop=prop,num_red=num_red,num_blue=num_blue,ave_velocity=ave_velocity,car_move=car_move,block=block)
  class(F_result) = "summary.BML"
  F_result
}


print.summary.BML = function (x, ...)
{
  cat("The initial and final status see the plot:\n")
  par(mfrow=c(1,2))
  plot(x$initial, main = "Initial Status")
  plot(x$final, main = "Final Status")
  cat("\nThe number of red and blue cars:\n ")
  print(c(numred=x$num_red, numblue=x$num_blue))
  cat("\nThe density of cars:\n ")
  print(x$prop)
  cat("\nThe final status:\n")
  print(x$final)
  cat("\nThe average parameter:\n")
  print(c(ave_velocity=x$ave_velocity, car_move=x$car_move, block=x$block))
}

runBMLGrid = function ( g, numSteps)
{ vstep=numeric(numSteps) #velocity in every step
  movestep=integer(numSteps) #number of cars move
  for (i in 1:numSteps)
  {result=move(g,i)
   g=result$grid
   vstep[i]=result$v #the blue car velocity
   movestep[i]=result$carmove
  }
  list(grid=g,vbystep=vstep, movestep=movestep)
}