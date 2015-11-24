# This is a function which is create grid, input:dim the grid dimention and carprop is the car proportion , 
#the prop is the red:blue, if it is c(a,b), a is number of red car and b is num of blue car
#output: the grid which you create
createBMLGrid = function (r, c , ncars, prop = 0.5 ) 
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

#The slow version for the move function which can move one step, time is the timeth step to move 
#output is a list: The first one is grid which is the new grid that move one step,
#the second one is v which is velocity, the third one is carmove which means the numeber of car move
moveslow = function (grid, time)
{
  # find the position of cars
  position=which(grid!="", arr.ind = TRUE)
  car=data.frame(position,color=grid[position])
  
  if (time%%2)
  { nextrow=c()
    colormove = "blue"
    curposition=as.matrix(subset(car[,-3], car$color== colormove ))
    for (i in 1 : nrow(curposition))
    {nextrow[i]=curposition[i,1]-1L
     if (nextrow[i] < 1)
       nextrow[i]=nrow(grid)
    }
    nextposition=cbind(nextrow,curposition[,2])
  }else
  { nextcol=c()
    colormove = "red"
    curposition=as.matrix(subset(car[,-3], car$color== colormove ))
    for (i in 1 : nrow(curposition))
    {nextcol[i]=curposition[i,2]+1L
     if (nextcol[i] > ncol(grid))
       nextcol[i]=1L
    }
    nextposition=cbind(r=curposition[,1],c=nextcol)
  }
  
  #see whether can move--whether the next is empty
  find=(grid[nextposition] == "")
  movenum=sum(find) 
  grid[nextposition[find, , drop = FALSE]] = colormove
  grid[curposition[find, , drop = FALSE]] = ""
  
  list(grid=grid,v=movenum/nrow(curposition),carmove=movenum)
}

#This is the victorlize version for move function which can 
#output: the same as the slowest version
move = function (grid, time) 
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

plot.BML = function (x, main="BML plot", ...)
{
  g=t(x)[,nrow(x):1] #because of the image funtion, in order to make the matric and the image same
  matchcol = matrix(match(g, c("", "blue", "red")), nrow(g))
  
  if(length(which(x==""))==0) {color=c("blue", "red")
  }else if(!"red" %in% x) {
    color=c("white", "blue")
  }else if(!"blue" %in% x){
    color = c("white","red")
  }else{
    color= c("white", "blue", "red")
  }
  
  image(matchcol, col = color,axes = FALSE, main = main, xlab = "", ylab = "" , ... )
  box()
}

summary.BML = function (object, numSteps, ...)
{
  initial=object
  num_red=sum(object == "red")
  num_blue=sum(object == "blue")
  prop=(num_red + num_blue) / length(object)
  result=runBMLGrid(object, numSteps)
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

#parameter: "g" stands for the initial grid, and "numSteps" stands for number of steps, "slow" means if 
#run the slow version of move function. The output is a list, the first one grid is the grid after
#"numSteps". The second one is "vbystep" which is a vector and means the velocity of each steps. The third
#one is "movestep" which is a vector and means the number of cars moves in every step
runBMLGrid = function ( g, numSteps, slow = FALSE)
{ vstep=numeric(numSteps) #velocity in every step
  movestep=integer(numSteps) #number of cars move
  for (i in 1:numSteps)
  { if (slow == TRUE) result=moveslow(g,i)
    else result=move(g,i)
    g=result$grid
    vstep[i]=result$v #the blue car velocity
    movestep[i]=result$carmove
  }
  list(grid=g,vbystep=vstep, movestep=movestep)
}

#"crunBMLGrid" is a function which can call the function in C. The input and output is the same
# as the runBMLGrid except there is no "slow" parameter in the input
crunBMLGrid = function (grid, numSteps)
{ #get the input parameter in C functions
  nrow=nrow(grid)
  ncol=ncol(grid)
  grid=match(grid, c("", "blue", "red"))-1 #change the charactor to be the numeric, because in C, the 1 is represent for blue, and 2 is represent for red, 0 is represent for no car, so there is minus 1
  nblue=sum(grid == 1)
  nred=sum(grid == 2)
  cresult=.C("crunBMLGrid", grid = as.integer(grid), nrow=as.integer(nrow), ncol=as.integer(ncol),nblue=as.integer(nblue),nred = as.integer(nred),numSteps = as.integer(numSteps), vstep = as.double(rep(0,numSteps)), carmovestep = as.integer(rep(0,numSteps)))
  cgrid = matrix(c('','blue','red')[cresult$grid+1], nrow=nrow) # change the numeric to be charactor again, 0,1,2 to be "","blue","red" respectively, "+1" means change the 0,1,2 to be 1,2,3 in order to give the charactor
  class(cgrid)=c("BML",class(cgrid))
  list(grid=cgrid, vstep=cresult$vstep,movestep=cresult$carmovestep)
}