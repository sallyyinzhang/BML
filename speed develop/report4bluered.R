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


print.summary.BML = function (object, ...)
{
  cat("The initial and final status see the plot:\n")
  par(mfrow=c(1,2))
  plot(object$initial, main = "Initial Status")
  plot(object$final, main = "Final Status")
  cat("\nThe number of red and blue cars:\n ")
  print(c(numred=object$num_red, numblue=object$num_blue))
  cat("\nThe density of cars:\n ")
  print(object$prop)
  cat("\nThe final status:\n")
  print(object$final)
  cat("\nThe average parameter:\n")
  print(c(ave_velocity=object$ave_velocity, car_move=object$car_move, block=object$block))
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
  
#check whether they are same
g=createBMLGrid(sample(x = 100,size = 1),sample(x = 100,size = 1),0.4)
c_result = crunBMLGrid(g,100)
r_result = runBMLGrid(g,100)
any(c_result$grid!=r_result$grid)
any(c_result$vstep!=r_result$vbystep)
any(c_result$movestep!=r_result$movestep)

#whether it is worth to build a function in C to createBMLGrid
g = createBMLGrid(1024,1024,0.4)
Rprof("move.out")
move = move(g, 1)
Rprof(NULL) #turn it off 
summaryRprof("move.out") 

system.time(createBMLGrid(1024,1024,0.7))
Rprof("createBMLGrid.out")
gird = createBMLGrid(1024,1024,0.7)
Rprof(NULL) #turn it off 
summaryRprof("createBMLGrid.out") 


#evaluate the crunBMLGrid function, to see if run the crunBMLGrid, the cost time in each part
g_testc = createBMLGrid(500,500,0.4)
Rprof("crunBMLGrid.out")
result = crunBMLGrid(g_testc, 10000)
Rprof(NULL) #turn it off 
summaryRprof("crunBMLGrid.out")


#run grid 100*100, the number of red cars and the number of blue cars are 2000 seperatly
timestep=seq(from = 0 ,to = 3000,by = 500 )
g=createBMLGrid(100,100,0.4)
timecostc=sapply(timestep, function(x) system.time(crunBMLGrid(g,x))[[3]])
timecostr=sapply(timestep, function(x) system.time(runBMLGrid(g,x))[[3]])
timecostsl=sapply(timestep, function(x) system.time(runBMLGrid(g,x, slow=TRUE))[[3]])
plot(timecostsl~timestep, type = "l", main="Comparision for three methods in timesteps", xlab = "time step/one unit", ylab = "running time/seconds")
lines(timecostc~timestep, col = "red")
lines(timecostr~timestep, col = "blue")
legend("topleft", legend=c("R for loop", "R vectorize", "C method"),col=c("black", "blue", "red"), lty=1, cex=0.5)
plot(timecostc~timestep, col = "red", type = "l", main="The running time for C method", xlab = "time step/one unit", ylab = "running time/seconds")
lm(timecostsl~timestep-1) #-1 means the intercept is 0 when regression, because running time is 0 when the timestep is 0  
lm(timecostc~timestep-1)
lm(timecostr~timestep-1)

#The stationary of the three method
timestep=seq(from = 0 ,to = 500,by = 100 )
g=createBMLGrid(100,100,0.4)
rep_c = replicate(5, sapply(timestep, function(x) system.time(crunBMLGrid(g,x))[[3]])) #3 means I choose elapse time to evaluate the performance
rep_r = replicate(5, sapply(timestep, function(x) system.time(runBMLGrid(g,x))[[3]]))
rep_rf = replicate(5, sapply(timestep, function(x) system.time(runBMLGrid(g,x, slow = TRUE))[[3]]))
timedata = as.data.frame(cbind(timestep = rep(timestep, 5), c = as.vector(rep_c), r = as.vector(rep_r), r_f = as.vector(rep_rf)))#5 is the number of times for replicate

install.packages("ggplot2")
install.packages("Hmisc")
install.packages("reshape")
library(reshape)
library(ggplot2)
library(Hmisc)
timedata= melt(timedata, id ='timestep', variable_name="methods")

ggplot(timedata, aes(x=timestep, value, colour=factor(methods))) +
  stat_summary(fun.data="mean_cl_normal", geom="smooth", size=1) +
  geom_point()+xlab("time step/one unit")+ylab("time/seconds")+ggtitle("Comparation of running time in C and R in different time step")+scale_colour_manual(values=c("c"="red", "r"="blue", "r_f" = "black"), name="methods",breaks=c("c", "r","r_f"),labels=c("C method", "R vecterize", "R for loop"))

##dimention vs time
dim=c(16, 32, 64, 128, 256)
g=lapply(dim, function(x) createBMLGrid(x,x,0.4))
dim_timecost_c=sapply(g, function(x) system.time(crunBMLGrid(x,1000))[[3]])
dim_timecost_r=sapply(g, function(x) system.time(runBMLGrid(x,1000))[[3]])
plot(dim_timecost_r~dim, type = "l", main="The comparision in different dimesion", xlab = "dimension of the grid", ylab = "running time/seconds")
lines(dim_timecost_c~dim, col = "red")
legend("topleft", legend=c("R method", "C method"),col=c('black', 'red'), lty=1, cex=0.5)
plot(dim_timecost_c~dim, col = "red", type = "l", main="C methods", xlab = "dimension of the grid", ylab = "running time/seconds")

##density vs time
dens = seq(0.2,0.7,0.1)
grid_dens=lapply(dens, function(x) createBMLGrid(100,100,x))
dens_timecost_c=sapply(grid_dens, function(x) system.time(crunBMLGrid(x,1000))[[3]])
dens_timecost_r=sapply(grid_dens, function(x) system.time(runBMLGrid(x,1000))[[3]])
plot(dens_timecost_r~dens, type = "l", main="The comparision in different density", xlab = "density of cars", ylab = "running time/seconds",ylim=c(0,3))
lines(dens_timecost_c~dens, col = "red")
legend("topleft", legend=c("R method", "C method"),col=c('black', 'red'), lty=1, cex=0.5)
plot(dens_timecost_c~dens, col = "red", type = "l", main="C methods", xlab = "density of cars", ylab = "running time/seconds")



