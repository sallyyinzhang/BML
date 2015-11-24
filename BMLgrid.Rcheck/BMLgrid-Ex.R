pkgname <- "BMLgrid"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('BMLgrid')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("BMLgrid-package")
### * BMLgrid-package

flush(stderr()); flush(stdout())

### Name: BMLgrid-package
### Title: Bihman-Middleton-Levin Traffic Model
### Aliases: BMLgrid-package BMLgrid
### Keywords: package

### ** Examples

g = createBMLGrid(r = 100, c = 99, ncars = c(red = 100,blue = 100))

g.out=runBMLGrid(g, numSteps = 10000)



cleanEx()
nameEx("createBMLGrid")
### * createBMLGrid

flush(stderr()); flush(stdout())

### Name: createBMLGrid
### Title: Create a BMLGrid
### Aliases: createBMLGrid
### Keywords: ~kwd1 ~kwd2

### ** Examples

g = createBMLGrid(r = 100, c = 99, ncars = c(red = 100,blue = 100))



cleanEx()
nameEx("move")
### * move

flush(stderr()); flush(stdout())

### Name: move
### Title: Move a grid at a exact time step
### Aliases: move
### Keywords: ~kwd1 ~kwd2

### ** Examples

g = createBMLGrid(r = 100, c = 99, ncars = c(red = 100,blue = 100))

move(g, 5) # move g at 5 time step



cleanEx()
nameEx("plot.BML")
### * plot.BML

flush(stderr()); flush(stdout())

### Name: plot.BML
### Title: Plot a BML grid
### Aliases: plot.BML
### Keywords: ~kwd1 ~kwd2

### ** Examples

g = createBMLGrid(r = 100, c = 99, ncars = c(red = 100,blue = 100))

plot(g)



cleanEx()
nameEx("print.summary.BML")
### * print.summary.BML

flush(stderr()); flush(stdout())

### Name: print.summary.BML
### Title: Print the summary for BML grid
### Aliases: print.summary.BML
### Keywords: ~kwd1 ~kwd2

### ** Examples

g = createBMLGrid(r = 100, c = 99, ncars = c(red = 100,blue = 100))

summary(g, numSteps = 1000)



cleanEx()
nameEx("runBMLGrid")
### * runBMLGrid

flush(stderr()); flush(stdout())

### Name: runBMLGrid
### Title: Run a BML grid steps
### Aliases: runBMLGrid
### Keywords: ~kwd1 ~kwd2

### ** Examples

g = createBMLGrid(r = 100, c = 99, ncars = c(red = 100,blue = 100))

g.out=runBMLGrid(g, numSteps = 10000)



cleanEx()
nameEx("summary.BML")
### * summary.BML

flush(stderr()); flush(stdout())

### Name: summary.BML
### Title: Summary for BML grid
### Aliases: summary.BML
### Keywords: ~kwd1 ~kwd2

### ** Examples

g = createBMLGrid(r = 100, c = 99, ncars = c(red = 100,blue = 100))

summary(g, numSteps = 1000)



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
