\name{BMLgrid-package}
\alias{BMLgrid-package}
\alias{BMLgrid}
\docType{package}
\title{
Bihman-Middleton-Levin Traffic Model
}
\description{
This is a package for create and simulate Biham-Middleton-Levine(BML) Traffic Model.
}
\details{
\tabular{ll}{
Package: \tab BMLgrid\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-04-30\cr
License: \tab GPL-2\cr
}
There are two important function, creatBMLGrid() to creat a grid and runBMLGrid() to run a grid several steps
}
\author{
Yin Zhang

Maintainer: Yin Zhang <yinzh@ucdavis.edu>
}
\references{

}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R ~~
~~ documentation directory ~~
\keyword{ package }
\seealso{
}
\examples{
g = createBMLGrid(r = 100, c = 99, ncars = c(red = 100,blue = 100))

g.out=runBMLGrid(g, numSteps = 10000)
}
