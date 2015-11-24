\name{runBMLGrid}
\alias{runBMLGrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Run a BML grid in steps in R
}
\description{
It can run a BML grid in certain steps.
}
\usage{
runBMLGrid(g, numSteps, slow = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{ BML grid, it is the initial status. 
%%     ~~Describe \code{g} here~~
}
  \item{numSteps}{ Interger, the number of steps to run the cars.
%%     ~~Describe \code{numSteps} here~~
  }
  \item{slow}{ Logical, means whether use the "moveslow"" function, the default value is FALSE, means not use the "moveslow"" function but "move" function
}
}

\value{
It is a LIST, 
  \item{grid }{The final grid after the number of steps}
  \item{vbystep }{The velosity for every steps }
  \item{movestep }{The number of cars to move in every steps}
}

\examples{
g = createBMLGrid(r = 100, c = 99, ncars = c(red = 100,blue = 100))

g.out=runBMLGrid(g, numSteps = 10000)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
