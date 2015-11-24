\name{runBMLGrid}
\alias{runBMLGrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Run a BML grid steps
}
\description{
It can run a BML grid in certain steps.
}
\usage{
runBMLGrid(g, numSteps)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{ BML grid, it is the initial status. 
%%     ~~Describe \code{g} here~~
}
  \item{numSteps}{ interger, the number of steps to run the cars.
%%     ~~Describe \code{numSteps} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
It is a LIST, use
  \item{grid }{The final grid after the number of steps}
  \item{vbystep }{The velosity for every steps }
  \item{movestep }{The number of cars to move in every steps}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
g = createBMLGrid(r = 100, c = 99, ncars = c(red = 100,blue = 100))

g.out=runBMLGrid(g, numSteps = 10000)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
