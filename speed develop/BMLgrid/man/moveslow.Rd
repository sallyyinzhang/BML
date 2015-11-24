\name{moveslow}
\alias{moveslow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Move a grid in a slow way
}
\description{
moveslow gives a result of a grid which move one step. The odd step should be move blue cars and the even step should be move red cars
}
\usage{
moveslow(grid, time)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grid}{ BML grid, the initial grid should be move 
%%     ~~Describe \code{grid} here~~
}
  \item{time}{ interger, the time step to move the grid
%%     ~~Describe \code{time} here~~
}
}

\value{
The result is a LIST, 
 \item{grid }{The final grid after one step}
 \item{v }{the velosity, moved cars/number of corresponding cars}
 \item{carmove }{number of moved car}
%% ...
}

\examples{
g = createBMLGrid(r = 100, c = 99, ncars = c(red = 100,blue = 100))
moveslow(g, 5) # move g at 5 time step
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
