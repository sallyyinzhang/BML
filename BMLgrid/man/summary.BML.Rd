\name{summary.BML}
\alias{summary.BML}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary for BML grid
}
\description{
It will give a summary of initial BLM grid and the status of after run several time steps.
}
\usage{
summary.BML(grid, numSteps, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grid}{ BML grid, the initial grid
%%     ~~Describe \code{grid} here~~
}
  \item{numSteps}{Interger, the steps to run the initial grid
%%     ~~Describe \code{numSteps} here~~
}
  \item{\dots}{ Additional arguments affecting the summary produced
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
It is a LIST,
 \item{initial }{The initial grid}
 \item{final }{The final grid}
 \item{prop }{The proportion of cars in the grid}
 \item{num_red }{The number of red cars}
 \item{num_blue}{The number of blue cars}
 \item{ave_velocity}{The average of velocity in the number of steps}
 \item{car_move }{The average number of moved cars}
 \item{block }{The average of blocked cars}
%% ...
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

summary(g, numSteps = 1000)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
