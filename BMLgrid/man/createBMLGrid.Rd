\name{createBMLGrid}
\alias{createBMLGrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a BMLGrid
}
\description{
Creat a grid with blue cars and red cars
}
\usage{
createBMLGrid(r, c, ncars, prop = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r}{ Integer. The number of row of the grid
%%     ~~Describe \code{r} here~~
}
  \item{c}{ Integer. The number of column of the grid
%%     ~~Describe \code{c} here~~
}
  \item{ncars}{ Two dimentional vector or numeric. The number of cars, it should be like c(red = 10, blue = 10), or a proportion of the cars in the grid which should be between 0 and 1
%%     ~~Describe \code{ncars} here~~
}
  \item{prop}{ Numeric. The proportion of red:blue cars. The default value is 0.5
%%     ~~Describe \code{prop} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
It returns a grid with red and blue cars.
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
