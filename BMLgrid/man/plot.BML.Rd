\name{plot.BML}
\alias{plot.BML}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot a BML grid
}
\description{
It will plot a BML grid.
}
\usage{
plot.BML(grid, main = "BML plot", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grid}{ The BML grid to plot.
%%     ~~Describe \code{grid} here~~
}
  \item{main}{ The name of the plot. The default value is "BML plot"
%%     ~~Describe \code{main} here~~
}
  \item{\dots}{ Arguments to other features of the plot
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
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

plot(g)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
