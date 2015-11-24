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
plot.BML(x, main = "BML plot", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ The BML grid to plot.
%%     ~~Describe \code{grid} here~~
}
  \item{main}{ The name of the plot. The default value is "BML plot"
%%     ~~Describe \code{main} here~~
}
  \item{\dots}{ Arguments to other features of the plot
%%     ~~Describe \code{\dots} here~~
}
}

\value{
it is a plot
}

\examples{
g = createBMLGrid(r = 100, c = 99, ncars = c(red = 100,blue = 100))

plot(g)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
