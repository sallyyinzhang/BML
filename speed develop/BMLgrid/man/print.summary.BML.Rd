\name{print.summary.BML}
\alias{print.summary.BML}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print the summary for BML grid
}
\description{
It will give a summary of initial BLM grid and the status of after run several time steps.
}
\usage{
print.summary.BML(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The result of summary.grid
%%     ~~Describe \code{x} here~~
}
  \item{\dots}{Additional arguments affecting the summary produced.
%%     ~~Describe \code{\dots} here~~
}
}

\value{
It will give a plot of intial grid and final grid and a LIST, 
}

\examples{
g = createBMLGrid(r = 100, c = 99, ncars = c(red = 100,blue = 100))

summary(g, numSteps = 1000)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
