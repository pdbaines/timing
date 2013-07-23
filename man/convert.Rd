\name{convert}
\alias{convert}
\title{Convert a vector in seconds, minutes or hours into a 'timing' object}
\usage{
  convert(x, units)
}
\arguments{
  \item{x}{vector of times in either hours, minutes or
  seconds}

  \item{units}{Either "hrs", "min" or "sec"}
}
\description{
  Convert a vector containing values in seconds, minutes or
  hours (including fractions) and convert into a
  \code{\link{timing}} object that represents hrs/min/sec
  and allows basic operations such as adding times,
  computing minimum/maximum times, summing times etc.
}
\examples{
# Not run
x <- seq(-1,2,by=0.5)
y <- convert(x,units="hrs")
y
summary(y)
}
\seealso{
  \code{\link{timing}}
}
\keyword{timing}

