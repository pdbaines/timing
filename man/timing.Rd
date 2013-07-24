\name{timing}
\alias{timing}
\title{Create an object of class \code{\link{timing}}, representing time measurements.}
\usage{
  timing(x, time.units)
}
\arguments{
  \item{x}{either a vector of times in units specified by
  time.units, or, an object of class \code{\link{hours}},
  \code{\link{minutes}} or \code{\link{seconds}}}

  \item{time.units}{if x is a numeric vector, then one of
  either "hours", "minutes" or "seconds"}
}
\description{
  Create an object of class \code{\link{timing}} containing
  values in seconds, minutes and hour from either an object
  of classes 'seconds', 'minutes', 'hours', or from a
  numeric vector representing time measurements in units
  specified by 'time.units'. The \code{\link{timing}}
  represents hrs/min/sec and allows basic operations such
  as adding times, computing minimum/maximum times, summing
  times etc.
}
\examples{
# Not run
x <- seq(-1,2,by=0.5)
y <- timing(x,time.units="hrs")
y
}
\seealso{
  \code{\link{timing}}
}
\keyword{timing}

