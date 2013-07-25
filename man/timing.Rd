\name{timing}
\alias{timing}
\title{Class for representing measurements of time (durations).}
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
\value{
  An object of class 'timing'
}
\description{
  The function \code{\link{timing}} creates an object of
  class \code{\link{timing}} representing measurements of
  time with values in seconds, minutes and hours. The
  object can be constructed from either an object of
  classes 'seconds', 'minutes', 'hours', or from a numeric
  vector representing time measurements in units specified
  by 'time.units'. The \code{\link{timing}} class
  represents hrs/min/sec and allows basic operations such
  as adding times, computing minimum/maximum times, summing
  times etc.
}
\examples{
# Not run
x <- seq(-1,2,by=0.5)
y <- timing(x,time.units="hrs")
y
median(y)
summary(y)
}
\seealso{
  \code{\link{timing}}
}
\keyword{timing}

