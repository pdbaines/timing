
#
# timing class
# 

#' Convert a vector in seconds, minutes or hours into a 'timing' object
#' 
#' Convert a vector containing values in seconds, minutes or hours
#' (including fractions) and convert into a \code{\link{timing}} object
#' that represents hrs/min/sec and allows basic operations such as 
#' adding times, computing minimum/maximum times, summing times etc.
#' 
#' @param x vector of times in either hours, minutes or seconds
#' @param from.units Either "hrs", "min" or "sec"
#' @value An object of class 'timing'
#' @export
#' @seealso \code{\link{timing}}
#' @keywords timing
#' @examples
#' # Not run
#' x <- seq(-1,2,by=0.5)
#' y <- convert(x,from.units="hrs")
#  x
#' y
#' summary(y)
"convert" <- function(x,from.units)
{
  if (missing(x)){
    stop("'x' must be specified when using 'convert'")
  }
  if (missing(from.units)){
    stop("'from.units' must be specified when using 'convert'")
  }
  if (!is.vector(x) || !is.numeric(x)){
    stop("'x' must be a numeric vector in 'convert'")
  }
  if (length(from.units) != 1){
    stop("'from.units' must be a scalar (length 1) argument in 'convert'")
  }
  ret <- rep(NA,length(x))
  signs <- sign(x)
  x <- abs(x)
  if (from.units == "sec"){
    # Convert to hrs/min/sec
    hrs <- floor(x/3600)
    floor_1 <- hrs*3600
    left_1 <- x-floor_1
    mins <- floor(left_1/60)
    floor_2 <- floor_1 + mins*60
    left_2 <- x-floor_2
    sec <- left_2
    raw <- x
  } else if (from.units == "min"){
    # Convert to hrs/min/sec
    hrs <- floor(x/60)
    floor_1 <- hrs*60
    left_1 <- x-floor_1
    mins <- floor(left_1)
    floor_2 <- floor_1 + mins
    left_2 <- (x-floor_2)*60
    sec <- left_2
    raw <- x*60
  } else if (from.units == "hrs"){
    # Convert to hrs/min/sec    
    hrs <- floor(x)
    floor_1 <- hrs
    left_1 <- x-floor_1
    mins <- floor(left_1*60)
    floor_2 <- floor_1 + (mins/60)
    left_2 <- (x-floor_2)*3600
    sec <- left_2
    raw <- x*3600
  }
  raw <- raw*signs
  ret <- list("sign"=signs,"hrs"=hrs,"min"=mins,"sec"=sec,"raw"=raw)
  class(ret) <- "timing"
  return(ret)
}

#' @export
"show.timing" <- function(x)
{
  if (class(x)!="timing"){
    stop("'x' is not of class 'timing'")
  }
  sign_vec <- ifelse(x$sign<0,"-","")
  ret <- paste(sign_vec,x$hrs,":",sprintf("%02d",x$min),":",sprintf("%02d",x$sec),sep="")
  return(ret)
}

#' @export
"print.timing" <- function(x)
{
  if (class(x)!="timing"){
    stop("'x' is not of class 'timing'")
  }
  # Just print as string for now...
  #print(as.character(show(x)))
}

#' @export
"summary.timing" <- function(x)
{
  return(x)
}

#' @export
"min.timing" <- function(x,na.rm=FALSE){
  return(convert(min(x$raw,na.rm=na.rm),from.units="sec"))
}

#' @export
"max.timing" <- function(x,na.rm=FALSE){
  return(convert(max(x$raw,na.rm=na.rm),from.units="sec"))
}

#' @export
"mean.timing" <- function(x,...){
  return(convert(mean(x$raw,...),from.units="sec"))
}

#' @export
"median.timing" <- function(x,...){
  return(convert(median(x$raw,...),from.units="sec"))  
}

#' @export
"quantile.timing" <- function(x,...){
  return(convert(quantile(x$raw,...),from.units="sec"))
}

#' @export
"range.timing" <- function(x,...){
  return(convert(range(x$raw,...),from.units="sec"))  
}

#' @export
"+.timing" <- function(x,y){
  return(convert(x$raw+y$raw,from.units="sec"))
}

#' @export
"-.timing" <- function(x,y){
  return(convert(x$raw-y$raw,from.units="sec"))
}

#' @export
"sum.timing" <- function(x,...){
  return(convert(sum(x$raw,...),from.units="sec"))    
}
