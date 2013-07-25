#' @docType package

#' @export "check_timing"
"check_timing" <- function(object) {
  if (FALSE){
    cat("hrs:\n") ; print(object@hrs)
    cat("mins:\n") ; print(object@mins)
    cat("secs:\n") ; print(object@secs)
    cat("sign:\n") ; print(object@sign)
    cat("raw:\n") ; print(object@raw)
  }
  errors <- character()
  len_h <- length(object@hrs@hrs)
  len_m <- length(object@mins@mins)
  len_s <- length(object@secs@secs)
  len_g <- length(object@sign)
  len_r <- length(object@raw)
  if (length(unique(c(len_h,len_m,len_s,len_g,len_r)))!=1){
    msg <- paste("Lengths of 'hrs', 'mins', 'secs', 'sign' and 'raw' must be equal (",
                 len_h,",",len_m,",",len_s,",",len_g,",",len_r,")",sep="")
    errors <- c(errors,msg)
  }
  # Convert to seconds for the check:
  raw_chk <- object@sign*(object@secs@secs + object@mins@mins*60 + object@hrs@hrs*3600)
  
  # Check for NA-to-non-NA pairs:
  if (any(is.na(raw_chk) & !is.na(object@raw)) ||
      any(!is.na(raw_chk) & is.na(object@raw))){
    # one NA, one none -- error
    msg <- "'raw' does not match value computed from 'hrs', 'mins' and 'secs' (one is NA)"
    errors <- c(errors,msg)
  } else {
    # All non-NA's:
    tol <- 1e-8 # seems reasonable...
    if (any(abs(raw_chk-object@raw)>tol,na.rm=TRUE)){
      if (TRUE){
        cat("raw_chk:\n") ; print(raw_chk)
        cat("object@raw:\n") ; print(object@raw)
        cat("abs(diff):\n") ; print(abs(raw_chk-object@raw))
      }
      msg <- "all 'raw' values must match value computed from 'hrs', 'mins' and 'secs'"
      errors <- c(errors,msg)
    }
  }
  # Bounds checks:
  if (any((object@secs@secs>=60 | object@secs@secs<0),na.rm=TRUE)){
      msg <- "Value for 'secs' out of bounds"
      errors <- c(errors,msg)
  }
  if (any((object@mins@mins>=60 | object@mins@mins<0),na.rm=TRUE)){
      msg <- "Value for 'mins' out of bounds"
      errors <- c(errors,msg)
  }
  if (any(abs(object@mins@mins-as.integer(object@mins@mins))>=.Machine$double.eps,na.rm=TRUE)){
      msg <- "Non-integer value for 'mins' not allowed"
      errors <- c(errors,msg)
  }
  if (any(abs(object@hrs@hrs-as.integer(object@hrs@hrs))>=.Machine$double.eps,na.rm=TRUE)){
      msg <- "Non-integer value for 'hrs' not allowed"
      errors <- c(errors,msg)
  }
  if (length(errors) == 0){
    return(TRUE)
  } else {
    if (TRUE){
      cat("===== ERROR =====\n")
      cat("hrs:\n") ; print(object@hrs)
      cat("mins:\n") ; print(object@mins)
      cat("secs:\n") ; print(object@secs)
      cat("raw:\n") ; print(object@raw)
    }
    return(errors)
  }
}

"check_seconds" <- function(object){
  return(TRUE)
}

"check_minutes" <- function(object){
  return(TRUE)
}

"check_hours" <- function(object){
  return(TRUE)
}

"check_sign" <- function(object){
  return(TRUE)
}

#' @exportClass "seconds"
setClass("seconds",
         representation(secs="numeric"),
         prototype(secs=NA_real_),
         validity=check_seconds)

#' @exportClass "minutes"
setClass("minutes",
         representation(mins="numeric"),
         prototype(mins=NA_real_),
         validity=check_minutes)

#' @exportClass "hours"
setClass("hours",
         representation(hrs="numeric"),
         prototype(hrs=NA_real_),
         validity=check_hours)

#' @exportClass "timing"
setClass("timing",
         representation(hrs="hours",mins="minutes",secs="seconds",
                        sign="numeric",raw="numeric"),
         prototype(hrs=new("hours",hrs=NA_real_),
                   mins=new("minutes",mins=NA_real_),
                   secs=new("seconds",secs=NA_real_),
                   sign=NA_real_,
                   raw=NA_real_),
         validity=check_timing)

#' Class for representing measurements of time (durations).
#' 
#' The function \code{\link{timing}} creates an object of class
#'\code{\link{timing}} representing measurements of time with values in seconds, 
#' minutes and hours. The object can be constructed from either an
#' object of classes 'seconds', 'minutes', 'hours', or from a numeric 
#' vector representing time
#' measurements in units specified by 'time.units'. 
#' The \code{\link{timing}} class 
#' represents hrs/min/sec and allows basic 
#' operations such as adding times, computing minimum/maximum times, 
#' summing times etc.
#' 
#' @param x either a vector of times in units specified by time.units,
#' or, an object of class \code{\link{hours}}, \code{\link{minutes}} or
#' \code{\link{seconds}}
#' @param time.units if x is a numeric vector, then one of either
#' "hours", "minutes" or "seconds"
#' @return An object of class 'timing'
#' @export "timing"
#' @seealso \code{\link{timing}}
#' @keywords timing
#' @examples
#' # Not run
#' x <- seq(-1,2,by=0.5)
#' y <- timing(x,time.units="hrs")
#  x
#' y
#' median(y)
#' summary(y)
"timing" <- function(x,time.units)
{
  ##
  # x can be either:
  # (1) of class 'hours', 'minutes', 'seconds', or, 
  # (2) numeric vector, with 'time.units' specifying the units.
  ##
  if (is(x,"hours") || is(x,"minutes") || is(x,"seconds")){
    return(as(x,"timing"))
  }
  if (missing(time.units)){
    stop("'time.units' must be specified")
  }
  if (length(time.units)!=1 || !is.character(time.units) ||
        !(time.units=="hours" || time.units=="minutes" || time.units=="seconds")){
    stop("'time.units' must be one of 'hours', 'minutes' or 'seconds'")
  }
  # new object of class hours, minutes or seconds:
  if (time.units=="hours"){
    tmp <- new("hours",hrs=x)
  }
  if (time.units=="minutes"){
    tmp <- new("minutes",mins=x)
  }
  if (time.units=="seconds"){
    tmp <- new("seconds",secs=x)
  }
  if (FALSE){
    cat("tmp:\n") ; print(tmp)
  }
  # coerce to timing:
  return(as(tmp,"timing"))
}

###############################################################################

#      setGeneric :: create new generic
# standardGeneric :: dispatches the appropriate method for an object
#           setAs :: object conversion
#      initialize :: called after creation with prototype (for checking etc)
#
###############################################################################

setAs("numeric" , "timing",
  function (from , to){
  new("timing", raw=as.numeric(from))
})

setAs("seconds" , "timing",
  function (from , to){
    # Convert to hrs/min/sec
    signs <- sign(from@secs)
    x <- abs(from@secs)
    hrs <- floor(x/3600)
    floor_1 <- hrs*3600
    left_1 <- x-floor_1
    mins <- floor(left_1/60)
    floor_2 <- floor_1 + mins*60
    left_2 <- x-floor_2
    sec <- left_2
    raw <- x*signs
    new("timing",
        hrs=new("hours",hrs=hrs),
        mins=new("minutes",mins=mins),
        secs=new("seconds",secs=sec),
        sign=signs,
        raw=raw)
})

setAs("minutes", "timing",
  function(from, to){
    # Convert to hrs/min/sec
    signs <- sign(from@mins)
    x <- abs(from@mins)
    hrs <- floor(x/60)
    floor_1 <- hrs*60
    left_1 <- x-floor_1
    mins <- floor(left_1)
    floor_2 <- floor_1 + mins
    left_2 <- (x-floor_2)*60
    sec <- left_2
    raw <- x*60*signs
    new("timing", 
        hrs=new("hours",hrs=hrs),
        mins=new("minutes",mins=mins),
        secs=new("seconds",secs=sec),
        sign=signs,
        raw=raw)
})

setAs("hours", "timing",
  function(from, to){
    # Convert to hrs/min/sec
    signs <- sign(from@hrs)
    x <- abs(from@hrs)
    hrs <- floor(x)
    floor_1 <- hrs
    left_1 <- x-floor_1
    mins <- floor(left_1*60)
    floor_2 <- floor_1 + (mins/60)
    left_2 <- (x-floor_2)*3600
    sec <- left_2
    raw <- x*3600*signs
    new("timing",
        hrs=new("hours",hrs=hrs),
        mins=new("minutes",mins=mins),
        secs=new("seconds",secs=sec),
        sign=signs,
        raw=raw)
})

###############################################################################

#' @export "concat"
setGeneric("concat",function(a,b){standardGeneric("concat")})

#' @exportMethod "concat"
setMethod("concat","timing",
  function(a,b){
    return(timing(c(a@raw,b@raw),time.units="seconds"))
})

setMethod("[","timing",
  function(x,i){
    return(timing(new("seconds",secs=x@raw[i])))
  })

#' @exportMethod "mean"
setGeneric("mean")

setMethod("mean","timing",
  function(x,...){
    return(timing(mean(x@raw,...),time.units="seconds"))
  })

#' @exportMethod "median"
setGeneric("median")

setMethod("median","timing",
          function(x,na.rm = FALSE){
            return(timing(median(x@raw,na.rm=na.rm),time.units="seconds"))
          })

#' @exportMethod "quantile"
setGeneric("quantile")

setMethod("quantile","timing",
          function(x,...){
            return(timing(quantile(x@raw,...),time.units="seconds"))
          })

#' @exportMethod "format"
setGeneric("format")

setMethod("format", "timing", 
  function(x) {
    sign_vec <- ifelse(x@sign<0,"-","")
    ret <- paste(sign_vec,x@hrs@hrs,":",
                 sprintf("%02d",x@mins@mins),":",
                 sprintf("%05.2f",x@secs@secs),sep="")
    return(ret)
})

#' @exportMethod "print"
setGeneric("print")

setMethod("print", "timing", 
  function(x) {
    print(format(x))
})

setMethod("min", "timing",
  function(x,na.rm=FALSE){
    return(timing(x=min(x@raw,na.rm=na.rm),time.units="seconds"))
})

setMethod("max", "timing",
  function(x,na.rm=FALSE){
    return(timing(x=max(x@raw,na.rm=na.rm),time.units="seconds"))
})

#' @exportMethod "summary"
setGeneric("summary")
setMethod("summary", "timing",
  function(object,...){
    qs <- quantile(object,...)
    ms <- mean(object,...)
    qsm <- format(concat(concat(qs[1:3],ms),qs[4:5]))
    names(qsm) <- c("Min.","1st Qu.","Median","Mean","3rd Qu.","Max")
    return(qsm)
})

setMethod("range", "timing",
  function(x,na.rm=FALSE){
    return(timing(x=c(min(x@raw,na.rm=na.rm),max(x@raw,na.rm=na.rm)),
            time.units="seconds"))
})

setMethod("length","timing",
  function(x){
    return(length(x@raw))
})

setMethod("sum", "timing",
  function(x,na.rm=FALSE){
    return(timing(x=sum(x@raw,na.rm=na.rm),time.units="seconds"))
})

setMethod("+", "timing",
  function(e1,e2){
    return(timing(x=(e1@raw+e2@raw),time.units="seconds"))
})

setMethod("-", "timing",
  function(e1,e2){
    return(timing(x=(e1@raw-e2@raw),time.units="seconds"))
})

setMethod(">", "timing",
  function(e1,e2){
    return(as.logical(e1@raw>e2@raw))
})

setMethod("<", "timing",
  function(e1,e2){
    return(as.logical(e1@raw<e2@raw))
})

setMethod(">=", "timing",
  function(e1,e2){
    return(as.logical(e1@raw>=e2@raw))
})

setMethod("<=", "timing",
  function(e1,e2){
    return(as.logical(e1@raw<=e2@raw))
})

setMethod("==", "timing",
  function(e1,e2){
    return(as.logical(e1@raw==e2@raw))
})

setMethod("!=", "timing",
  function(e1,e2){
    return(as.logical(e1@raw!=e2@raw))
})

