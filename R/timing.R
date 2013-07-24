
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
  
  if (any(is.na(raw_chk) & !is.na(object@raw)) ||
      any(!is.na(raw_chk) & is.na(object@raw))){
    # one NA, one none -- error
    msg <- "'raw' does not match value computed from 'hrs', 'mins' and 'secs' (one is NA)"
    errors <- c(errors,msg)
  }
  # TODO: add work for partial NA's
  if (all(!is.na(raw_chk)) && all(!is.na(object@raw))){
    tol <- 1e-8 # seems reasonable...
    if (any(abs(raw_chk-object@raw)>tol)){
      if (TRUE){
        cat("raw_chk:\n") ; print(raw_chk)
        cat("object@raw:\n") ; print(object@raw)
        cat("abs(diff):\n") ; print(abs(raw_chk-object@raw))
      }
      msg <- "all 'raw' values must match value computed from 'hrs', 'mins' and 'secs'"
      errors <- c(errors,msg)
    }
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

#' @exportClass
setClass("seconds",
         representation(secs="numeric"),
         prototype(secs=NA_real_),
         validity=check_seconds)

#' @exportClass
setClass("minutes",
         representation(mins="numeric"),
         prototype(mins=NA_real_),
         validity=check_minutes)

#' @exportClass
setClass("hours",
         representation(hrs="numeric"),
         prototype(hrs=NA_real_),
         validity=check_hours)

#' Class \code{\link{timing}}, for representing time measurements.
#' 
#' Objects of class \code{\link{timing}} contain values in seconds, 
#' minutes and hours representing durations. The class supports
#' most basic operations such as addition, subtraction (not 
#' multiplication and division), computing summaries such as min,
#' max, quantiles, mean and median.

#' The function \code{\link{timing}} is recommend to be used to 
#' create objects of the class, not the function \code{\link{new}}.
#' 
#' @value An object of class 'timing'
#' @exportClass
#' @seealso \code{\link{timing}}
#' @keywords timing
setClass("timing",
         representation(hrs="hours",mins="minutes",secs="seconds",
                        sign="numeric",raw="numeric"),
         prototype(hrs=new("hours",hrs=NA_real_),
                   mins=new("minutes",mins=NA_real_),
                   secs=new("seconds",secs=NA_real_),
                   sign=NA_real_,
                   raw=NA_real_),
         validity=check_timing)

#' Create an object of class \code{\link{timing}}, representing time measurements.
#' 
#' Create an object of class \code{\link{timing}} containing values in seconds, 
#' minutes and hour from either an object of classes 'seconds',
#' 'minutes', 'hours', or from a numeric vector representing time
#' measurements in units specified by 'time.units'. 
#' The \code{\link{timing}} represents hrs/min/sec and allows basic 
#' operations such as adding times, computing minimum/maximum times, 
#' summing times etc.
#' 
#' @param x either a vector of times in units specified by time.units,
#' or, an object of class \code{\link{hours}}, \code{\link{minutes}} or
#' \code{\link{seconds}}
#' @param time.units if x is a numeric vector, then one of either
#' "hours", "minutes" or "seconds"
#' @value An object of class 'timing'
#' @export
#' @seealso \code{\link{timing}}
#' @keywords timing
#' @examples
#' # Not run
#' x <- seq(-1,2,by=0.5)
#' y <- timing(x,time.units="hrs")
#  x
#' y
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
# Need:
#
# initialization from @raw
# 
#
# for timing class:
#
# -- element-access
# -- min
# -- max
# -- "+"
# -- "-"
# -- "mean"
# -- "median"
# -- "quantile"
# -- "summary"
# -- "sum"
# -- "range"
#
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

#' @export
setGeneric("concat",function(a,b){standardGeneric("concat")})

#' @exportMethod
setMethod("concat","timing",
  function(a,b){
    return(timing(c(a@raw,b@raw),time.units="seconds"))
})

# #' @exportMethod
# setGeneric("c")
# 
# setMethod("c","timing",
#   function(x,...){
#     mc <- as.list(match.call(expand.dots=TRUE))
#     ret <- timing(numeric(0),time.units="seconds")
#     if (length(mc)<2){
#       return(ret)
#     } else {
#       for (i in 2:length(mc)){
#         if (class(mc[i])!="timing"){
#           cat("mc:\n")
#           for (j in 1:length(mc)){
#             print(mc[j])
#           }
#           stop(paste("Unable to concatenate non-timing objects (class = ",
#                      class(mc[i]),")",sep=""))
#         }
#         ret <- timing(c(ret@raw,mc[i]@raw),time.units="seconds")
#       }
#     }
#     return(ret)
# })

setMethod("[","timing",
  function(x,i){
    return(timing(new("seconds",secs=x@raw[i])))
  })

#' @exportMethod
setGeneric("mean")

setMethod("mean","timing",
  function(x,...){
    return(timing(mean(x@raw,...),time.units="seconds"))
  })

#' @exportMethod
setGeneric("median")

setMethod("median","timing",
          function(x,na.rm = FALSE){
            return(timing(median(x@raw,na.rm=na.rm),time.units="seconds"))
          })

#' @exportMethod
setGeneric("quantile")

setMethod("quantile","timing",
          function(x,...){
            return(timing(quantile(x@raw,...),time.units="seconds"))
          })

#' @exportMethod
setGeneric("format")

setMethod("format", "timing", 
  function(x) {
    sign_vec <- ifelse(x@sign<0,"-","")
    ret <- paste(sign_vec,x@hrs@hrs,":",
                 sprintf("%02d",x@mins@mins),":",
                 sprintf("%02g",x@secs@secs),sep="")
    return(ret)
})

#' @exportMethod
setGeneric("print")

setMethod("print", "timing", 
  function(x) {
    print(format(x))
    return(ret)
})

setMethod("min", "timing",
  function(x,na.rm=FALSE){
    return(timing(x=min(x@raw,na.rm=na.rm),time.units="seconds"))
})

setMethod("max", "timing",
  function(x,na.rm=FALSE){
    return(timing(x=max(x@raw,na.rm=na.rm),time.units="seconds"))
})

#' @exportMethod
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
