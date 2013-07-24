
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

setClass("seconds",
         representation(secs="numeric"),
         prototype(secs=NA_real_),
         validity=check_seconds)

setClass("minutes",
         representation(mins="numeric"),
         prototype(mins=NA_real_),
         validity=check_minutes)

setClass("hours",
         representation(hrs="numeric"),
         prototype(hrs=NA_real_),
         validity=check_hours)

setClass("timing",
         representation(hrs="hours",mins="minutes",secs="seconds",
                        sign="numeric",raw="numeric"),
         prototype(hrs=new("hours",hrs=NA_real_),
                   mins=new("minutes",mins=NA_real_),
                   secs=new("seconds",secs=NA_real_),
                   sign=NA_real_,
                   raw=NA_real_),
         validity=check_timing)

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

setGeneric("mean")

setMethod("mean","timing",
  function(x,...){
    return(timing(mean(x@raw,...),time.units="seconds"))
  })

setGeneric("median")

setMethod("median","timing",
          function(x,na.rm = FALSE){
            return(timing(median(x@raw,na.rm=na.rm),time.units="seconds"))
          })

setGeneric("quantile")

setMethod("quantile","timing",
          function(x,...){
            return(timing(quantile(x@raw,...),time.units="seconds"))
          })

#setMethod("print", "timing", 
#  function(x) {
#  structure(list(x))
#})
#
#setMethod("min", "timing",
#  function(x,na.rm=FALSE){
#    return(new(raw=min(x@raw,na.rm=na.rm)))
#}
#
#"max.timing" <- function(x,na.rm=FALSE){
#  return(convert(max(x@raw,na.rm=na.rm),from.units="sec"))
#}

