
# Testing of 'timing' class:

foo <- new("timing", 
           hrs=new("hours",hrs=10), 
           mins=new("minutes",mins=12),
           secs=new("seconds",secs=50),
           sign=1.0,
           raw=c(10*3600 + 12*60 + 50))
foo

if (FALSE){
foo <- new("timing", 
           hrs=new("hours",hrs=10), 
           mins=new("minutes",mins=12),
           secs=new("seconds",secs=50),
           sign=1.0,
           raw=c(10*3600 + 12*60 + 52)) # gives error
}

foo_vec <- seq(0,10,by=0.25)
#foo2 <- timing(foo_vec,time.units="hours")
#print(foo2)

###

scs <- new("seconds",secs=c(0.5,65.2,3599.9,3601.0))
scs

scs2 <- as(scs,"timing")
scs2

###

mns <- new("minutes",mins=c(-1.0,30.0,60.0,61.0,661.5))
mns

mns2 <- as(mns,"timing")
mns2

###

hors <- new("hours",hrs=c(-1.0,30.0,60.0,61.0,661.5))
hors

hors2 <- as(hors,"timing")
hors2

timing(rnorm(10),time.units="hours")
timing(rnorm(10),time.units="minutes")
timing(rnorm(10),time.units="seconds")
timing(hors)
timing(mns)
timing(scs)

mns2@raw

mean(mns2)
quantile(hors2)
median(scs2)

quantile(hors2,prob=seq(0,1,by=0.1)) # fails
