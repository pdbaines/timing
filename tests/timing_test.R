
# Testing of 'timing' class:

foo <- new("timing", 
           hrs=new("hours",hrs=10), 
           mins=new("minutes",mins=12),
           secs=new("seconds",secs=50),
           sign=1.0,
           raw=c(10*3600 + 12*60 + 50))
print(foo)

if (FALSE){
foo <- new("timing", 
           hrs=new("hours",hrs=10), 
           mins=new("minutes",mins=12),
           secs=new("seconds",secs=50),
           sign=1.0,
           raw=c(10*3600 + 12*60 + 52)) # gives error
}

foo_vec <- seq(0,10,by=0.25)
foo2 <- timing(foo_vec,time.units="hours")
print(foo2)

###

scs <- new("seconds",secs=c(0.5,65.2,3599.9,3601.0))
scs

scs2 <- as(scs,"timing")
print(scs2)

###

mns <- new("minutes",mins=c(-1.0,30.0,60.0,61.0,661.5))
mns

mns2 <- as(mns,"timing")
print(mns2)

###

hors <- new("hours",hrs=c(-1.0,30.0,60.0,61.0,661.5))
hors

hors2 <- as(hors,"timing")
print(hors2)

print(timing(rnorm(10),time.units="hours"))
print(timing(rnorm(10),time.units="minutes"))
print(timing(rnorm(10),time.units="seconds"))
print(timing(hors))
print(timing(mns))
print(timing(scs))

mns2@raw

print(mns2)
print(mean(mns2))
print(quantile(hors2))

print(scs2)
print(median(scs2))

print(mns2)
print(median(mns2))

print(quantile(hors2,prob=seq(0,1,by=0.1)))

hors2[3]

min(hors2)
max(scs2)

length(mns2)
length(scs2)
length(hors2)

mns2+hors2
mns2-hors2
scs2+hors2 # warning

mns2 > hors2
hors2 == hors2
mns2 == (mns2+hors2-hors2)

print(sum(mns2))

print(foo2)
print(sum(foo2))

