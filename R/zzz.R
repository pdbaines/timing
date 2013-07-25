
".onLoad" <- function(libname, pkgname) {
  ## figure out this year automatically 
  this.year <- substr(as.character(Sys.Date( )), 1, 4)
  
  ## echo output to screen
  packageStartupMessage("##\n## timing\n")
  packageStartupMessage("## Copyright 2013-", this.year, " Paul Baines\n\n", sep="")
}





