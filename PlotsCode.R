rm(list=ls())
library(rsbml)
setwd("/home/b0033126/Documents/SBML")
#Number of Models Against Time#
#Find Created Date#
l <- list.files()
n <- length(l)
for(i in 1:n){
  doc <- try(rsbml_read(l[i], dom=FALSE), silent=TRUE)
  if(class(doc) == "SBMLDocument"){
    dom <- try(rsbml_dom(doc), silent=TRUE)
    if(class(dom) != "SBML"){
      l[i] <- "Model Doesn't Work"
    }
  } else {
    l[i] <- "Model Doesn't Work"
  }
}
l <- l[l != "Model Doesn't Work"]
l
n <- length(l)
z <- character(n)
for(i in 1:n){
  doc <- rsbml_read(l[i], dom=FALSE)
  dom <- rsbml_dom(doc)
  string_1 <- model(dom)@annotation
  x <- strsplit(string_1, "<dcterms:W3CDTF>")
  y <- strsplit(x[[1]][2], "T")
  z[i] <- y[[1]][1]
}
#Convert To Dates#
d <- as.Date(z)
p <- as.POSIXlt(d)
q <- p$year
r <- q + 1900
#Plot Code#
m <- max(r) - (min(r) - 1)
s <- numeric(m)
years <- numeric(m)
for(i in 1:m){
  s[1] = sum(r==min(r))
  for(j in 1:(m-1)){
    year = min(r) + j
    s[j+1] = s[j] +sum(r==year)
  }
  years[i] = min(r) + i - 1
}
plot(years, s, type="l", main="Plot of Cummulative Number of Biomodels against Year", xlab="Year", ylim=c(0, 350), ylab="Cummulative Total")
#Number of Species Against Time#
#Find Years#
rm(list=ls())
library(rsbml)
setwd("/home/b0033126/Documents/SBML")
l <- list.files()
n <- length(l)
for(i in 1:n){
  doc <- try(rsbml_read(l[i], dom=FALSE), silent=TRUE)
  if(class(doc) == "SBMLDocument"){
    dom <- try(rsbml_dom(doc), silent=TRUE)
    if(class(dom) != "SBML"){
      l[i] <- "Model Doesn't Work"
    }
  } else {
    l[i] <- "Model Doesn't Work"
  }
}
l <- l[l != "Model Doesn't Work"]
l
GetYears <- function(filenames){
  n <- length(filenames)
  z <- character(n)
  for(i in 1:n){
    filename <- filenames[i]
    doc <- rsbml_read(filename, dom=FALSE)
    dom <- rsbml_dom(doc)
    string_1 <- model(dom)@annotation
    x <- strsplit(string_1, "<dcterms:W3CDTF>")
    y <- strsplit(x[[1]][2], "T")
    z[i] <- y[[1]][1]
  }
  d <- as.Date(z)
  p <- as.POSIXlt(d)
  q <- p$year
  years <- q + 1900
  return(years)
}
years <- GetYears(l)
#Functions to Extract Number of Species#
IdentifyAllModelsInAnyGivenYear <- function(vector, year){
  n <- length(vector)
  for(i in 1:n){
    if(vector[i] == year){
      vector[i] = i
    } else {
      vector[i] = 0
    }
  }
  z = vector[vector != 0]
  return(z)
}
GetAllSpeciesFromAYear <- function(filenames, vector){
  n <- length(vector)
  x <- list()
  for(i in 1:n){
    v = vector[i]
    filename <- filenames[v]
    doc <- rsbml_read(filename, dom=FALSE)
    dom <- rsbml_dom(doc)
    s <- sapply(species(model(dom)), id)
    u <- names(s)
    x[[i]] <- u
  }
  return(x)
}
lst <- list()
m <- max(years) - min(years) + 1
s <- numeric(m)
for(i in 1:m){
  yr <- 2004 + i
  mdls <- IdentifyAllModelsInAnyGivenYear(years, yr)
  lst[[i]] <- unlist(GetAllSpeciesFromAYear(l, mdls))
  u <- unlist(lst)
  s[i] <- length(unique(u))
}
s
#Plot Code#
yrs <- c(2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012)
plot(yrs, s, type="l", main="Plot of Cummulative Number of Species \n in the Biomodels Database for Each Year", xlab="Year", ylim=c(0, 4000), ylab="Cummulative Number of Species")