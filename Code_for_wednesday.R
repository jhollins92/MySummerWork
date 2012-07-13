rm(list=ls())
library(rsbml)
setwd("/home/b0033126/Documents/SBML")
l = list.files()
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
n = length(l)
u = character(n)
for(i in 1:n){
	doc = rsbml_read(l[i], dom = FALSE)
	dom = rsbml_dom(doc)
	if(length(id(model(dom))) == 0){
		u[i] = name(model(dom))
	} else {
		u[i] = id(model(dom))
	}
}
v = numeric(n)
for(i in 1:n){
  doc = rsbml_read(l[i], dom = FALSE)
  dom = rsbml_dom(doc)
  model(dom)
  v[i] = length(sapply(compartments(model(dom)), id))
}
w = numeric(n)
for(i in 1:n){
  doc = rsbml_read(l[i], dom = FALSE)
  dom = rsbml_dom(doc)
  model(dom)
  w[i] = length(sapply(species(model(dom)), id))
}
x = numeric(n)
for(i in 1:n){
	doc = rsbml_read(l[i], dom = FALSE)
	dom = rsbml_dom(doc)
	model(dom)
	x[i] = length(sapply(reactions(model(dom)), id))
}
y = numeric(n)
for(i in 1:n){
	doc = rsbml_read(l[i], dom = FALSE)
	dom = rsbml_dom(doc)
	model(dom)
	y[i] = length(sapply(parameters(model(dom)), id))
}
z = numeric(n)
for(i in 1:n){
	doc = rsbml_read(l[i], dom = FALSE)
	dom = rsbml_dom(doc)
	r = reactions(model(dom))
	m = length(r)
	if(m == 0){
		z[i] = 0
	} else {
		sm = numeric(m)
		for(j in 1:m){
			s = length(r[[j]]@kineticLaw@parameters)
			sm[j] = s
		}
	z[i] = sum(sm)
	}  
}
u
v
w
x
y
z
v1 <- numeric(7)
for(i in 1:7){
  v1[i] <- sum(v == i)
}
barplot(v1, names.arg=c(1, 2, 3, 4, 5, 6, 7), xlab="Number of Compartments", main="Barplot of the Number of Compartments \n in each Biomodel", ylim=c(0, 200))
hist(w[w<=150], breaks=15, main="Histogram of the Number of Species \n in each Biomodel", xlim=c(0, 150), xlab="Number of Species", ylim=c(0, 200), ylab="Frequency")
hist(x[x<=400], breaks=20, main="Histogram of the Number of Reactions \n in each Biomodel", xlim=c(0, 400), xlab="Number of Reactions", ylim=c(0, 200), ylab="Frequency")
hist(y, breaks=25, main="Histogram of the Number of Global Parameters \n in each Biomodel", xlim=c(0,250), xlab="Number of Global Parameters", ylim=c(0, 150), ylab="Frequency")
hist(z[z<1735], breaks=35, main="Histogram of the Number of Local Parameters \n in each Biomodel", xlim=c(0,350), xlab="Number of Local Parameters", ylim=c(0, 250), ylab="Frequency")
hist(z[z>0 & z<1735], breaks=35, main="Histogram of the Number of Local Parameters in each Biomodel \n (models with zero local parameters removed)", xlim=c(0,350), xlab="Number of Local Parameters", ylim=c(0, 30), ylab="Frequency")