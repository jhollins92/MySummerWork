rm(list=ls())
library(rsbml)
WhichFilesWork <- function(direc){
  setwd(direc)
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
  return(l)
}
GetModels <- function(filenames){
  n <- length(filenames)
  lst <- list()
  for(i in 1:n){
    filename <- filenames[i]
    doc <- rsbml_read(filename, dom = FALSE)
    dom <- rsbml_dom(doc)
    lst[[i]] <- model(dom)
  }
  return(lst)
}
GetParameters <- function(mdl){
  lst <- list()
  lst_1 <- list()
  lst[[1]] <- (parameters(mdl))
  reacts <- reactions(mdl)
  n <- length(reacts)
  if(n != 0){
    for(i in 1:n){
      react <- reacts[[i]]
      paras <- react@kineticLaw@parameters
      if(length(paras) != 0){
        lst_1[[i]] <- paras
      }
    }
  }
  lst[[2]] <- unlist(lst_1) 
  return(unlist(lst))
}
ProportionOfUnknownParameters <- function(paras){
  n <- length(paras)
  unknown_initial_value <- 0
  known_initial_value <- 0
  for(i in 1:n){
    para <- paras[[i]]
    if(length(para) != 0){
      if(length(para@value) != 0){
        known_initial_value <- known_initial_value + 1
      } else {
        unknown_initial_value <- unknown_initial_value + 1
      }
    }
  }
  total <- known_initial_value + unknown_initial_value
  proportion <- unknown_initial_value/total
  return(proportion)
}
ProportionOfUnknownParametersForallModels <- function(direc){
  mdls <- GetModels(direc)
  n <- length(mdls)
  vec <- numeric(n)
  for(i in 1:n){
    mdl <- mdls[[i]]
    params <- GetParameters(mdl)
    unknown_params <- ProportionOfUnknownParameters(params)
    vec[i] <- unknown_params  
  }
  return(vec)
}
#Tests#
a <- WhichFilesWork("/home/b0033126/Documents/SBML")
b <- GetModels(a)
d <- GetParameters(d[[1]])
e <- ProportionOfUnknownParameters(d)
f <- ProportionOfUnknownParametersForallModels(a)
f
#End Of Tests#
#And now, for the species#
GetSpecies <- function(mdl){
  return(species(mdl))
}
ProportionOfUnknownSpecies <- function(specs){
  n <- length(specs)
  unknown_initial_value <- 0
  known_initial_value <- 0
  if(n != 0){
    for(i in 1:n){
      spec <- specs[[i]]
      if(length(spec) != 0){
        if(length(spec@initialAmount) == 0 & length(spec@initialConcentration) == 0){
          unknown_initial_value <- unknown_initial_value + 1
        } else {
          known_initial_value <- known_initial_value + 1
        }
      }
    }
    total <- known_initial_value + unknown_initial_value
    proportion <- unknown_initial_value/total
  } else {
    proportion <- -1
  }
  return(proportion)
}
ProportionOfUnknownSpeciesForallModels <- function(direc){
  mdls <- GetModels(direc)
  n <- length(mdls)
  vec <- numeric(n)
  for(i in 1:n){
    mdl <- mdls[[i]]
    specs <- GetSpecies(mdl)
    unknown_specs <- ProportionOfUnknownSpecies(specs)
    vec[i] <- unknown_specs  
  }
  return(vec)
}
#Tests#
x <- GetSpecies(b[[1]])
y <- ProportionOfUnknownSpecies(x)
y
z <- ProportionOfUnknownSpeciesForallModels(a)
z
#End of Tests#