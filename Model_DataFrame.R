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
GetModelID <- function(mdl){
  if(length(id(mdl)) != 0){
    ID <- id(mdl)
  } else {
    ID <- mdl@name
  }
  return(ID)
}
GetModelDate <- function(mdl){
  split_1 <- strsplit(mdl@annotation, "<dcterms:W3CDTF>")
  split_2 <- strsplit(split_1[[1]][2], "T")
  dt <- as.Date(split_2[[1]][1])
  dt_1 <- as.POSIXlt(dt)
  yr <- dt_1$year + 1900
  return(yr)
}
GetNumberOfSpecies <- function(mdl){
  return(length(sapply(species(mdl), id)))
}
GetNumberOfReactions <- function(mdl){
  return(length(sapply(reactions(mdl), id)))
}
GetNumberofCompartments <- function(mdl){
  return(length(sapply(compartments(mdl), id)))
}
GetNumberofGlobalParameters <- function(mdl){
  return(length(sapply(parameters(mdl), id)))
}
GetNumberOfLocalParameters <- function(mdl){
  r <- reactions(mdl)
  n <- length(r)
  y <- list()
  if(n == 0){
    x <- 0
  } else {
    w <- numeric(n)
    for(j in 1:n){
      s <- length(r[[j]]@kineticLaw@parameters)
      w[j] <- s
    }
    x <- sum(w)
  }
  y[[2]] <- x
  if(x != 0){
    y[[1]] <- "T"
  } else {
    y[[1]] <- "F"
  }
  return(y)
}
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
  if(n != 0){
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
  } else {
    proportion <- -1
  }
  return(proportion)
}
DataFrameForTheModels <- function(Models){
  No_Of_Models <- length(Models)
  for(i in 1:No_Of_Models){
    Model <- Models[[i]]
    ID <- GetModelID(Model)
    DATE <- GetModelDate(Model)
    Species <- GetNumberOfSpecies(Model)
    Reactions <- GetNumberOfReactions(Model)
    Compartments <- GetNumberofCompartments(Model)
    GlobalParameters <- GetNumberofGlobalParameters(Model)
    LocalParametersPresent <- GetNumberOfLocalParameters(Model)[[1]]
    LocalParameters <- GetNumberOfLocalParameters(Model)[[2]]
    IdentifySpecies <- GetSpecies(Model)
    UnknownSpeciesProportion <- ProportionOfUnknownSpecies(IdentifySpecies)
    IdentifyParameters <- GetParameters(Model)
    UnknownParametersProportion <- ProportionOfUnknownParameters(IdentifyParameters)
    df1 <- data.frame(ID, DATE, Species, Reactions, Compartments, GlobalParameters, LocalParametersPresent, LocalParameters, UnknownSpeciesProportion , UnknownParametersProportion, stringsAsFactors=FALSE)
    if(i == 1){
      df <- df1
    } else {
      df <- rbind(df, df1)
    }
  }
  return(df)
} 
#Tests#
a <- WhichFilesWork("/home/b0033126/Documents/SBML")
b <- GetModels(a)
d <- GetModelID(b[[1]])
e <- GetModelDate(b[[1]])
f <- GetNumberOfSpecies(b[[1]])
g <- GetNumberOfReactions(b[[1]])
h <- GetNumberofCompartments(b[[1]])
p <- GetNumberofGlobalParameters(b[[1]])
q <- GetNumberOfLocalParameters(b[[1]])
r <- GetSpecies(b[[1]])
s <- ProportionOfUnknownSpecies(r)
u <- GetParameters(b[[1]])
v <- ProportionOfUnknownParameters(u)
w <- DataFrameForTheModels(b)
b
#End Of Tests#