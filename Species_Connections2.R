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
    doc <- rsbml_read(filename, dom=FALSE)
    dom <- rsbml_dom(doc)
    mdl <- model(dom)
    lst[[i]] <- mdl
  }
  return(lst)
}
GetModelSpecies <- function(mdl){
  specs <- mdl@species
  return(specs)
}
GetSpeciesIDs <- function(lst){
  n <- length(lst)
  Spec_IDs <- character(n)
  if(n != 0){
    for(i in 1:n){
      Spec_IDs[i] <- lst[[i]]@id
    }
  }
  return(Spec_IDs)
}
OrderSpeciesIDs <- function(mdl, vec_1){
  vec_2 <- character()
  vec_3 <- character()
  lst <- mdl@reactions
  lst_3 <- list()
  n <- length(lst)
  k <- 1
  k_1 <- 1
  if(n != 0){
    for(i in 1:n){
      lst_1 <- lst[[i]]@reactants
      lst_2 <- lst[[i]]@products
      m <- length(lst_1)
      if(m != 0){
        for(j in 1:m){
          vec_2[k] <- lst_1[[j]]@species
          k <- k + 1
        }
      }
      l <- length(lst_2)
      if(l != 0){
        for(j in 1:l){
          vec_3[k_1] <- lst_2[[j]]@species
          k_1 <- k_1 + 1
        }
      }
    }
    lst_3[[1]] <- vec_2
    lst_3[[2]] <- vec_3
    vec_4 <- unique(unlist(lst_3))
  } else {
    vec_4 <- character(0)
  }
  return(vec_4)
}
GetUniqueSpecies <- function(filenames){
  mdls <- GetModels(filenames)
  n <- length(mdls)
  lst <- list()
  for(i in 1:n){
    mdl <- mdls[[i]]
    specs <- GetModelSpecies(mdl)
    spec_IDs <- GetSpeciesIDs(specs)
    ordered_IDs <- OrderSpeciesIDs(mdl, spec_IDs)
    lst[[i]] <- ordered_IDs
  }
  x <- unlist(lst)
  x_1 <- unique(x)
  return(x_1)
}
GetReactions <- function(file){
  doc <- rsbml_read(file, dom=FALSE)
  dom <- rsbml_dom(doc) 
  z <- model(dom)@reactions
  return(z)
}
GetAllReactions <- function(filenames){
  n <- length(filenames)
  lst <- list()
  for(i in 1:n){
    filename <- filenames[i]
    lst[[i]] <- GetReactions(filename) 
  }
  x <- unlist(lst)
  return(x)
}
ReactionConnections <- function(react, spec){
  is.reverse <- react@reversible
  rctnts <- react@reactants
  prdcts <- react@products
  m <- length(rctnts)
  n <- length(prdcts)
  rctnt_IDs <- character(m)
  prdct_IDs <- character(n)
  if(m != 0){
    for(i in 1:m){
      rctnt_IDs[i] <- rctnts[[i]]@species
    }
  } else {
    rctnt_IDs <- "No_Reactants"
  }
  if(n != 0){
    for(j in 1:n){
      prdct_IDs[j] <- prdcts[[j]]@species
    }
  } else {
    prdct_IDs <- "No_Reactants"
  }
  vec <- numeric(2)
  if(sum(spec == rctnt_IDs) != 0){
    if(is.reverse == TRUE){
      vec[1] <- sum(spec == rctnt_IDs)
      vec[2] <- sum(spec == rctnt_IDs)
    } else {
      vec[1] <- sum(spec == rctnt_IDs)
    }
  }
  if(sum(spec == prdct_IDs) != 0){
    if(is.reverse == TRUE){
      vec[1] <- sum(spec == prdct_IDs)
      vec[2] <- sum(spec == prdct_IDs)
    } else {
      vec[2] <- sum(spec == prdct_IDs)
    }
  }
  return(vec)
}
GetAllConnectionsInAModel <- function(mdl){
  mdl_specs <- GetModelSpecies(mdl)
  spec_IDs <- GetSpeciesIDs(mdl_specs)
  ordered_spec_IDs <- OrderSpeciesIDs(mdl, spec_IDs)
  reacts <- mdl@reactions
  m <- length(reacts)
  n <- length(ordered_spec_IDs)
  vec_1 <- numeric(n)
  vec_2 <- numeric(n)
  if(n != 0){
    if(m != 0){
      for(i in 1:n){
        spec <- ordered_spec_IDs[i]
        for(j in 1:m){
          react <- reacts[[j]]
          connects <- ReactionConnections(react, spec)
          vec_1[i] <- vec_1[i] + connects[1]
          vec_2[i] <- vec_2[i] + connects[2]
        }
      }
    }
  }
  vec <- ordered_spec_IDs
  df <- data.frame(vec, vec_1, vec_2, stringsAsFactors = FALSE)
  return(df)
}
GetAllConnections <- function(mdls){
  n <- length(mdls)
  for(i in 1:n){
    mdl <- mdls[[i]]
    if(i == 1){
      df <- GetAllConnectionsInAModel(mdl)
    } else {
      df_1 <- GetAllConnectionsInAModel(mdl)
      df <- rbind(df, df_1)
    }
  }
  return(df)
}
SortOutAllConnections <- function(dtfm){
  specs <- dtfm[, 1]
  uniq_specs <- unique(specs)
  n <- length(uniq_specs)
  vec_1 <- numeric(n)
  vec_2 <- numeric(n)
  for(i in 1:n){
    spec <- uniq_specs[i]
    df_1 <- dtfm[dtfm[, 1] == spec, ]
    vec_1[i] <- sum(df_1[, 2])
    vec_2[i] <- sum(df_1[, 3])
  }
  df <- data.frame(uniq_specs, vec_1, vec_2, stringsAsFactors = FALSE)
  return(df)
}
#Tests#
a <- WhichFilesWork("/home/b0033126/Documents/SBML")
b <- GetModels(a)
d <- GetModelSpecies(b[[1]])
e <- GetSpeciesIDs(d)
f <- OrderSpeciesIDs(b[[1]], e)
g <- GetUniqueSpecies(a)
h <- GetAllReactions(a)
w <- GetAllConnections(b)
x <- SortOutAllConnections(w)
y <- (sum(x[, 2]) + sum(x[, 3]))/length(x[, 1])
#End Of Tests#
z <- hist(x[, 2][x[, 2] < 100], breaks = 20, xlab="Number Of Connections", ylab="Frequency", main="Histogram Of Reactant Connections", xlim=c(0, 100), ylim=c(0, 3000))
z_1 <- hist(x[, 2][x[, 2] < 100], breaks = 20, xlab="Number Of Connections", ylab="Frequency", main="Histogram Of Product Connections", xlim=c(0, 100), ylim=c(0, 3000))