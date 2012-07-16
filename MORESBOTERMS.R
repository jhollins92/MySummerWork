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
GetModelsboTerms <- function(fl){
  pattern <- " sboTerm=\"SBO:[[:digit:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]]\""
  doc <- rsbml_read(fl, dom=FALSE)
  xml <- rsbml_xml(doc)
  m <- gregexpr(pattern, xml)
  mtchs <- regmatches(xml, m)
  sboTermVec <- unlist(mtchs)
  return(sboTermVec)
}
FrequencyOfsboTermsInaModel <- function(vec){
  unqTerms <- unique(vec)
  n <- length(unqTerms)
  freqvec <- numeric(n)
  for(i in 1:n){
    freqvec[i] <- sum(vec == unqTerms[i])
  }
  return(freqvec)
}
DataFrameOfsboTermsInaModel <- function(fl){
  Terms <- GetModelsboTerms(fl)
  if(length(Terms) != 0){
    unqTerms <- unique(Terms)
    freq <- FrequencyOfsboTermsInaModel(Terms)
    n <- length(unqTerms)
    ModID <- character(n)
    doc <- rsbml_read(fl, dom=FALSE)
    dom <- rsbml_dom(doc)
    if(length(id(model(dom))) == 0){
      ID <- name(model(dom))
    } else {
      ID <- id(model(dom))
    }
    for(i in 1:n){
      ModID[i] <- ID
    }
    df <- data.frame(unqTerms, ModID, freq, stringsAsFactors = FALSE)
  } else {
    df <- data.frame()
  }
  return(df)
}
DataFrameOfsboTermsinAllModels <- function(fls){
  n <- length(fls)
  for(i in 1:n){
    fl <- fls[i]
    df1 <- DataFrameOfsboTermsInaModel(fl)
    if(i == 1){
      df <- df1 
    } else {
      df <- rbind(df, df1)
    }
  }
  return(df)
}
GetCorrespondingNames <- function(fl){
  pattern <- " sboTerm=\"SBO:[[:digit:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]]\""
  doc <- rsbml_read(fl, dom=FALSE)
  xml <- rsbml_xml(doc)
  splt <- strsplit(xml, pattern)
  unlst <- unlist(splt)
  n <- length(unlst)
  vec <- character(n)
  for(i in 1:n){
    vec[i] <- unlist(strsplit(unlst[i], "\""))[2]
  }
  return(vec)
}
#Tests#
a <- WhichFilesWork("/home/b0033126/Documents/SBML")
b <- GetModelsboTerms(a[1])
d <- FrequencyOfsboTermsInaModel(b)
e <- DataFrameOfsboTermsInaModel(a[1])
f <- DataFrameOfsboTermsinAllModels(a)
g <- GetCorrespondingNames(a[1])
m <- length(g)
h <- g[2:m]
df <- data.frame(h, b, stringsAsFactors=FALSE)
df_1 <- data.frame(h[2:13], b[2:13], stringsAsFactors=FALSE)
#End of Tests#
doc <- rsbml_read(a[1], dom=FALSE)
xml <- rsbml_xml(doc)
pattern1 <- " sboTerm=\"SBO:0000290\""
splt <- strsplit(xml, pattern1)
unlst <- unlist(splt)
unlst