library(XLConnect)  #to load xlsx files in server.R
library(psych)  #for the describe function in statisticTable.R
library(ggplot2)  #for the vizualization plots
library(reshape2)  #for the melt function in correlation plot in correlationPlot.R
library(plyr)  #for the ddply function to create barplots for frequency table. Used in stackedBarplot.R and statisticsDiffAnal.R
library(xlsx)  #write excel files

count_missing <- function(dataset){
  return (sum(is.na(dataset)))
}

all_missing <- function(dataset){
  if (sum(is.na(dataset)) == length(dataset)){
    return (TRUE)
  }
  else{
    return (FALSE)
  }
}

cor.mtest <- function(mat, conf.level = 0.95, u, met){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <-  matrix(NA, n, n)
  colnames(p.mat) = colnames(mat)
  rownames(p.mat) = colnames(mat)
  diag(p.mat) <- NA
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      if (is.na(cor(mat[,i],mat[,j], use = u, method = met)) == F){
        tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level, use = u, method = met)
        p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      }
    }
  }
  return(p.mat)
}

abbreviateSTR <- function(value, prefix){  # format string more concisely
  lst = c()
  for (item in value) {
    if (is.nan(item) || is.na(item)) { # if item is NaN return empty string
      lst <- c(lst, '')
      next
    }
    item <- round(item, 2) # round to two digits
    if (item == 0) { # if rounding results in 0 clarify
      item = '<.01'
    }
    item <- as.character(paste("=", item, sep = ""))
    lst <- c(lst, paste(prefix, item , sep = ""))
  }
  return(lst)
}