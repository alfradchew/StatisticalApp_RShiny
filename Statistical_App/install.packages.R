new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

if (!require("pacman")) install.packages("pacman")
pacman::p_load("XLConnect", "psych", "ggplot2", "reshape2", "shiny", "shinyjs" ,"shinyBS", "plyr", "xlsx")