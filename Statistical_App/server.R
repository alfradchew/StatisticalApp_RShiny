library(shiny)
source("helpers.R")
files <- NULL

shinyServer(function(input, output, session) {
  
  upload_files <- eventReactive(input$upload,{
    
    if(is.null(input$file))
      return()
    
    if (input$type == 'xlsx'){
      files <<- readWorksheetFromFile(input$file$datapath, header = T, sheet = 1)
    }
    
    if (input$type == 'csv'){
      files <<- read.csv(input$file$datapath, sep = ',', header = T, stringsAsFactors=F, check.names = F)
      }
    }
  )
  
  #dictionary used in analysis. Refer to analysisPage.R for more info
  listb <- list()
  
  source("mainControl.R",local=TRUE)
  source("analysisPage.R",local=TRUE)
  
  source("statistics/characterizationTable.R", local=TRUE)
  source("statistics/correlationTable.R", local=TRUE)
  source("statistics/statisticsDiffAnal.R", local=TRUE)
  
  source("vizualization/histogram.R", local=TRUE)
  source("vizualization/scatterplot.R", local=TRUE)
  source("vizualization/boxplot.R", local=TRUE)
  source("vizualization/stackedBarplot.R", local=TRUE)
  source("vizualization/correlationPlot.R", local=TRUE)
})
