#Function to produce correlation and p-value table for up to a maximum of 15 variables
makeText1.2 <- reactive({
  
  n <- length(colnames(selec_var()[[1]]))
  
  #For Correlation table
  if(input$select1.2 == 1){
    #Gives a correlation table of max 15 variables
    if(n > 15){
      variable <- selec_var()[[1]][,1:15]
      info <- cor(variable, use = "pairwise.complete.obs", method = input$type1.2)
      info
    }
    
    #Need at least 2 variables to give a correlation table
    else if(n > 2){
      variable <- selec_var()[[1]]
      info <- cor(variable, use = "pairwise.complete.obs", method = input$type1.2)
      info
    }
    
    else{
      return()
    }
  }
  
  #For P-Value Table
  else{
    #Gives a correlation significance table of max 15 variables
    if(n > 15){
      variable <- selec_var()[[1]][,1:15]
      p.mat <- cor.mtest(variable, u = "pairwise.complete.obs", met = input$type1.2)
      p.mat
    }
    
    #Need at least 2 variables to give a correlation significance table
    else if(n > 2){
      variable <- selec_var()[[1]]
      p.mat <- cor.mtest(variable, u = "pairwise.complete.obs", met = input$type1.2)
      p.mat
    }
    
    else{
      return()
    }
  }
  
})

#Function to produce correlation and p-value table with no limits to the number of variables
#For downloading purpose
makeText1.2_1 <- reactive({
  
  n <- length(colnames(selec_var()[[1]]))
  
  #For Correlation table
  if(input$select1.2 == 1){
    
    if(n > 2){
      variable <- selec_var()[[1]]
      info <- cor(variable, use = "pairwise.complete.obs", method = input$type1.2)
      info
    }
    
    else{
      return()
    }
  }
  
  #For P-Value Table
  else{
    if(n > 2){
      variable <- selec_var()[[1]]
      p.mat <- cor.mtest(variable, u = "pairwise.complete.obs", met = input$type1.2)
      p.mat
    }
    
    else{
      return()
    }
  }
  
})

#Function to produce the help text for the correlation and p-value table
helpText1.2 <- reactive({
  
  #For Correlation table
  if(input$select1.2 == 1){
    info1 <- paste("The table below displays the pairwise correlation values and")
  }
  
  #For P-Value table
  else if(input$select1.2 == 2){
    info1 <- paste("The table below displays the pairwise significance values and")
  }
  
  info2 <- paste("displays up to a maximum of 15 continuous variables.")
  info3 <- paste("")
  info4 <- paste("Download the data to view all of them.")
  
  cat(info1, "\n")
  cat(info2, "\n")
  cat(info3, "\n")
  cat(info4, "\n")
})

listb[["1-2"]] <- tagList(radioButtons("type1.2", "Correlation Type", 
                                       choices = c("Pearson (parametric)" = "pearson","Spearman (non-parametric)" = "spearman"), inline = T),
                          radioButtons("select1.2", "Select which table to view", 
                                       choices = c("Correlation Table" = 1, "P-Value Table" = 2), inline = T),
                          downloadButton('downloadData1.2', 'Download data'),
                          actionButton("help1.2","Help",icon=icon("question-circle")),
                          hidden(verbatimTextOutput("helptext1.2")),
                          verbatimTextOutput('text1.2'))

#Enables the "Help" button to produce the help text for the correlation and p-value table when clicked
observeEvent(input$help1.2, {
  toggle("helptext1.2")
})

output$helptext1.2 <- renderPrint({
  helpText1.2()
})

output$downloadData1.2 <- downloadHandler(
  filename = function() {
    if(input$select1.2 == 1){
      paste('Correlation Table','.xlsx', sep='')
    }
    else if(input$select1.2 == 2){
      paste('P Value Table','.xlsx', sep='')
    }},
  
  content = function(file) {
    write.xlsx(makeText1.2_1(), file, sheetName = "Sheet1", showNA = F)
  }
)

output$text1.2 <- renderPrint({
  makeText1.2()
})