#Produce the statistics for continuous/ categorical variables
makeText1.1 <- reactive({
  
  #Limit to 3 significant figures
  options(digits = 3)
  
  #If users choose to view statistics for continuous variables
  if(input$select1.1 == 1){
    
    #select_var()[[1]] contains the data frame of all the selected continous variables
    #Refer to mainControl.R for more info
    variable <- selec_var()[[1]]
    
    #The describe function produces basic statistics such as mean, median, sample size
    info <- describe(variable)
    
    #Count missing values
    if (length(variable) > 1){
      Missing <- NULL
      for (j in 1:length(variable)){
        Missing <- c(Missing, count_missing(variable[,j]))
      }
    }
    else{
      Missing <- count_missing(variable[,1])
    }
    
    #Remove unnecessary information produced by the "describe" function
    info <- cbind(info[2],Missing,info[c(3:5,8,9,10)])
    info
  }
  
  #If users choose to view statistics for continuous variables
  else if(input$select1.1 == 2){
    
    #select_var()[[2]] contains the data frame of all the selected categorical variables
    #Refer to mainControl.R for more info
    cat_var <- selec_var()[[2]]
    
    main_variable <- NULL
    group_variable <- NULL
    n <- NULL
    
    for (i in 1:length(colnames(cat_var))){
      
      #Previously in mainControl.R all the columns for the categorical variables are encoded as a factor
      #Have to convert back to character to count the number of missing values
      cat_var[,i] <- as.character(cat_var[,i])
      
      #Convert emptry string "" to NA so that the R function could count the number of missing values
      cat_var[,i][which(cat_var[,i] == "")] <- NA
      
      #Build a contingency table of the counts at each combination of factor levels.
      a <- table(cat_var[,i])
      
      #Count number of missing values, refer to helpers.R for the count_missing function
      n_missing <- count_missing(cat_var[,i])
      
      main_variable <- c(main_variable, colnames(cat_var)[i], rep("", length(a)))
      group_variable <- c(group_variable, names(a), "Missing")
      n <- c(n, as.data.frame(a)$Freq, n_missing)
      
    }
    
    df <- data.frame(Variable = main_variable, Group = group_variable, n = n)
    df <- format(df, justify = "left")
    print(df, right=F, row.names = F)
  }
})

#Function to produce the help text that describes the statistics table
helpText1.1 <- reactive({
  if(input$select1.1 == 1){
    info1 <- paste("The headers in the table below represents")
    info2 <- paste("")
    info3 <- paste("n: number of present samples")
    info4 <- paste("missing: numer of missing samples")
    info5 <- paste("mean: average")
    info6 <- paste("sd: standard deviation")
    info7 <- paste("median: 50 percentile value")
    info8 <- paste("min: minimum value")
    info9 <- paste("max: maximum value")   
    info10 <- paste("range: max - min")
    
    cat(info1, "\n")
    cat(info2, "\n")
    cat(info3, "\n")
    cat(info4, "\n")
    cat(info5, "\n")
    cat(info6, "\n")
    cat(info7, "\n")
    cat(info8, "\n")
    cat(info9, "\n")
    cat(info10, "\n")
  }
  else if(input$select1.1 == 2){
    info1 <- paste("The table below displays the sample size of each group for each categorical variable.")
    cat(info1, "\n")
  }
})

listb[["1-1"]] <- tagList(radioButtons("select1.1", paste("Select which statistics table to view"), 
                                       choices = c("Continuous variable" = 1, "Categorical variable" = 2), inline = T),
                          downloadButton('downloadData1.1', 'Download data'),
                          actionButton("help1.1","Help",icon=icon("question-circle")),
                          hidden(verbatimTextOutput("helptext1.1")),
                          verbatimTextOutput('text1.1'))

#Enable the "Help" button to produce the help text when clicked
observeEvent(input$help1.1, {
  toggle("helptext1.1")
})

output$helptext1.1 <- renderPrint({
  helpText1.1()
})

# output$downloadData1.1 <- downloadHandler(
#   filename = function() {
#     if(input$select1.1 == 1){
#       paste('exp_stats','.csv', sep='')
#     }
#     else if(input$select1.1 == 2){
#       paste('cat_stats','.csv', sep='')
#     }},
#   
#   content = function(file) {
#     if(input$select1.1 == 1){
#       #The row names of the describe function are the variables themselves.
#       write.csv(makeText1.1(), file,na="")
#     }
#     else if(input$select1.1 == 2){
#       write.csv(makeText1.1(), file,na="", row.names = F)
#     }
#   }
# )

output$downloadData1.1 <- downloadHandler(
  filename = function() {
    if(input$select1.1 == 1){
      paste('exp_stats','.xlsx', sep='')
    }
    else if(input$select1.1 == 2){
      paste('cat_stats','.xlsx', sep='')
    }},
  
  content = function(file) {
    if(input$select1.1 == 1){
      #The row names of the describe function are the variables themselves.
      write.xlsx(makeText1.1(), file, sheetName = "Sheet1", showNA = F)
    }
    else if(input$select1.1 == 2){
      write.xlsx(makeText1.1(), file, sheetName = "Sheet1", showNA = F, row.names = F)
    }
  }
)

output$text1.1 <- renderPrint({
  makeText1.1()
})