#Output title "Selection of Variables" when user click the "upload" button
output$title <- renderUI({
  upload_files()
  h3("Selection of Variables")
})

#Radio button to allow user to select all variables
output$selectAll <- renderUI({
  upload_files()
  dataset <- files
  radioButtons("select_all", "Select all variables?", choices = c("Yes" = 1, "No" = 2), selected = 1, inline = T)
})

#Select button to allow users to choose variables
output$choose_var <- renderUI({
  upload_files()
  
  #Remove first column because it is the sample ID
  dataset <- files[-1]
  #list of variable names
  var_names <- colnames(dataset)
  
  #User select "Yes" for "Select all variables?"
  if(input$select_all == 1){
    selectInput("choose_variable", "Select explanatory variables", choices = var_names, 
                multiple = T, selectize = F, selected = var_names, size = 10)
  }
  
  #User select "No" for "Select all variables?"
  else if(input$select_all == 2){
    selectizeInput("choose_variable", "Select explanatory variables", choices = var_names, 
                   multiple = T)
  }
  
})

#Select button to allow users to choose which variables are categorical
output$select_cat_var <- renderUI({
  upload_files()
  
  #Remove first column because it is the sample ID
  dataset <- files[-1]
  
  #When at least one variable is selected for analysis
  if(length(input$choose_variable) > 0){
    
    #Select the relevant columns of the data 
    dataset <- dataset[,colnames(dataset) %in% input$choose_variable,drop = FALSE]
    
    #list of variable names
    var_names <- colnames(dataset)
    
    #Create a vector to store the number of unique entries for each variable
    #in order to rank which variables are highly likely to be categorical
    #(the lower the number of unique entries, the more likely the variable is categorical)
    uniq_var <- NULL
    
    #Append the number of unique entries for each variable to the vector
    for (index in 1:length(colnames(dataset))){
      uniq_var <- c(uniq_var, length(unique(dataset[,index])))
    }
    
    #df is a data frame which contains a list of variable names in the first column
    #and a list of unique entries in the second column
    df <- data.frame(var_names, uniq_var, stringsAsFactors = F)
    #Sort the data frame according to the number of unique entries in ascending order
    df <- df[order(df[,2]),]
    
    #Preselect suspected categorical variables which contain no numerical data
    suspected_var <- NULL
    for(index in 1:length(colnames(dataset))){
      #Convert all the data into numeric. Non numerical data will be converted to NA values.
      #If the entire column contain only NA values, means the column is assumed to be categorical
      dataset[,index] <- as.numeric(dataset[,index])
      
      #Refer to helpers.R for the all_missing function
      if(all_missing(dataset[,index])){
        suspected_var <- c(suspected_var, colnames(dataset)[index])
      }
    }
    
    selectizeInput("cat_variable", "Select categorical explanatory variables", 
                   choices = df[,1], selected = suspected_var, multiple = T)
  }
  
  #No variables are selected, hence unable to determine which variables are categorical
  else {
    helpText("No variables are selected.")
  }
})
outputOptions(output, 'select_cat_var', suspendWhenHidden=FALSE)

#Help text to state that the list of variables are sorted according to their likelihood to be a categorical variable
#and those variables suspected to be categorical are preselected
output$help <- renderUI({
  upload_files()
  dataset <- files
  dataset <- dataset[,colnames(dataset) %in% input$choose_variable,drop = FALSE]
  
  #Only show the test if more than one variable is selected
  if(length(dataset) > 1){
    helpText("The variables in the list above are sorted by its likelihood to be a categorical variable. 
             Variables suspected to be categorical have been preselected.")
  }
})
outputOptions(output, 'help', suspendWhenHidden=FALSE)

#Action button to select variables
output$select_var <- renderUI({
  upload_files()
  actionButton('select_variable', "Select")
})

#Select button to select the statistical functions to execute
output$select_func <- renderUI({
  upload_files()
  selectInput("main_function",
              "Select main function",
              choices = c("Statistics" = 1,
                          "Vizualization" = 2)
  )
})
outputOptions(output, 'select_func', suspendWhenHidden=FALSE)

#Create a list which contains a list of non categorical variables, categorical variables
#and statistical function
selec_var <- eventReactive(input$select_variable,{
  dataset <- files
  #Select all the relevant variables
  dataset <- dataset[,colnames(dataset) %in% input$choose_variable,drop = FALSE]
  #Select non categorical variables
  exp_var <- dataset[,!colnames(dataset) %in% input$cat_variable,drop = FALSE]
  #Select categorical variables
  cat_var <- dataset[,colnames(dataset) %in% input$cat_variable,drop = FALSE]
  
  #Convert the data in the entire column to be numeric for all non categorical variables
  for(index in 1:length(colnames(exp_var))){
    exp_var[,index] <- as.numeric(exp_var[,index])
  }
  
  list(exp_var,  cat_var, input$main_function)
})
