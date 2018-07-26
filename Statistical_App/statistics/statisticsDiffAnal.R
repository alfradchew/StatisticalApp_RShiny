#Function for pairwise wilcox test
test.fun <- function(dat, col) { 
  
  c1 <- combn(unique(dat[,1]),2)
  sigs <- list()
  for(i in 1:ncol(c1)) {
    sigs[[i]] <- wilcox.test(
      dat[dat[,1] == c1[1,i],col],
      dat[dat[,1] == c1[2,i],col],
      alternative = "two.sided"
    )
  }
  names(sigs) <- paste(c1[1,],"by",c1[2,])
  
  tests <- data.frame(Group = names(sigs),
                      W=unlist(lapply(sigs,function(x) x$statistic)),
                      PValue=unlist(lapply(sigs,function(x) x$p.value)),row.names=NULL)
  
  return(tests)
}

#Function to produce the help text for differential analysis
helpText1.3.1 <- reactive({
  dataset <- selec_var()[[1]]
  group <- selec_var()[[2]]
  if(input$var_interest %in% colnames(dataset)){
    info1 <- paste("The headers in the table below represents")
    info2 <- paste("")
    info3 <- paste("Variable: Name of variables")
    info4 <- paste("Type: Variable type")
    info5 <- paste("n: Number of paired samples")
    info6 <- paste("rho (only continuous variables): Spearman's rank correlation value")
    info7 <- paste("RSquared (only continuous variables): rho^2")
    info8 <- paste("PValue: Significance level")
    info9 <- paste("QValue: Adjusted P-Value")
    info10 <- paste("")   
    info11 <- paste("Method for continuous variables: Spearman's rank correlation test")
    info12 <- paste("Method for categorical variables: Kruskal-Wallis test")
    
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
    cat(info11, "\n")
    cat(info12, "\n")
  }
  
  else {
    info1 <- paste("The headers in the table below represents")
    info2 <- paste("")
    info3 <- paste("Variable: Name of variables")
    info4 <- paste("Type: Variable type")
    info5 <- paste("n: Number of paired samples")
    info6 <- paste("PValue: Significance level")
    info7 <- paste("AdjustedPValue: Adjusted P-Value")
    info8 <- paste("")   
    info9 <- paste("Method for continuous variable: Kruskal-Wallis")
    info10 <- paste("Method for categorical variable: Fisher's exact Test")
    
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
})

#Function to produce the significance table for differential analysis
makeTable1.3.1 <- reactive({
  withProgress(message = 'Loading',value = 0, {
    
    #select_var()[[1]] = a data frame of all continuous variables
    #selec_var()[[2]] = a data frame of all cateogrical variables
    #Refer to mainControl.R for more info
    
    dataset <- selec_var()[[1]]
    group <- selec_var()[[2]]
    
    var_name <- NULL  #A vector of variable names (other than variable of interest)
    type <- NULL  #A vector of type of variables (categorical or continous)
    rho <- NULL  #A vector of correlation values (only applicable if variable X and variable Y are continuous)
    p_value <- NULL #A vector of p-values
    adjusted_p_value <- NULL #A vector of adjusted p-values (based on Benjamini Hochberg)
    r_squared <- NULL  #A vector of adjusted R squared values (only applicable if variable X and variable Y are continuous)
    n <- NULL #A vector of paired sample size
    method <- NULL #A vector of statistical methods used for analysis
    
    n1 <- length(input$choose_variable)
    
    #If variable Y is continous
    if(input$var_interest %in% colnames(dataset)){
      
      varInterest <- dataset[,colnames(dataset) %in% input$var_interest, drop =F]
      
      for(i in input$choose_variable){
        
        #Y(Continuous) vs X(Continuous)
        
        #Only do statistical test for variables other than the variable of interest
        if(i %in% colnames(dataset) & i != input$var_interest){
          
          #Select the column of variable for analysis
          v <- dataset[,colnames(dataset) %in% i, drop =F]
          
          a <- cor.test(varInterest[,1],v[,1],alternative = "two.sided",method = "spearman")
          
          var_name <- c(var_name,i)
          type <- c(type,"Continuous")
          n <- c(n, length(na.omit(cbind(varInterest,v))[,1]))
          rho <- c(rho, a$estimate)
          r_squared <- c(r_squared, (a$estimate)^2)
          p_value <- c(p_value, a$p.value)
          method <- c(method,"Spearman's rank correlation test")
          incProgress(1/n1)
        }
        
        #Y(Continuous) vs X(Categorical)
        else if(i %in% colnames(group) & i != input$var_interest){
          
          #Select the column of variable for analysis
          v <- group[,colnames(group) %in% i, drop =F]
          v[,1] <- as.character(v[,1])
          v[v == ""] <- NA
          
          a <- cbind(varInterest[,1],v[,1])
          a <- na.omit(a)  #Remove rows that contain emptry values
          
          if(length(unique(a[,2])) == 1){
            #Cannot used Kruskal-Wallis test if the number of groups in the categorical variables is 1
            method <- c(method,"Non Applicable")
            p_value <- c(p_value, NA)
          }
          if(length(unique(a[,2])) == 2){
            #Mann-Whitney U test
            a1 <- wilcox.test(as.numeric(a[,1]) ~ as.factor(a[,2]))
            method <- c(method,"Mann-Whitney U test")
            p_value <- c(p_value, a1$p.value)
          }
          if(length(unique(a[,2])) > 2){
            #Kruskal-Wallis test
            a2 <- kruskal.test(as.numeric(a[,1]) ~ as.factor(a[,2]))
            method <- c(method,"Kruskal-Wallis test")
            p_value <- c(p_value, a2$p.value)
          }
          
          var_name <- c(var_name,i)
          type <- c(type,"Categorical")
          
          #Give NA values for rho and r squared since kruskal wallis test does not produce correlation values
          rho <- c(rho, NA)
          r_squared <- c(r_squared, NA)
          
          n <- c(n, length(a[,1]))
          incProgress(1/n1)
        }
        
        adjusted_p_value <- rep(NA,length(var_name))
        
        df <- data.frame(Variable = var_name, Type = type, n = n, rho = rho, Rsquared = r_squared, PValue = p_value, AdjustedPValue = adjusted_p_value, Method = method, stringsAsFactors = F)
      }
    }
    
    #If variable Y is Categorical
    else if(input$var_interest %in% colnames(group)){
      
      varInterest <- group[,colnames(group) %in% input$var_interest, drop =F]
      varInterest[,1] <- as.character(varInterest[,1])
      varInterest[varInterest == ""] <- NA
      
      
      for(i in input$choose_variable){
        
        #Y(Categorical) vs X(Continuous)
        if(i %in% colnames(dataset) & i != input$var_interest){
          
          #Select the column of variable for analysis
          v <- dataset[,colnames(dataset) %in% i, drop =F]
          
          a <- cbind(varInterest[,1],v[,1])
          a <- na.omit(a) #Remove rows that contain emptry values
          
          if(length(unique(a[,1])) == 1){
            #Cannot used Kruskal-Wallis test if the number of groups in the categorical variables is 1
            method <- c(method,"Non Applicable")
            p_value <- c(p_value, NA)
          }
          if(length(unique(a[,1])) == 2){
            #Kruskal-Wallis test
            a1 <- wilcox.test(as.numeric(a[,2]) ~ as.factor(a[,1]))
            method <- c(method,"Mann-Whitney U test")
            p_value <- c(p_value, a1$p.value)
          }
          if(length(unique(a[,1])) > 2){
            #Kruskal-Wallis test
            a2 <- kruskal.test(as.numeric(a[,2]) ~ as.factor(a[,1]))
            method <- c(method,"Kruskal-Wallis test")
            p_value <- c(p_value, a2$p.value)
            
          }
          
          var_name <- c(var_name,i)
          type <- c(type,"Continuous")
          n <- c(n, length(a[,1]))
          incProgress(1/n1)
        }
        
        #Y(Categorical) vs X(Categorical)
        else if(i %in% colnames(group) & i != input$var_interest){
          
          v <- group[,colnames(group) %in% i, drop =F]
          v[,1] <- as.character(v[,1])
          v[v == ""] <- NA
          
          a <- cbind(varInterest[,1],v[,1])
          a <- na.omit(a) #Remove rows that contain emptry values
          
          #Fisher exact test
          a1 <- try(fisher.test(as.factor(a[,1]), as.factor(a[,2]), workspace = 2000000))
          
          #For cases for p values are extremely small for fisher test to work
          if(class(a1) == "try-error"){
            a1 <- fisher.test(as.factor(a[,1]), as.factor(a[,2]), simulate.p.value = TRUE, workspace = 2000000)
          }
          
          var_name <- c(var_name,i)
          type <- c(type,"Categorical")
          p_value <- c(p_value, a1$p.value)
          method <- c(method,"Fisher's exact test")
          
          n <- c(n, length(a[,1]))
          incProgress(1/n1)
        }
        
        adjusted_p_value <- rep(NA,length(var_name))
        
        df <- data.frame(Variable = var_name, Type = type, n = n, PValue = p_value, AdjustedPValue = adjusted_p_value, Method = method, stringsAsFactors = F)
      }
    }
    
    #Order the table by the p values
    df <- df[order(df$PValue),]
    
    df$AdjustedPValue <- p.adjust(df$PValue, method = "hochberg")
    
    incProgress(1/n1)
    df
    
  })
})

#Function to produce the visuzaliation plot
makePlot1.3.1 <- function(){
  dataset <- selec_var()[[1]]
  group <- selec_var()[[2]]
  
  #input_varinterest = variable Y
  #input_varinterest2 = variable X
  
  #Scatterplot, Y(Continuous) vs X(Continuous)
  if(input$var_interest %in% colnames(dataset) & input$var_interest2 %in% colnames(dataset)){
    X_var <- dataset[,colnames(dataset) %in% input$var_interest2, drop = FALSE]
    Y_var <- dataset[,colnames(dataset) %in% input$var_interest, drop = FALSE]
    X_var[,1] <- as.numeric(X_var[,1])
    Y_var[,1] <- as.numeric(Y_var[,1])
    ID <- files[,1,drop=F]
    
    df <- cbind(ID, X_var, Y_var)
    df <- na.omit(df)
    colnames(df)[c(1,2,3)] <- c("ID","VarX","VarY")
    
    #Tells ggplot to plot VarX on the X axis, VarY on the Y axis
    p <- ggplot(df, aes(x=VarX, y=VarY)) + geom_point(size = 2) + 
      scale_colour_hue(l = 50) + # Use a slightly darker palette than normal
      geom_smooth(method = lm,   # Add linear regression lines
                  se = FALSE,    # Don't add shaded confidence region
                  fullrange = TRUE) + #Extrapolate the line to fill the entire graph
      
      #Adjustments of theme for the scatterplot
      theme(text = element_text(size = 20),
            axis.line.x = element_line(colour = "black", size = 1),
            axis.line.y = element_line(colour = "black", size = 1),
            axis.title.x = element_text(margin = margin(20,0,0,0)),
            axis.title.y = element_text(margin = margin(0,20,0,0)),
            axis.text.x = element_text(margin = margin(10,0,0,0)),
            axis.text.y = element_text(margin = margin(0,10,0,0)),
            plot.title = element_text(margin = margin(0,0,10,0), hjust = 0.5)) + xlab(input$var_interest2) + ylab(input$var_interest) +
      ggtitle(input$main1.3.1)
    
    p
  }
  
  #Boxplot, Y(Categorical) vs X(Continuous)
  else if(input$var_interest %in% colnames(group) & input$var_interest2 %in% colnames(dataset)){
    variable <- dataset[,colnames(dataset) %in% input$var_interest2, drop=F]
    group_var <- group[,colnames(group) %in% input$var_interest, drop=F]
    
    group_var[,1] <- as.character(group_var[,1])
    group_var[group_var == ""] <- NA
    
    df <- cbind(variable, group_var)
    colnames(df) <- c("VarX","VarY")
    df <- na.omit(df)
    
    #Tells ggplot to plot VarX (must be a factor) on the X axis, VarY on the Y axis, and
    #to give a fixed width for each boxplot
    #coord_flip() transpose the graph   
    p <- ggplot(df, aes(factor(VarY), VarX, fill = VarY), colour = "black") + coord_flip() + geom_boxplot(width = 0.5) +
      
      #Adjustments of theme for the boxplot
      theme(text = element_text(size = 20), 
            axis.text.x = element_text(margin = margin(10,0,0,0)),
            axis.text.y = element_text(margin = margin(0,10,0,0)),
            panel.border = element_rect(colour = "black", fill=NA, size=1),
            legend.key.height = unit(2.5, "line"),
            axis.title.x = element_text(margin = margin(20,0,0,0)),
            axis.title.y = element_text(margin = margin(0,20,0,0)),
            plot.title = element_text(margin = margin(0,0,20,0), hjust = 0.5)) + xlab(input$var_interest) + ylab(input$var_interest2) +
      ggtitle(input$main1.3.1) + scale_fill_discrete(name = input$var_interest)
    p
  }
  
  #Boxplot, Y(Continuous) vs X(Categorical)
  else if(input$var_interest %in% colnames(dataset) & input$var_interest2 %in% colnames(group)){
    variable <- dataset[,colnames(dataset) %in% input$var_interest, drop=F]
    group_var <- group[,colnames(group) %in% input$var_interest2, drop=F]
    
    group_var[,1] <- as.character(group_var[,1])
    group_var[group_var == ""] <- NA
    
    df <- cbind(variable, group_var)
    colnames(df) <- c("VarX","VarY")
    df <- na.omit(df)
    
    #Tells ggplot to plot VarX (must be a factor) on the X axis, VarY on the Y axis, and
    #to give a fixed width for each boxplot
    p <- ggplot(df, aes(factor(VarY), VarX, fill = VarY ), colour = "black") + geom_boxplot(width = 0.5) +
      
      #Adjustments of theme for the boxplot
      theme(text = element_text(size = 20), 
            axis.text.x = element_text(hjust=1, margin = margin(10,0,0,0)),
            axis.text.y = element_text(margin = margin(0,10,0,0)),
            panel.border = element_rect(colour = "black", fill=NA, size=1),
            legend.key.height = unit(2.5, "line"),
            axis.title.x = element_text(margin = margin(20,0,0,0)),
            axis.title.y = element_text(margin = margin(0,20,0,0)),
            plot.title = element_text(margin = margin(0,0,20,0), hjust = 0.5)) + xlab(input$var_interest2) + ylab(input$var_interest) +
      ggtitle(input$main1.3.1) + scale_fill_discrete(name = input$var_interest2)
    p
  }
  
  #Barplot, Y(Categorical) vs X(Categorical)
  else{
    group2 <- group[,colnames(group) %in% input$var_interest, drop=F]
    group1 <- group[,colnames(group) %in% input$var_interest2, drop=F]
    
    group1[,1] <- as.character(group1[,1])
    group1[group1 == ""] <- NA
    group2[,1] <- as.character(group2[,1])
    group2[group2 == ""] <- NA
    
    df <- na.omit(cbind(group1,group2))
    
    df <- data.frame(df)
    #Give the frequency table for each combination of categorical variable
    counts <- ddply(df, .(df[,1], df[,2]), nrow)
    
    #Creates a 0 value for those combinations of categorical variables not present
    #in the frequency table
    for(i in unique(counts[,1])){
      for(j in unique(counts[,2])){
        a <- 0
        for(k in 1:length(rownames(counts))){
          if(i == counts[k,1] & j == counts[k,2]){
            a <- a + 1
          }
        }
        if(a == 0){
          counts <- rbind(counts,c(i,j,0))
        }
      }
    }
    
    colnames(counts) <- c("VarX","VarY","value")
    counts[,3] <- as.numeric(counts[,3])
    
    #Plotting the barplot
    ggplot(counts, aes(VarX, value)) +   
      geom_bar(aes(fill = VarY), position = "dodge", stat="identity", colour = "black") + xlab(input$var_interest2) + ylab("Freq") +
      ggtitle(input$main1.3.1) + scale_fill_discrete(name = input$var_interest) + 
      theme(text = element_text(size = 20), 
            axis.text.x = element_text(hjust=1, margin = margin(10,0,0,0)),
            axis.text.y = element_text(margin = margin(0,10,0,0)),
            panel.border = element_rect(colour = "black", fill=NA, size=1),
            legend.key.height = unit(2.5, "line"),
            axis.title.x = element_text(margin = margin(20,0,0,0)),
            axis.title.y = element_text(margin = margin(0,20,0,0)),
            plot.title = element_text(margin = margin(0,0,20,0), hjust = 0.5)) 
    
  }
}

#Function to produce the help text for differential analysis plot
helpText1.3.2 <- function(){
  
  info1 <- paste("Continuous X and Continuous Y: Scatterplot")
  info2 <- paste("Categorical X and Continuous Y: Boxplot")
  info3 <- paste("Continuous X and Categorical Y: Boxplot")
  info4 <- paste("Categorical X and Categorical Y: Barplot")
  
  cat(info1, "\n")
  cat(info2, "\n")
  cat(info3, "\n")
  cat(info4, "\n")
}

listb[["1-3"]] <- tagList(h3("Significance Test"),
                          uiOutput("varInterest"),
                          downloadButton('downloadData1.3.1', 'Download data'),
                          actionButton("help1.3.1","Help",icon=icon("question-circle")),
                          hidden(verbatimTextOutput("helptext1.3.1")),
                          br(),
                          dataTableOutput("table1.3.1"),
                          
                          br(),
                          uiOutput("varInterest2"),
                          textInput("main1.3.1", "Key in the title of plot"),
                          plotOutput("plot1.3.1", height = "600px"),
                          downloadButton('downloadPlot1.3.1', 'Download plot'),
                          actionButton("help1.3.2","Help",icon=icon("question-circle")),
                          hidden(verbatimTextOutput("helptext1.3.2")))

#input$choose_variable refers to all the selected variables
#Refer to mainControl.R for more info
output$varInterest <- renderUI({
  selectInput("var_interest", label = "Select outcome of interest", choices = input$choose_variable, multiple = F)
})

output$downloadData1.3.1 <- downloadHandler(
  filename = function() {"differential_analysis.xlsx"},
  content = function(file) {
    write.xlsx(makeTable1.3.1(), file, sheetName = "Sheet1", showNA = F, row.names = F)})


#Enable the "Help" button  to produce the results table when clicked
observeEvent(input$help1.3.1, {
  toggle("helptext1.3.1")
})

output$helptext1.3.1 <- renderPrint({
  helpText1.3.1()
})

output$table1.3.1 <- renderDataTable({
  makeTable1.3.1()
})

output$varInterest2 <- renderUI({
  selectInput("var_interest2", label = "Select explanatory variable for visualization", choices = makeTable1.3.1()$Variable, multiple = F)
})

output$plot1.3.1 <- renderPlot({
  makePlot1.3.1()
})

output$downloadPlot1.3.1 <- downloadHandler(
  filename = function() {"diff_analysis_plot.pdf"},
  content = function(file) {
    ggsave(file, makePlot1.3.1(), dpi = 600, width = 30, height = 20, units = "cm")})


#Enable the "Help" button  to produce the results table when clicked
observeEvent(input$help1.3.2, {
  toggle("helptext1.3.2")
})

output$helptext1.3.2 <- renderPrint({
  helpText1.3.2()
})