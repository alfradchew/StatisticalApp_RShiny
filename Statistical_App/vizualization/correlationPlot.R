#Function to produce p-value text such as p<0.01** and p=0.03*
abbreviateSTR_P <- function(value, prefix){
  lst = c()
  for (item in value) {
    
    if (is.nan(item) || is.na(item)) { # if item is NaN return empty string
      lst <- c(lst, '')
      next
    }
    
    #Round the number to 3 decimal places
    item <- round(item, 3)
    
    if (item < 0.01) {
      item <- '<0.01**'
      lst <- c(lst, paste(prefix, item , sep = ""))
    }
    
    else if (item < 0.05){
      item <- paste(item,"*",sep="")
      item <- as.character(paste("=", item, sep = ""))
      lst <- c(lst, paste(prefix, item , sep = ""))
    }
    
    else if (item >= 0.05){
      item <- as.character(paste("=", item, sep = ""))
      lst <- c(lst, paste(prefix, item , sep = ""))
    }

  }
  return(lst)
}

#Function to produce correlation text such as r=0.01 and r=0.02
abbreviateSTR_R <- function(value, prefix){  
  lst = c()
  for (item in value) {
    if (is.nan(item) || is.na(item)) { # if item is NaN return empty string
      lst <- c(lst, '')
      next
    }
    item <- round(item, 3) 
    item <- as.character(paste("=", item, sep = ""))
    lst <- c(lst, paste(prefix, item , sep = ""))
  }
  return(lst)
}

#Function to produce the correlation matrix
makePlot2.5 <- function(text_size){
  n <- length(selec_var()[[1]])
  variable <- selec_var()[[1]]
  if(length(input$choose_variable2.5) >= 2){
    if(input$type2.5 == "pearson"){
      r <- "r"
    }
    
    if(input$type2.5 == "spearman"){
      r <- "rho"
    }
    
    variable <- variable[,colnames(variable) %in% input$choose_variable2.5, drop = FALSE]
    
    #Plot up to a maximum of 150variables
    if(length(input$choose_variable2.5) > 10){
      variable <- variable[,1:10]
    }
    
    info <- cor(variable, use = "pairwise.complete.obs", method = input$type2.5)
    p.mat <- cor.mtest(variable, u = "pairwise.complete.obs", met = input$type2.5)
    
    j <- NULL
    for (i in 1:length(info[1,])){
      if (all_missing(info[,i])){
        j <- c(j,i)
      }
    }
    
    if(length(j) > 0){
      info <- info[,-j]
      p.mat <- p.mat[,-j]      
    }

    cordata <- melt(info)
    pdata <- melt(p.mat)
    
    cordata$labelr = abbreviateSTR_R(melt(cordata)$value, prefix = r)
    cordata$labelP = abbreviateSTR_P(melt(pdata)$value, prefix = 'p')
    
    #Puts R label on top and P label below 
    cordata$label = paste(cordata$labelr, "\n", 
                          cordata$labelP, sep = "")
    
    #Removes label if user wants to display significant pairwise variables only
    if(input$displaysig2.5 == T){
      for(i in 1:length(rownames(cordata))){
        if(!is.na(pdata$value[i]) & pdata$value[i] > ((100 - input$alpha_2.5) / 100)){
          cordata$label[i] <- NA
        }
      }
    }
    
    #Creates a lower triangular matrix
    cordata.lower = subset(cordata[lower.tri(info, diag = T),])
    cordata.lower$Var1 <- with(cordata.lower, factor(cordata.lower$Var1, levels = rev(levels(cordata.lower$Var1))))
    
    txtsize <- par('din')[2] / 2
    
    a <- 0
    if(length((p.mat[,1])) > 10){
      a <- 2
    }
    
    p <- ggplot(cordata.lower, aes(x=Var1, y=Var2)) + geom_tile(aes(fill = value), colour = "black") + theme_classic() +
      theme(legend.key.height = unit(2.5, "line"),
            axis.text.x = element_text(size = text_size, hjust=TRUE, angle = 45, margin=margin(10,0,0,0)),
            axis.text.y = element_text(size = text_size, margin=margin(0,10,0,0)),
            plot.title = element_text(size = text_size, margin=margin(0,0,10,0))) +
      xlab("") + ylab("") + 
      geom_text(label = cordata.lower$label, size = input$slider2.5) +
      scale_fill_gradientn(name=r, colours = rev(rainbow(20*10, start = 0/6, end = 4/6))) +
      ggtitle(input$main2.2) +
      annotate("text", x = (length(p.mat[,1])-a), y = (length(p.mat[,1])-a) , label = paste("** p<0.01", " * p<0.05", sep = "\n"), size = 6)
    
    p
  } 
  else({
    return()
  })
}

#Function to produce the help text for the correlation plot
helpText2.5 <- function(){
  info1 <- paste("The plot above displays the correlation plot filled with a colour gradient.")
  info2 <- paste("")
  info3 <- paste("Maximum number of variables displayed: 10")
  info4 <- paste("If more than 10 variables are selected, only the first 10 will be plotted.")
  
  cat(sprintf(info1), "\n")
  cat(sprintf(info2), "\n")
  cat(sprintf(info3), "\n")
  cat(sprintf(info4), "\n")
}

listb[["2-5"]] <- tagList(radioButtons("type2.5", "Correlation Type",
                                       choices = c("Pearson (parametric)" = "pearson",
                                                   "Spearman (non-parametric)" = "spearman"),
                                       inline = T),
                          uiOutput("Select_all2.5"),

                          br(),
                          
                          uiOutput("Choice2.5"),
                          
                          br(),
                          
                          checkboxInput("displaysig2.5", "Display Only Significant Variables"),
                          uiOutput("uiExample2.5"),
                            
                          br(),
                                 
                          fluidRow(column(4, sliderInput("slider2.5", "Enter the Font Size of Labels in Correlation Plot", min = 1, max = 10, value = 3, step = 0.5, width = "500px")),
                                   column(4, uiOutput("alpha2.5"))),
                          
                          br(),
                          
                          uiOutput("Main2.5"),
                          plotOutput("Plot2.5", height = "800px"),
                          downloadButton('downloadPlot2.5', 'Download plot as pdf'),
                          actionButton("help2.5","Help",icon=icon("question-circle")),
                          hidden(verbatimTextOutput("helptext2.5")))


output$alpha2.5 <- renderUI({
  if(input$displaysig2.5 == T){
    sliderInput("alpha_2.5", "Input Significance Level (%)", 95, min = 0, max = 100, step = 0.5)
  }
})

output$Select_all2.5 <- renderUI({
  radioButtons("select_all2.5", paste("Select All", length(colnames(selec_var()[[1]])), "Continuous Variables for the Correlation Matrix?"), choices = c("Yes" = 1, "No" = 2), selected = 1, inline = T)
})

output$Choice2.5 <- renderUI({
  if(input$select_all2.5 == 2){
    selectizeInput("choose_variable2.5", "Select at Least 2 Continuous Variables for the Correlation Matrix (Max 10)", choices = colnames(selec_var()[[1]]), 
                   multiple = T)
  }
  else if(input$select_all2.5 == 1){
    if(length(colnames(selec_var()[[1]])) > 10){
      selectInput("choose_variable2.5", "Select at Least 2 Continuous Variables for the Correlation Matrix (Max 10)", choices = colnames(selec_var()[[1]]), 
                  multiple = T, selectize = F, selected = colnames(selec_var()[[1]]), size = 10)
    }
    else{
      selectInput("choose_variable2.5", "Select at Least 2 Continuous Variables for the Correlation Matrix (Max 10)", choices = colnames(selec_var()[[1]]), 
                  multiple = T, selectize = F, selected = colnames(selec_var()[[1]]), size = length(colnames(selec_var()[[1]])))
    }
  }
})

output$uiExample2.5 <- renderUI({
  tipify(bsButton("pB22", "Help", icon=icon("question-circle"), size = "extra-small"), placement = "right",
         "Removes label for insignificant correlation values based on the inputed significance level.")
})

output$Main2.5 <- renderUI({
  if(length(input$choose_variable2.5) > 1){
    textInput("main2.5", "Key in the title of correlation plot")
  }
})

output$Plot2.5 <- renderPlot({
  makePlot2.5(20)
})

output$downloadPlot2.5 <- downloadHandler(
  filename = function() {
    paste('Correlation Matrix','.pdf', sep='')},
  content = function(file) {
    ggsave(file, makePlot2.5(20), dpi = 600, height = 30, width = 30, units = "cm")})

#Enables the "Help" button to produce the help text for the correlation plot
observeEvent(input$help2.5, {
  toggle("helptext2.5")
})

output$helptext2.5 <- renderPrint({
  helpText2.5()
})