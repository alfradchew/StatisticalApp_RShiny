#Function to produce the scatterplot
makePlot2.2 <- reactive({
  dataset <- selec_var()[[1]]
  group <- selec_var()[[2]]
  
  ID <- files[,1,drop=F]
  X_var <- dataset[,colnames(dataset) %in% input$scatterX, drop = FALSE]
  Y_var <- dataset[,colnames(dataset) %in% input$scatterY, drop = FALSE]
  X_var[,1] <- as.numeric(X_var[,1])
  Y_var[,1] <- as.numeric(Y_var[,1])
    
  if(input$group2.2 == 1){
    if(length(colnames(group)) >= 2){  
      group <- group[,colnames(group) %in% input$scatter_group2.2, drop = FALSE]
      group[,1] <- as.character(group[,1])
      group[group == ""] <- NA
      df <- cbind(ID, group, X_var, Y_var)
      df <- na.omit(df)
      df[,2] <- as.factor(df[,2])
      colnames(df)[c(1,2,3,4)] <- c("ID","Group","VarX","VarY")
    }
    else{
      group[,1] <- as.character(group[,1])
      group[group == ""] <- NA
      df <- cbind(ID, group, X_Var, Y_var)
      df <- na.omit(df)
      df[,2] <- as.factor(df[,2])
      colnames(df)[c(1,2,3,4)] <- c("ID","Group","VarX","VarY")
    }
      
    #Tells ggplot to plot VarX on the X axis, VarY on the Y axis, and
    #to give a different colour for each dot (of size 2) for different groups
    p <- ggplot(df, aes(x = VarX, y = VarY)) + geom_point(size = 2, aes(colour = Group)) +
      scale_colour_hue(l = 50) + # Use a slightly darker palette than normal
        
      #Adjustments of theme for the scatterplot
      theme(text = element_text(size = 20),
            axis.line.x = element_line(colour = "black", size = 1),
            axis.line.y = element_line(colour = "black", size = 1),
            axis.title.x = element_text(margin=margin(20,0,0,0)),
            axis.title.y = element_text(margin=margin(0,20,0,0)),
            axis.text.x = element_text(margin=margin(10,0,0,0)),
            axis.text.y = element_text(margin=margin(0,10,0,0)),
            plot.title = element_text(margin=margin(0,0,10,0))) + xlab(input$scatterX) + ylab(input$scatterY) + scale_colour_discrete(name = input$scatter_group2.2) +
      ggtitle(input$main2.2)
  }
    
    
  if(input$group2.2 == 2){
    df <- cbind(ID, X_var, Y_var)
    df <- na.omit(df)
    colnames(df)[c(1,2,3)] <- c("ID","VarX","VarY")
      
    #Tells ggplot to plot VarX on the X axis, VarY on the Y axis
    p <- ggplot(df, aes(x = VarX, y = VarY)) + geom_point(size = 2) + 
        
        #Adjustments of theme for the scatterplot
        theme(text = element_text(size=20),
              axis.line.x = element_line(colour = "black", size = 1),
              axis.line.y = element_line(colour = "black", size = 1),
              axis.title.x = element_text(margin=margin(20,0,0,0)),
              axis.title.y = element_text(margin=margin(0,20,0,0)),
              axis.text.x = element_text(margin=margin(10,0,0,0)),
              axis.text.y = element_text(margin=margin(0,10,0,0)),
              plot.title = element_text(margin=margin(0,0,10,0))) + xlab(input$scatterX) + ylab(input$scatterY) +
        ggtitle(input$main2.2)
  }
  
  if(input$line2.2 == 1 & input$group2.2 == 1){
    p <- p + 
      geom_smooth(method=lm,   # Add linear regression lines
                  se=FALSE,    # Don't add shaded confidence region
                  aes(colour = Group),  # Give a different colour for each group
                  fullrange=TRUE) #Extrapolate the line to fill the entire graph
  }
  
  if(input$line2.2 == 1 & input$group2.2 == 2){
    p <- p + 
      geom_smooth(method=lm,   # Add linear regression lines
                  se=FALSE,    # Don't add shaded confidence region
                  fullrange=TRUE) #Extrapolate the line to fill the entire graph
  }
  
  if(input$ellipse_2.2 == 1){
    p <- p + stat_ellipse(geom = "polygon", alpha = 1/2, aes(fill = Group)) + scale_fill_discrete(guide=FALSE)
  }
  
  p
})

listb[["2-2"]] <- tagList(fluidRow(column(4, uiOutput("scatter_varX")),
                                   column(4, uiOutput("scatter_varY"))),
                          fluidRow(column(4, uiOutput("group2.2")),
                                   column(4, uiOutput("ellipse2.2")),
                                   column(4, uiOutput("group_var2.2"))),
                          radioButtons("line2.2", label = "Insert Line of Best Fit?", choices = c("Yes" = 1, "No" = 2), selected = 2, inline = T),
                          textInput("main2.2", "Key in the title of scatterplot"),
                          plotOutput("plot2.2", height = "600px"),
                          downloadButton('downloadPlot2.2', 'Download plot as pdf'))

output$scatter_varX <- renderUI({
  selectInput("scatterX", label = "Select Variable X", choices = colnames(selec_var()[[1]]), multiple = F)
})

output$scatter_varY <- renderUI({
  selectInput("scatterY", label = "Select Variable Y", choices = colnames(selec_var()[[1]])[-which(colnames(selec_var()[[1]]) == input$scatterX)])
})

output$group2.2 <- renderUI({
  radioButtons("group2.2", label = "Display by Group Variable?", choices = c("Yes" = 1, "No" = 2), selected = 1, inline = T)
})

output$ellipse2.2 <- renderUI({
  if(input$group2.2 == 1){
    radioButtons("ellipse_2.2", label = "Insert Standard Deviational Ellipse?", choices = c("Yes" = 1, "No" = 2), selected = 2, inline = T)
  }
})

output$group_var2.2 <- renderUI({
  if(input$group2.2 == 1){
    selectInput("scatter_group2.2", label = "Select Group Variable?", choices = colnames(selec_var()[[2]]), colnames(selec_var()[[2]])[1], multiple = F)
  }
  else{
    return()
  }
})

output$plot2.2 <- renderPlot({
  makePlot2.2()
})

output$downloadPlot2.2 <- downloadHandler(
  filename = function() {"scatterplot.pdf"},
  content = function(file) {
    ggsave(file, makePlot2.2(), dpi = 600, width = 30, height = 20, units = "cm")})
