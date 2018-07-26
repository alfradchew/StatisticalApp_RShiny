#Function to produce the scatterplot
makePlot2.3 <- reactive({
  dataset <- selec_var()[[1]]
  group <- selec_var()[[2]]
  variable <- dataset[,colnames(dataset) %in% input$boxplot_cont, drop = F]
  group_var <- group[,colnames(group) %in% input$boxplot_cat, drop = F]
  
  variable[,1] <- as.numeric(variable[,1])
  group_var[,1] <- as.character(group_var[,1])
  group_var[group_var == ""] <- NA
  
  #coord_flip() transpose the graph 
  
  if(input$group2.3 == 1){
    sub_group_var <- group[,colnames(group) %in% input$sub_group2.3, drop = F]
    sub_group_var[,1] <- as.character(sub_group_var[,1])
    sub_group_var[sub_group_var == ""] <- NA
    
    df <- cbind(variable, group_var, sub_group_var)
    colnames(df) <- c("Cont","Group", "Subgroup")
    df <- na.omit(df)
    
    p <- ggplot(df, aes(x = factor(Group), y = Cont, fill = Subgroup)) + geom_boxplot(width = 0.5) + 
      scale_fill_discrete(name = input$sub_group2.3)
  }
  
  if(input$group2.3 == 2){
    df <- cbind(variable, group_var)
    colnames(df) <- c("Cont","Group")
    df <- na.omit(df)
    
    p <- ggplot(df, aes(x = factor(Group), y = Cont, fill = factor(Group)), colour = "black") + geom_boxplot(width = 0.5) + 
      scale_fill_discrete(name = input$boxplot_cat)
  }
  
  p <- p + theme(text = element_text(size = 20), 
                 axis.text.x = element_text(margin = margin(10,0,0,0)),
                 axis.text.y = element_text(margin = margin(0,10,0,0)),
                 panel.border = element_rect(colour = "black", fill=NA, size=1),
                 legend.key.height = unit(2.5, "line"),
                 axis.title.x = element_text(margin = margin(20,0,0,0)),
                 axis.title.y = element_text(margin = margin(0,20,0,0)),
                 plot.title = element_text(margin = margin(0,0,20,0), hjust = 0.5))  +
    ggtitle(input$main2.3) 
  
  if(input$invertBoxplot == 1){
    plt <- p + coord_flip() + ylab(input$boxplot_cont) + xlab(input$boxplot_cat)
  }
  
  if(input$invertBoxplot == 2){
    plt <- p + ylab(input$boxplot_cont) + xlab(input$boxplot_cat)
  }
  
  plt
  
})

listb[["2-3"]] <- tagList(fluidRow(column(4, uiOutput("boxplot_contVar")),
                                   column(4, uiOutput("boxplot_catVar"))),
                          fluidRow(column(4, radioButtons("invertBoxplot", label = "Invert Graph?", choices = c("Yes" = 1, "No" = 2), selected = 2, inline = T)),
                                   column(4, uiOutput("group2.3")),
                                   column(4, uiOutput("group_var2.3"))),
                          textInput("main2.3", "Key in the title of boxplot"),
                          plotOutput("plot2.3", height = "800px"),
                          downloadButton('downloadPlot2.3', 'Download plot as pdf'))

output$boxplot_contVar <- renderUI({
  selectInput("boxplot_cont", label = "Select Continuous Variable", choices = colnames(selec_var()[[1]]), multiple = F)
})

output$boxplot_catVar <- renderUI({
  selectInput("boxplot_cat", label = "Select Categorical Variable", choices = colnames(selec_var()[[2]]), multiple = F)
})

output$group2.3 <- renderUI({
  radioButtons("group2.3", label = "Display by Subgroup Variable?", choices = c("Yes" = 1, "No" = 2), selected = 1, inline = T)
})

output$group_var2.3 <- renderUI({
  if(input$group2.3 == 1){
    selectInput("sub_group2.3", label = "Select Group Variable?", 
                choices = colnames(selec_var()[[2]])[-which(colnames(selec_var()[[2]]) == input$boxplot_cat), drop = F], 
                multiple = F)
  }
  else{
    return()
  }
})

output$plot2.3 <- renderPlot({
  makePlot2.3()
})

output$downloadPlot2.3 <- downloadHandler(
  filename = function() {"boxplot.pdf"},
  content = function(file) {
    ggsave(file, makePlot2.3(), dpi = 600, width = 30, height = 20, units = "cm")})
