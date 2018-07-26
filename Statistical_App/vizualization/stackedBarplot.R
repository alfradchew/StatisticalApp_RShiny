#Function to produce the scatterplot
makePlot2.4 <- reactive({
  group <- selec_var()[[2]]
  
  group_var <- group[,colnames(group) %in% input$stackX, drop = FALSE]
  sub_group_var <- group[,colnames(group) %in% input$stackY, drop = FALSE]
  group_var[,1] <- as.character(group_var[,1])
  group_var[group_var == ""] <- NA
  sub_group_var[,1] <- as.character(sub_group_var[,1])
  sub_group_var[sub_group_var == ""] <- NA
    
  df <- cbind(group_var, sub_group_var)
  colnames(df) <- c("Group", "Subgroup")
  df <- na.omit(df)
  
  counts <- ddply(df, .(df[,1], df[,2]), nrow)
  colnames(counts) <- c("Group","Subgroup","Frequency")
  
  
  if(input$insertCount == 1){
    p <- ggplot(counts, aes(x = Group, y = Frequency, fill = Subgroup, label = Frequency)) + 
      geom_bar(stat = "identity") +
      geom_text(size = 5, position = position_stack(vjust = 0.5))
  }
  
  if(input$insertCount == 2){
    p <- ggplot(counts, aes(x = Group, y = Frequency, fill = Subgroup)) + 
      geom_bar(stat = "identity")
  }
  
  p <- p + theme(text = element_text(size = 20),
                 axis.line.x = element_line(colour = "black", size = 1),
                 axis.line.y = element_line(colour = "black", size = 1),
                 axis.title.x = element_text(margin=margin(20,0,0,0)),
                 axis.title.y = element_text(margin=margin(0,20,0,0)),
                 axis.text.x = element_text(margin=margin(10,0,0,0)),
                 axis.text.y = element_text(margin=margin(0,10,0,0)),
                 plot.title = element_text(margin=margin(0,0,10,0))) + xlab(input$stackX) + ylab("Frequency") + labs(fill = input$stackY) +
      ggtitle(input$main2.4)
  
  if(input$invertBarplot == 1){
    plt <- p + coord_flip() 
  }
  
  if(input$invertBarplot == 2){
    plt <- p 
  }
  
  plt
    
})

listb[["2-4"]] <- tagList(fluidRow(column(4, uiOutput("stack_varX")),
                                   column(4, uiOutput("stack_varY"))),
                          fluidRow(column(4, radioButtons("invertBarplot", label = "Invert Graph?", choices = c("Yes" = 1, "No" = 2), selected = 2, inline = T)),
                                   column(4, radioButtons("insertCount", label = "Insert Count Label?", choices = c("Yes" = 1, "No" = 2), selected = 2, inline = T))),
                          textInput("main2.4", "Key in the title of stacked barplot"),
                          plotOutput("plot2.4", height = "600px"),
                          downloadButton('downloadPlot2.4', 'Download plot as pdf'))

output$stack_varX <- renderUI({
  selectInput("stackX", label = "Select Variable of Interest", choices = colnames(selec_var()[[2]]), multiple = F)
})

output$stack_varY <- renderUI({
  selectInput("stackY", label = "Select Group Variable", choices = colnames(selec_var()[[2]])[-which(colnames(selec_var()[[2]]) == input$stackX)], multiple = F)
})

output$plot2.4 <- renderPlot({
  makePlot2.4()
})

output$downloadPlot2.4 <- downloadHandler(
  filename = function() {"Stacked_Boxplot.pdf"},
  content = function(file) {
    ggsave(file, makePlot2.4(), dpi = 600, width = 30, height = 20, units = "cm")})
