#Function to produce the histogram
makePlot2.1 <- reactive({
  dataset <- selec_var()[[1]]
  ID <- files[,1,drop=F]
  X_var <- dataset[,colnames(dataset) %in% input$hist_varinterest, drop = FALSE]
  X_var[,1] <- as.numeric(X_var[,1])
  
  df <- cbind(ID, X_var)
  df <- na.omit(df)
  colnames(df) <- c("ID","VarX")
  
  p <- ggplot(df, aes(x=VarX)) + 
    geom_histogram(fill = "blue", col = "white", bins = input$bins) +
      #Adjustments of theme for the boxplot
      theme(text = element_text(size = 20),
            axis.line.x = element_line(colour = "black", size = 1),
            axis.line.y = element_line(colour = "black", size = 1),
            axis.title.x = element_text(margin = margin(20,0,0,0)),
            axis.title.y = element_text(margin = margin(0,20,0,0)),
            axis.text.x = element_text(margin = margin(10,0,0,0)),
            axis.text.y = element_text(margin = margin(0,10,0,0)),
            plot.title = element_text(margin = margin(0,0,10,0), hjust = 0.5)) + xlab(input$hist_varinterest) + ylab("Frequency") +
      ggtitle(input$main2.1) + xlim(c(floor(min(df$VarX)), ceiling(max(df$VarX))))

    p
})

listb[["2-1"]] <- tagList(uiOutput("hist_varInterest"),
                          sliderInput(inputId = "bins",
                                      label = "Number of bins:",
                                      min = 1,
                                      max = 50,
                                      value = 30),
                          textInput("main2.1", "Key in the title of histogram"),
                          plotOutput("plot2.1", height = "600px"),
                          downloadButton('downloadPlot2.1', 'Download plot as pdf'))

output$hist_varInterest <- renderUI({
  selectInput("hist_varinterest", label = "Select Outcome of Interest", choices = colnames(selec_var()[[1]]), multiple = F)
})

output$plot2.1 <- renderPlot({
  makePlot2.1()
})

output$downloadPlot2.1 <- downloadHandler(
  filename = function() {"histogram.pdf"},
  content = function(file) {
    ggsave(file, makePlot2.1(), dpi = 600, width = 30, height = 20, units = "cm")})
