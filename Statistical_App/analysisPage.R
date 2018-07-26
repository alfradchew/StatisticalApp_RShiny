#The main function list
lst <- list()
lst[[1]] <- "Basic Statistics"
lst[[2]] <- "Vizualization"
lst[[3]] <- "Principal Component Analysis (PCA)"
lst[[4]] <- "Hierachical Clustering"

#selec_var()[[3]] refer to the statistical function that is selected.
#Refer to mainControl.R for more info about select_var.
output$title1 <- renderUI({
  h1(lst[[as.numeric(selec_var()[[3]])]], class="smaller-margins")
})

############################### Take Note ############################################

# lista refers to the layout for the header of main panel
# listb refers to the layout for the content of main panel

######################################################################################

#lista[[1]] - layout format for "Basic Statistics" function
#lista[[2]] - layout format for "Vizualization" function
lista <- list()

lista[[1]] <- tagList(h3("Select sub function"),
                      selectInput("sub_function", label = NULL,
                                  choices = c("Characterization: Table" = 1,
                                              "Correlation Table" = 2,
                                              "Differential Analysis" = 3)))

lista[[2]] <- tagList(h3("Select Plot"),
                      selectInput("viz_plot", label = NULL,
                                  choices = c("Histogram" = 1,
                                              "Scatterplot" = 2,
                                              "Boxplot" = 3,
                                              "Stacked Barplot" = 4,
                                              "Correlation Plot" = 5)))

lista[[3]] <- NULL

lista[[4]] <- NULL

output$select_subfunc <- renderUI({
  selec_var()
  lista[[as.numeric(selec_var()[[3]])]]
})

#Layout for the content of main panel
output$output1 <- renderUI({
  selec_var()
  if(selec_var()[[3]] == 1){
    listb[[paste(selec_var()[[3]],input$sub_function,sep="-")]]
  }
  else{
    listb[[paste(selec_var()[[3]],input$viz_plot,sep="-")]]
  }
})


