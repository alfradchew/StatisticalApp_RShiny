library(shiny)
library(shinyjs)
library(shinyBS)
options(digits = 3, error = expression(NULL))

shinyUI(fluidPage(shinyjs::useShinyjs(),
                  tags$style(type="text/css",
                             ".shiny-output-error { visibility: hidden; }",
                             ".shiny-output-error:before { visibility: hidden; }"
                  ),
  titlePanel("Statistical App"),
    sidebarLayout(
      sidebarPanel(
        fileInput('file', 'Upload Dataset', multiple = T),

        fluidRow(
          column(6, radioButtons('type', 'File Type',
                                c('xlsx' = 'xlsx', 'csv' = 'csv'),
                                selected = 'xlsx', inline = T))
        ),

        actionButton('upload', "Upload"),
        
        br(),
        br(),
        
        #Output title "Selection of Variables" when user click the "upload" button
        #(refer to mainControl.R)
        uiOutput("title"),
        
        br(),
        
        #Radio button to allow user to select all variables (refer to mainControl.R)
        uiOutput("selectAll"),
        
        br(),
        
        #Select button to allow user to select variables (refer to mainControl.R)
        uiOutput("choose_var"),
        #Select button to allow user to select which variables are categorical (refer to mainControl.R)
        uiOutput('select_cat_var'),
        #Help text for categorical variables (refer to mainControl.R)
        uiOutput('help'),
        
        hr(),
        
        #Select button to allow user to select which statistical functions to execute (refer to mainControl.R)
        uiOutput('select_func'),
        #Action button to select variables (refer to mainControl.R)
        uiOutput('select_var')
        
      ),
      
      mainPanel(
        verticalLayout(
          #Refer to analysisPage.R for more info
          uiOutput("title1"),
          div(uiOutput("select_subfunc")),
          div(uiOutput("output1")))
        )
      )
  ))