#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source("predict.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  dataInput_sample1 <- reactive({
    txt =clean_text(input$txt)
    predict=predict.data1(txt)
  })
  
  dataInput_sample2 <- reactive({
    txt =clean_text(input$txt)
    predict=predict.data2(txt)
  })
  
  dataInput_sample3 <- reactive({
    txt =clean_text(input$txt)
    predict=predict.data3(txt)
  })
  
  output$sample1_next <- renderText({
    dataInput_sample1()
  })
  output$sample2_next  <- renderText({
    
    dataInput_sample2() 
  })
  output$sample3_next  <- renderText({
    
    dataInput_sample3() 
  })
  
})
