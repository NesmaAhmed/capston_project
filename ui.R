#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel(" Nature Language Processing"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      textInput("txt", label = h3("Input"), value = ""),
      h6(em("dont enter stopwords, Punctuation, number")),
      
      submitButton("OK")
    ),
    mainPanel(
      h3("using first sample:"),
      verbatimTextOutput("sample1_next"),
      
      h3("using second sample:"),
      verbatimTextOutput("sample2_next"),
    
      h3("using third sample:"),
      verbatimTextOutput("sample3_next")
      
    )
  )
))
