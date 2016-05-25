#setwd("C:/Users/anubissmile/Desktop/R_test-shiny/App-1")

library(shiny)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("MID-TERM SCORE"),
  
  # Sidebar with controls to select a dataset and specify the
  # number of observations to view
  sidebarLayout(
    sidebarPanel(
      
    selectInput("fieldset", "Choose a data:", 
              choices = c("mid", "quiz", "class", "Total")),
      textInput("stdID1","Please insert your ID","55037163"),
      textInput("pwd","Please insert your password","63"),
      sliderInput("finalterm", "Please insert your final term score. (40)", 
                  min=0, max=40, value=0
                  ),
      hr()
    ),
    
    # Show a summary of the dataset and an HTML table with the 
    # requested number of observations
    mainPanel(
      plotOutput("ScorePlot"),
      plotOutput("diversity"),
      #tableOutput("tabOut"),
      # verbatimTextOutput("summary2"),
      verbatimTextOutput("summary"),
      verbatimTextOutput("meanClass"),
      verbatimTextOutput("meanQuiz"),
      verbatimTextOutput("meanMid"),
      verbatimTextOutput("greatSummary")
    )
  )
))