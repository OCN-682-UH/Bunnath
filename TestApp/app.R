library(shiny)
library(tidyverse)
#library(rsconnect)


#everything in ui need comman for seperation
ui<-fluidPage(
  # Input Functions
  sliderInput(inputId = "num", #ID name for the input
              label = "Choose a number", # label above the input
              value = 25, min = 1, max = 100 # values for the slider
              ),
  textInput(inputId = "title", #new Id is title
            label = "Write a title",
            value = "Histogram of Random Normal Values"),
  
  # Output Functions
  plotOutput("hist"),
  verbatimTextOutput("stats")
)

server<-function(input,output){
  
  data<- reactive({
    tibble(x = rnorm(input$num)) # 100 random normal points
  })
  
  output$hist <- renderPlot({
    ggplot(data(), aes(x = x))+ 
      geom_histogram() +
      labs(title = input$title) #Add a new title
  })
  
  output$stats<- renderPrint({
    summary(data())
  })
}

#everything in server is written just like R scripts
shinyApp(ui = ui, server = server)

