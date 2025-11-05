

library(shiny)
ui<-fluidPage('Hello World')
server<-function(input,output){}
shinyApp(ui = ui, server = server)



library(shiny)
ui<-fluidPage(
  # Input Functions
  sliderInput(inputId = "num", #ID name for the input
              label = "Choose a number", # label above the input
              value = 25, min = 1, max = 100 # values for the slider
              )
  # Output Functions
)
server<-function(input,output){}
shinyApp(ui = ui, server = server)

