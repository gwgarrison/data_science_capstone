
shinyUI(fluidPage(
  
  # Copy the line below to make a text input box
  textInput("text", label = h3("Text input"), value = ""),
  #br(),
  #actionButton("action",label="Predict Text"),
  hr(),
  h3("Prediction"),
  fluidRow(column(5, verbatimTextOutput("value")))
  
))

