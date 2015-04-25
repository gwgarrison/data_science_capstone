
shinyServer(function(input, output) {
  
  output$value <- renderPrint({ call_predict(input$text) })
  
})

