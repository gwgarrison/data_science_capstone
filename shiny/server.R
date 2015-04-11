
shinyServer(function(input, output) {
  
  # You can access the value of the widget with input$text, e.g.
#  output$value <- renderPrint({ input$text })
  output$value <- renderPrint({ call_predict(input$text) })
#  output$value <- renderPrint({ predict_next_word(input$text) })
  
})

