library(shiny)

ui <- fluidPage(
  titlePanel("Hello World!"),
  textInput("c", "Enter Text:"),
  actionButton("clickme", "Click Me!"),
  textOutput("result")

)

server <- function(input, output){
  output$c <- renderText("Hello World!")
  observeEvent(input$clickme, {
    output$result <- renderText(paste("Congrats you learned inputs dear ", input$c))
  })
}

shinyApp(ui = ui, server = server)

