library(shiny)

ui <- fluidPage(
  
  h3("Datenbank-Fenster"),
  
  div(
    style = "
      border: 2px solid black;
      height: 250px;
      width: 400px;
      overflow-y: scroll;
      padding: 10px;
      background-color: #f9f9f9;
    ",
    
    tableOutput("db_table")
  )
)

server <- function(input, output) {
  
  output$db_table <- renderTable({
    mtcars
  })
}

shinyApp(ui, server)