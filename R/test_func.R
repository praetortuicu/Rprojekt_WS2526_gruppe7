library(shiny)

ui <- fluidPage(
  div(
    style = "position: relative; height: 600px;",
    
    div(
      style = "
        position: absolute;
        top: 50px;
        left: 100px;
        width: 300px;
        height: 300px;
      ",
      plotOutput("plot1", height = "100%", click = "click1")
    ),
    
    div(
      style = "
        position: absolute;
        top: 200px;
        left: 450px;
        width: 400px;
        height: 200px;
      ",
      plotOutput("plot2", height = "100%", click = "click2")
    )
  )
)

server <- function(input, output, session) {
  
  # Separate Punkt-Speicher
  points1 <- reactiveVal(matrix(ncol = 2, nrow = 0))
  points2 <- reactiveVal(matrix(ncol = 2, nrow = 0))
  
  # Klick für Canvas 1
  observeEvent(input$click1, {
    new_point <- c(input$click1$x, input$click1$y)
    points1(rbind(points1(), new_point))
  })
  
  # Klick für Canvas 2
  observeEvent(input$click2, {
    new_point <- c(input$click2$x, input$click2$y)
    points2(rbind(points2(), new_point))
  })
  
  output$plot1 <- renderPlot({
    plot(0, 0, type = "n",
         xlim = c(0, 10),
         ylim = c(0, 10),
         main = "Canvas 1")
    
    if (nrow(points1()) > 0) {
      points(points1()[,1], points1()[,2],
             pch = 19, col = "red")
    }
  })
  
  output$plot2 <- renderPlot({
    plot(0, 0, type = "n",
         xlim = c(0, 10),
         ylim = c(10, 100),
         main = "Canvas 2")
    
    if (nrow(points2()) > 0) {
      points(points2()[,1], points2()[,2],
             pch = 19, col = "blue")
    }
  })
}

shinyApp(ui, server)