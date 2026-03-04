library(shiny)

ui <- fluidPage(
  plotOutput(
    "plot",
    click = "plot_click",
    hover = hoverOpts("plot_hover", delay = 0)
  ),
  verbatimTextOutput("info")
)

server <- function(input, output, session) {
  
  point <- reactiveVal(c(5, 5))
  dragging <- reactiveVal(FALSE)
  
  # Start / Stop dragging when clicking near the point
  observeEvent(input$plot_click, {
    
    p <- point()
    d <- sqrt((input$plot_click$x - p[1])^2 +
                (input$plot_click$y - p[2])^2)
    
    if (d < 0.5) {
      dragging(!dragging())   # toggle dragging
    }
  })
  
  # Move point while dragging
  observe({
    if (dragging() && !is.null(input$plot_hover)) {
      point(c(input$plot_hover$x, input$plot_hover$y))
    }
  })
  
  output$plot <- renderPlot({
    plot(0, 0,
         type = "n",
         xlim = c(0, 10),
         ylim = c(0, 10))
    
    p <- point()
    
    points(p[1], p[2],
           pch = 19,
           col = if (dragging()) "blue" else "red",
           cex = 2)
  })
  
  output$info <- renderPrint({
    list(
      position = point(),
      dragging = dragging()
    )
  })
}

shinyApp(ui, server)