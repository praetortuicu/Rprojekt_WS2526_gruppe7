library(shiny)

m_offset <- c(0, 1.5)
r_offset <- c(0.5, 1.5)
l_offset <- c(-0.5, 1.5)

lin_offset <- c(0, 0)

ui <- fluidPage(
  plotOutput("plot")
)


server <- function(input, output) {
  output$plot <- renderPlot({
    plot(0, 0,
         type = "n",      # nichts zeichnen
         xlim = c(-10, 10),
         ylim = c(-10, 10),
         xlab = "X",
         ylab = "Y")
    
    points(0, 0, col = "red", pch = 19, cex = 2, main = "Root")
    points(-2, -4, col = "blue", pch = 19, cex = 2, main = "Child L")
    points(2, -4, col = "blue", pch = 19, cex = 2, main = "Child R")
    segments(0, 0, -2, -4, col = "blue", lwd = 2, main = "condition 1")
    segments(0, 0, 2, -4, col = "blue", lwd = 2, main = "condition 2")
    
    text(col = "red", x = 0, y = 0 + m_offset[2], labels = "Root", cex = 1.5)
    text(col = "blue", x = -2 + l_offset[1], y = -4 + l_offset[2] , labels = "Child L", cex = 1.5)
    text(col = "blue", x = 2 + r_offset[1], y = -4 + r_offset[2] , labels = "Child R", cex = 1.5)
    text(col = "black", x = -1 + lin_offset[1], y = -2 + lin_offset[2] , labels = "condition 1", cex = 1)
    text(col = "black", x = 1 + lin_offset[1], y = -2 + lin_offset[2] , labels = "condition 2", cex = 1)
  })
}

shinyApp(ui = ui, server = server)

