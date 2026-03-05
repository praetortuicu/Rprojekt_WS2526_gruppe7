library(shiny)

ui <- fluidPage(
  
  tags$h2("Canvas Test", style="text-align:center;"),
  
  tags$div(
    style="position:relative; width:100%; height:900px;",
    
    # ---------- CANVAS ----------
    tags$div(
      style="
        width:800px;
        height:400px;
        border:2px solid black;
        overflow:hidden;
        margin:auto;
        margin-top:20px;
      ",
      
      tags$canvas(
        id="canvas1",
        width="6000",
        height="4000"
      )
    ),
    
    
    # ---------- LINKS UNTER CANVAS ----------
    tags$div(
      style="
        position:absolute;
        top:480px;
        left:calc(50% - 420px);
        width:380px;
      ",
      
      actionButton("draw_tree","Draw on Canvas",
                   style="width:100%; height:40px; margin-bottom:10px;"),
      
      actionButton("choose_algo","Choose Algorithm",
                   style="width:100%; height:40px; margin-bottom:20px;"),
      
      tags$div(textOutput("selected_algo"), style="font-size:18px;"),
      tags$div(textOutput("depth"), style="font-size:18px;"),
      tags$div(textOutput("time"), style="font-size:18px;"),
      tags$div(textOutput("leafs"), style="font-size:18px;")
    ),
    
    
    # ---------- RECHTS UNTER CANVAS ----------
    tags$div(
      style="
        position:absolute;
        top:480px;
        left:calc(50% + 40px);
        width:380px;
      ",
      
      tags$div(
        style="
          height:300px;
          border:2px solid black;
          overflow:auto;
          margin-bottom:10px;
        ",
        tableOutput("database_table")
      ),
      
      tags$div(
        style="display:flex; gap:10px; margin-bottom:10px;",
        
        actionButton("add_data","Add Data", style="width:50%;"),
        actionButton("add_entry","Add Entry", style="width:50%;")
      ),
      
      actionButton("generate_data","Generate Data", style="width:100%;")
    )
  ),
  
  
  # ---------- JAVASCRIPT CANVAS ----------
  tags$script(HTML("
  
const canvas = document.getElementById('canvas1');
const ctx = canvas.getContext('2d');

let scale = 1;
let originX = 0;
let originY = 0;

let isDragging = false;
let startX;
let startY;


/* ---- STARTPOSITION (OBEN MITTIG) ---- */

function setStartPosition(){

  const viewWidth = 800;
  const canvasWidth = canvas.width;

  originX = (viewWidth - canvasWidth) / 2;
  originY = 0;

}

setStartPosition();


/* ---- ZEICHNEN ---- */

function draw(){

  ctx.setTransform(1,0,0,1,0,0);
  ctx.clearRect(0,0,canvas.width,canvas.height);

  ctx.setTransform(scale,0,0,scale,originX,originY);

  ctx.strokeStyle = '#cccccc';

  for(let x=0;x<6000;x+=200){
    ctx.beginPath();
    ctx.moveTo(x,0);
    ctx.lineTo(x,4000);
    ctx.stroke();
  }

  for(let y=0;y<4000;y+=200){
    ctx.beginPath();
    ctx.moveTo(0,y);
    ctx.lineTo(6000,y);
    ctx.stroke();
  }

}

draw();


/* ---- PAN ---- */

canvas.addEventListener('mousedown', function(e){

  isDragging = true;
  startX = e.clientX;
  startY = e.clientY;

});

window.addEventListener('mouseup', function(){

  isDragging = false;

});

window.addEventListener('mousemove', function(e){

  if(!isDragging) return;

  originX += (e.clientX - startX);
  originY += (e.clientY - startY);

  startX = e.clientX;
  startY = e.clientY;

  draw();

});


/* ---- ZOOM ---- */

canvas.addEventListener('wheel', function(e){

  e.preventDefault();

  const zoom = e.deltaY < 0 ? 1.1 : 0.9;

  const mouseX = e.offsetX;
  const mouseY = e.offsetY;

  originX = mouseX - zoom * (mouseX - originX);
  originY = mouseY - zoom * (mouseY - originY);

  scale *= zoom;

  draw();

});

  "))
)

server <- function(input, output, session){
  
  
  # ---------- DUMMY DATENBANK ----------
  
  db <- reactiveVal(
    data.frame(
      Node=c("A","B","C"),
      Value=c(10,20,30)
    )
  )
  
  
  output$database_table <- renderTable({
    db()
  })
  
  
  # ---------- DUMMY OUTPUT ----------
  
  output$selected_algo <- renderText({
    paste("Algorithm: DummyAlgo")
  })
  
  output$depth <- renderText({
    paste("Depth:", 4)
  })
  
  output$time <- renderText({
    paste("Time:", "0.01s")
  })
  
  output$leafs <- renderText({
    paste("Leafs:", 12)
  })
  
  
  # ---------- BUTTON DUMMYS ----------
  
  observeEvent(input$add_entry,{
    
    new <- db()
    new <- rbind(new, data.frame(Node="X",Value=sample(1:100,1)))
    
    db(new)
    
  })
  
  
  observeEvent(input$generate_data,{
    
    db(
      data.frame(
        Node=LETTERS[1:10],
        Value=sample(1:100,10)
      )
    )
    
  })
  
}

shinyApp(ui, server)