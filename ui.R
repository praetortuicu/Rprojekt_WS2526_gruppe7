library(shiny)

ui <- fluidPage(
  
  tags$h2("Another Random Forest App", style="text-align:center;"),
  
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
      
      actionButton("draw_tree","Generate Tree",
                   style="width:100%; height:40px; margin-bottom:10px;"),
      
      
      #open dropdown to choose algo and display selected algo as text on button
      selectInput(
        "choose_algo",
        "Choose Algorithm",
        choices = c("Choose Algorithm", "Greedy-Algorithm", "Bagging", "Random Forest", "Boosting"),
        width = "100%",
      ),
      
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
        
        actionButton("add_data","Add Data Column", style="width:50%;"),
        actionButton("add_entry","Add Entry", style="width:50%;")
      ),
      
      actionButton("generate_data","Generate Random Dataset", style="width:100%;")
    )
  ),
  
  
  # ---------- JS CANVAS ----------
  # ---------- JS CANVAS ----------
  tags$script(HTML("

const canvas = document.getElementById('canvas1');
const ctx = canvas.getContext('2d');

let scale = 1;
let originX = 0;
let originY = 0;

let isDragging = false;
let startX;
let startY;

let treeNodes = [];

function setStartPosition(){

  const viewWidth = 800;
  const canvasWidth = canvas.width;

  originX = (viewWidth - canvasWidth) / 2;
  originY = 0;

}

setStartPosition();

function draw(){

  ctx.setTransform(1,0,0,1,0,0);
  ctx.clearRect(0,0,canvas.width,canvas.height);

  ctx.setTransform(scale,0,0,scale,originX,originY);

  drawGrid();
  drawTree();

}

function drawGrid(){

  ctx.strokeStyle = '#cccccc';

  for(let x=0; x<canvas.width; x+=200){
    ctx.beginPath();
    ctx.moveTo(x,0);
    ctx.lineTo(x,canvas.height);
    ctx.stroke();
  }

  for(let y=0; y<canvas.height; y+=200){
    ctx.beginPath();
    ctx.moveTo(0,y);
    ctx.lineTo(canvas.width,y);
    ctx.stroke();
  }

}

function drawTree(){

  if(treeNodes.length === 0) return;

  ctx.strokeStyle = '#000000';
  ctx.fillStyle = '#ffffff';

  treeNodes.forEach(node => {

    if(node.parent_id !== null){

      const parent = treeNodes.find(n => n.node_id === node.parent_id);

      if(parent){
        ctx.beginPath();
        ctx.moveTo(parent.x, parent.y);
        ctx.lineTo(node.x, node.y);
        ctx.stroke();
      }

    }

  });

  treeNodes.forEach(node => {

    ctx.beginPath();
    ctx.arc(node.x, node.y, 20, 0, Math.PI * 2);
    ctx.fill();
    ctx.stroke();

  });

}

draw();

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

Shiny.addCustomMessageHandler('draw_tree', function(data){

  treeNodes = data.nodes;

  canvas.width = data.width;
  canvas.height = data.height;

  scale = 1;
  setStartPosition();

  draw();

});

"))
)