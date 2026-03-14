library(shiny)

ui <- fluidPage(
  
  tags$h2("Another Random Forest App", style="text-align:center;"),
  
  tags$div(
    style="position:relative; width:100%; height:1000px;",
    
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
        choices = c("Choose Algorithm", "Greedy-Algorithm", "Cost Complexity Pruning", "Bagging", "Random Forest", "Boosting"),
        width = "100%",
      ),
      
      tags$div(textOutput("selected_algo"), style="font-size:18px;"),
      tags$div(textOutput("depth"), style="font-size:18px;"),
      tags$div(textOutput("time"), style="font-size:18px;"),
      tags$div(textOutput("leafs"), style="font-size:18px;")
    ),
    
    # below, conditional on select input
    
    # if input$choose_algo == "Greedy-Algorithm
    # have textinputs for max_depth and min_leaf_size
    conditionalPanel(
      condition = "input.choose_algo == 'Greedy-Algorithm'",
      div(
        style="position:absolute; top:750px; left:10px",
        column(width = 5, numericInput("max_depth", "Max Depth", value = 5, min= 1, step = 1, width = "80%")),
        column(width = 5, numericInput("min_leaf_size", "Min Leaf Size", value = 5, min = 1, step = 1, width = "80%"))
      )
    ),
    
    conditionalPanel(
      condition = "input.choose_algo == 'Cost Complexity Pruning'",
      div(
        style="position:absolute; top:750px; left:10px",
        column(width = 5, numericInput("max_depth", "Max Depth", value = 5, min= 1, step = 1, width = "80%")),
        column(width = 5, numericInput("min_leaf_size", "Min Leaf Size", value = 5, min = 1, step = 1, width = "80%")),
        sliderInput(
          "prune_level",
          "Pruning level",
          min = 1,
          max = 20,
          value = 1
        )
      )
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
      
      actionButton("generate_data","Generate Random Dataset", style="width:100%;"),
      
      actionButton("clear_data", "Clear Current Dataset", style="width:100%; margin-top:10px")
    )
  ),
  

  # ---------- JS CANVAS ----------
  tags$script(HTML("

const canvas = document.getElementById('canvas1');
const ctx = canvas.getContext('2d');

const NODE_RADIUS = 20;

let nodes = [];

let scale = 1;
let offsetX = 0;
let offsetY = 0;

let dragging = false;
let dragStartX = 0;
let dragStartY = 0;

let hoveredNode = null;



// ---------- WORLD → SCREEN ----------

function worldToScreen(x,y){

  return {
    x: x * scale + offsetX,
    y: y * scale + offsetY
  }

}

function screenToWorld(x,y){

  return {
    x: (x - offsetX) / scale,
    y: (y - offsetY) / scale
  }

}



// ---------- DRAW ----------

function draw(){

  ctx.setTransform(1,0,0,1,0,0);
  ctx.clearRect(0,0,canvas.width,canvas.height);

  drawEdges();
  drawNodes();
  drawTooltip();

}



// ---------- EDGES ----------

function drawEdges(){

  ctx.lineWidth = 2;
  ctx.strokeStyle = '#444';

  nodes.forEach(node => {

    if(node.parent_id === null) return;

    const parent = nodes.find(n => n.id === node.parent_id);

    if(!parent) return;

    const p = worldToScreen(parent.x,parent.y);
    const c = worldToScreen(node.x,node.y);

    ctx.beginPath();
    ctx.moveTo(p.x,p.y);
    ctx.lineTo(c.x,c.y);
    ctx.stroke();

  });

}



// ---------- NODES ----------

function drawNodes(){

  nodes.forEach(node => {

    const p = worldToScreen(node.x,node.y);

    let color;

    if(node.depth === 0){
      color = '#e53935';        // ROOT
    }
    else if(node.is_leaf){
      color = '#43a047';        // LEAF
    }
    else{
      color = '#1e88e5';        // INTERNAL
    }

    ctx.fillStyle = color;
    ctx.strokeStyle = '#222';

    ctx.beginPath();
    ctx.arc(p.x,p.y,NODE_RADIUS*scale,0,Math.PI*2);
    ctx.fill();
    ctx.stroke();


    // label

    ctx.fillStyle = '#000';
    ctx.font = (12*scale)+'px Arial';
    ctx.textAlign = 'center';
    ctx.textBaseline = 'middle';

    ctx.fillText(node.id,p.x,p.y);

  });

}



// ---------- TOOLTIP ----------

function drawTooltip(){

  if(!hoveredNode) return;
  
  const text = 
    'Node ' + hoveredNode.id +
    ' | depth: ' + hoveredNode.depth +
    ' Split Condition: ' + hoveredNode.s_feature + ' <> ' + hoveredNode.s_value;

  ctx.setTransform(1,0,0,1,0,0);

  ctx.font = '14px Arial';
  ctx.textAlign = 'left';

  const padding = 8;
  const width = ctx.measureText(text).width + padding*2;
  const height = 32;

  const x = 20;
  const y = 20;

  ctx.fillStyle = 'rgba(255,255,255,0.9)';
  ctx.fillRect(x,y,width,height);

  ctx.strokeStyle = '#333';
  ctx.strokeRect(x,y,width,height);

  ctx.fillStyle = '#000';
  ctx.textBaseline = 'middle';

  ctx.fillText(text,x+padding,y+height/2);

}



// ---------- HOVER DETECTION ----------

canvas.addEventListener('mousemove',function(e){

  const pos = screenToWorld(e.offsetX,e.offsetY);

  hoveredNode = null;

  for(const node of nodes){

    const dx = pos.x - node.x;
    const dy = pos.y - node.y;

    const dist = Math.sqrt(dx*dx + dy*dy);

    if(dist < NODE_RADIUS){

      hoveredNode = node;
      break;

    }

  }

  draw();

});



// ---------- PAN ----------

canvas.addEventListener('mousedown',function(e){

  dragging = true;

  dragStartX = e.clientX;
  dragStartY = e.clientY;

});

window.addEventListener('mouseup',function(){

  dragging = false;

});

window.addEventListener('mousemove',function(e){

  if(!dragging) return;

  offsetX += e.clientX - dragStartX;
  offsetY += e.clientY - dragStartY;

  dragStartX = e.clientX;
  dragStartY = e.clientY;

  draw();

});



// ---------- ZOOM ----------

canvas.addEventListener('wheel',function(e){

  e.preventDefault();

  const zoom = e.deltaY < 0 ? 1.1 : 0.9;

  const mouse = screenToWorld(e.offsetX,e.offsetY);

  scale *= zoom;

  offsetX = e.offsetX - mouse.x * scale;
  offsetY = e.offsetY - mouse.y * scale;

  draw();

});



// ---------- CAMERA ----------

function centerOnRoot(){

  const root = nodes.find(n => n.depth === 0);

  if(!root) return;

  scale = 1;

  offsetX = canvas.width/2 - root.x;
  offsetY = canvas.height/2 - root.y;

}



// ---------- RECEIVE DATA FROM SHINY ----------

function dfToRows(df){

  if(Array.isArray(df)) return df;

  const rows = [];
  const n = df.id.length;

  for(let i = 0; i < n; i++){

    rows.push({
      id: df.id[i],
      parent_id: df.parent_id[i],
      x: df.x[i],
      y: df.y[i],
      depth: df.depth[i],
      is_leaf: df.is_leaf[i],
      s_feature: df.s_feature[i],
      s_value: df.s_value[i]
    });

  }

  return rows;

}

Shiny.addCustomMessageHandler('draw_tree',function(data){

  nodes = dfToRows(data.nodes);

  console.log('tree received', data);

  canvas.width = data.width;
  canvas.height = data.height;

  centerOnRoot();

  draw();

});



draw();

"))
)