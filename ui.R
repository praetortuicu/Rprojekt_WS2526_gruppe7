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
        choices = c("Choose Algorithm", "Greedy-Algorithm", "Bagging", "Random Forest", "Boosting", "Test"),
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
  tags$script(HTML("
Shiny.addCustomMessageHandler('draw_tree', function(data){

  const canvas = document.getElementById('tree_canvas');
  const ctx = canvas.getContext('2d');

  canvas.width = data.width;
  canvas.height = data.height;

  const nodes = data.nodes;
  const radius = 20;

  ctx.clearRect(0,0,canvas.width,canvas.height);

  // -------- Build lookup table --------
  const nodeMap = {};
  nodes.forEach(n => {
    nodeMap[n.id] = n;
  });

  // -------- Draw Edges --------
  nodes.forEach(node => {

    if(node.parent_id && nodeMap[node.parent_id]){

      const parent = nodeMap[node.parent_id];

      ctx.beginPath();
      ctx.moveTo(parent.x, parent.y);
      ctx.lineTo(node.x, node.y);
      ctx.strokeStyle = '#444';
      ctx.lineWidth = 2;
      ctx.stroke();

    }

  });

  // -------- Draw Nodes --------
  nodes.forEach(node => {

    let color;

    if(node.depth === 0){
      color = '#FFD700';   // root yellow
    }
    else if(node.is_leaf){
      color = '#4CAF50';   // leaf green
    }
    else{
      color = '#4A90E2';   // internal node blue
    }

    ctx.beginPath();
    ctx.arc(node.x, node.y, radius, 0, 2*Math.PI);
    ctx.fillStyle = color;
    ctx.fill();
    ctx.strokeStyle = '#333';
    ctx.lineWidth = 2;
    ctx.stroke();

  });

  // -------- Draw Node IDs --------
  ctx.fillStyle = 'black';
  ctx.font = '14px Arial';
  ctx.textAlign = 'center';
  ctx.textBaseline = 'middle';

  nodes.forEach(node => {

    ctx.fillText(node.id, node.x, node.y);

  });

});
"))
)