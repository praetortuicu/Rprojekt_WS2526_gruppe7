library(shiny)
library(ggplot2)

source("R/node.R")
source("R/binary_tree.R")

node_radius <- 20


# ------------------------------------------------
# TREE -> DATAFRAME
# ------------------------------------------------

get_all_nodes <- function(tree){
  
  root <- tree@ref$root
  
  nodes <- data.frame(
    node_id = integer(),
    parent_id = integer(),
    depth = integer(),
    is_leaf = logical()
  )
  
  counter <- 1
  
  traverse <- function(node, parent, depth){
    
    id <- counter
    counter <<- counter + 1
    
    nodes <<- rbind(nodes, data.frame(
      node_id = id,
      parent_id = parent,
      depth = depth,
      is_leaf = is_leaf(node)
    ))
    
    if(!is_leaf(node)){
      
      traverse(get_left_child(node), id, depth + 1)
      traverse(get_right_child(node), id, depth + 1)
      
    }
    
  }
  
  traverse(root, NA, 0)
  
  nodes
}


# ------------------------------------------------
# SUBTREE NODES
# ------------------------------------------------

get_subtree_nodes <- function(nodes, node_id){
  
  result <- node_id
  
  children <- nodes$node_id[nodes$parent_id == node_id]
  
  for(c in children){
    result <- c(result, get_subtree_nodes(nodes, c))
  }
  
  result
}


# ------------------------------------------------
# LAYOUT
# ------------------------------------------------

computate_node_layout <- function(tree){
  
  nodes <- get_all_nodes(tree)
  
  nodes$x <- NA_real_
  nodes$y <- NA_real_
  
  node_radius <- 20
  x_spacing <- 120
  y_spacing <- 120
  
  max_depth <- max(nodes$depth)
  
  # assign leaf x positions
  
  leaf_ids <- nodes$node_id[nodes$is_leaf]
  
  leaf_x <- seq(
    node_radius,
    by = x_spacing,
    length.out = length(leaf_ids)
  )
  
  nodes$x[nodes$is_leaf] <- leaf_x
  
  # assign internal nodes
  
  for(depth in seq(max_depth-1,0,-1)){
    
    layer <- nodes[nodes$depth == depth,]
    
    for(i in 1:nrow(layer)){
      
      id <- layer$node_id[i]
      
      children <- nodes$node_id[nodes$parent_id == id]
      
      child_x <- nodes$x[nodes$node_id %in% children]
      
      if(length(child_x) > 0){
        nodes$x[nodes$node_id == id] <- mean(child_x)
      }
      
    }
    
  }
  
  # assign y positions
  
  nodes$y <- nodes$depth * y_spacing + node_radius
  
  canvas_width <- max(nodes$x) + 200
  canvas_height <- (max_depth + 1) * y_spacing + 200
  
  list(
    nodes = nodes,
    canvas_width = canvas_width,
    canvas_height = canvas_height
  )
}


# ------------------------------------------------
# TEST TREE
# ------------------------------------------------

generate_test_tree <- function(){
  
  root <- Node(ROOT=TRUE,s_feature=1,s_value=5)
  
  n1 <- Node(s_feature=2,s_value=3)
  n2 <- Node(s_feature=3,s_value=7)
  
  l1 <- Node(leaf_value=0)
  l2 <- Node(leaf_value=1)
  
  set_left_child(root,n1)
  set_right_child(root,n2)
  
  set_left_child(n1,l1)
  set_right_child(n1,l2)
  
  tree <- BinaryTree(root=root)
  
  tree@ref$depth <- 2
  tree@ref$n_nodes <- 5
  tree@ref$n_leaves <- 3
  
  tree
}