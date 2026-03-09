# global.R
library(shiny)
library(ggplot2)

generate_alpha_ids <- function(n) {
  
  ids <- c()
  i <- 1
  
  while(length(ids) < n) {
    
    id <- ""
    x <- i
    
    while(x > 0) {
      x <- x - 1
      id <- paste0(LETTERS[(x %% 26) + 1], id)
      x <- x %/% 26
    }
    
    ids <- c(ids, id)
    i <- i + 1
  }
  
  return(ids)
}

# --------Node Positions--------------
source("R/node.R")
source("R/binary_tree.R")

#give nodes x and y properties for visualization
x <- S7::new_property(class = S7::class_double, default = NA_real_)
y <- S7::new_property(class = S7::class_double, default = NA_real_)


#Parameters
node_radius <- 20


computate_node_layout <- function(BinaryTree) {
  #find amount of leafs and nodes per layer to determine spacing
  nodes <- BinaryTree@ref$nodes
  leafs <- BinaryTree@ref$leafs
  d <- BinaryTree@ref$depth
  
  #calc all leafs behind each node to determine width of subtree
  nodes$leafs_behind <- sapply(nodes$node_id, function(node_id) {
    sum(leafs$node_id %in% get_subtree_nodes(BinaryTree, node_id))
  })
  
  #assign x-spacing based on max width and amount of leafs
  x_spacing <- 100
  canvas_width <- x_spacing * max(nodes$leafs_behind)
  
  #assign y-spacing based on max depth and amount of layers
  y_spacing <- 100
  canvas_height <- y_spacing * d
  
  #assign x and y coordinates to each node based on amount of leafs behind and depth
  #starting with deepest leaf starting on left and adjusting if needed by parents and moving upwards to root, assigning x-coordinates based on mean of child nodes and y-coordinates based on depth
  layers <- split(nodes, nodes$depth)
  for(i in seq(d, 0, by=-1)) {
    layer <- layers[[as.character(i)]]
    
    if(i == d) {
      #assign x-coordinates to leafs starting from left
      layer$x <- seq(node_radius, canvas_width - node_radius, length.out=nrow(layer))
    } else {
      #assign x-coordinates to non-leafs based on mean of child nodes
      for(j in 1:nrow(layer)) {
        node_id <- layer$node_id[j]
        child_nodes <- get_child_nodes(BinaryTree, node_id)
        if(length(child_nodes) > 0) {
          child_x <- nodes$x[nodes$node_id %in% child_nodes]
          layer$x[j] <- mean(child_x)
        } else {
          #if no child nodes, assign x-coordinate based on amount of leafs behind
          layer$x[j] <- (sum(nodes$leafs_behind[nodes$node_id < node_id]) + nodes$leafs_behind[nodes$node_id == node_id] / 2) * x_spacing
        }
      }
    }
    
    #assign y-coordinates based on depth
    layer$y <- (d - i) * y_spacing + node_radius
    
    #update nodes with new coordinates
    nodes$x[nodes$node_id %in% layer$node_id] <- layer$x
    nodes$y[nodes$node_id %in% layer$node_id] <- layer$y
    
  }
  ret <-list(
    nodes = nodes,
    canvas_width = canvas_width,
    canvas_height = canvas_height
  )
  return (ret)
}

#TEMP - test
generate_test_tree <- function(db) {
  tree <- BinaryTree()
  
  # Manually create a simple binary tree structure
  root <- Node(node_id = "A", depth = 0)
  left_child <- Node(node_id = "B", depth = 1)
  right_child <- Node(node_id = "C", depth = 1)
  left_left_child <- Node(node_id = "D", depth = 2)
  left_right_child <- Node(node_id = "E", depth = 2)
  
  # Build the tree
  root$set_left_child(left_child)
  root$set_right_child(right_child)
  left_child$set_left_child(left_left_child)
  left_child$set_right_child(left_right_child)
  
  # Add nodes to the tree's reference list
  tree@ref$nodes <- rbind(tree@ref$nodes, data.frame(node_id = c("A", "B", "C", "D", "E"), depth = c(0, 1, 1, 2, 2)))
  
  return(tree)
}