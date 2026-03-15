library(shiny)
library(ggplot2)

source("R/node.R")
source("R/binary_tree.R")
source("R/cart.R")
source("R/cost_complexity_pruning.R")
source("R/random_forests.R")
source("R/random_forests_cart.R")
source("R/bagging.R")

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
    is_leaf = logical(),
    
    s_feature = numeric(),
    s_value = numeric()
  )
  
  counter <- 1
  
  traverse <- function(node, parent, depth){
    
    id <- counter
    counter <<- counter + 1
    
    nodes <<- rbind(nodes, data.frame(
      node_id = id,
      parent_id = parent,
      depth = depth,
      is_leaf = is_leaf(node),
      s_feature = node@ref$s_feature,
      s_value = node@ref$s_value
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


computate_node_layout <- function(tree){
  
  nodes <- get_all_nodes(tree)
  
  if(nrow(nodes) == 0) return(NULL)
  
  # safety
  if(!"is_leaf" %in% names(nodes)){
    nodes$is_leaf <- FALSE
  }
  
  nodes$x <- NA_real_
  nodes$y <- NA_real_
  
  node_radius <- 20
  x_spacing <- 120
  y_spacing <- 120
  
  max_depth <- max(nodes$depth)
  
  # ---------- LEAF POSITIONS ----------
  
  leaf_ids <- nodes$node_id[nodes$is_leaf]
  
  leaf_x <- seq(
    node_radius,
    by = x_spacing,
    length.out = length(leaf_ids)
  )
  
  nodes$x[nodes$is_leaf] <- leaf_x
  
  # ---------- INTERNAL NODES ----------
  
  if(max_depth>0){
  for(depth in seq(max_depth - 1, 0, -1)){
    
    layer <- nodes[nodes$depth == depth, ]
    
    for(i in seq_len(nrow(layer))){
      
      id <- layer$node_id[i]
      
      children <- nodes$node_id[nodes$parent_id == id]
      
      child_x <- nodes$x[nodes$node_id %in% children]
      
      child_x <- child_x[!is.na(child_x)]
      
      if(length(child_x) > 0){
        nodes$x[nodes$node_id == id] <- mean(child_x)
      }
      
    }
  }
  }else{print("Error in Tree generation no nodes has depth > 0")}
  
  # ---------- Y POSITIONS ----------
  
  nodes$y <- nodes$depth * y_spacing + node_radius
  
  # ---------- CANVAS SIZE ----------
  
  canvas_width <- max(nodes$x, na.rm = TRUE) + 200
  canvas_height <- (max_depth + 1) * y_spacing + 200
  
  list(
    nodes = nodes,
    canvas_width = canvas_width,
    canvas_height = canvas_height
  )
}


# ------------------------------------------------
# TTREE GENERATORS
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

generate_greedy_cart_tree <- function(db, max_depth, min_leaf_size){
  
  if(nrow(db) == 0){
    stop("Database empty")
  }
  
  if(!"target" %in% names(db)){
    stop("Column 'target' must exist")
  }
  
  # ---------- DETERMINE TYPE ----------
  
  if(is.numeric(db$target)){
    
    type <- "regression"
    y <- as.numeric(db$target)
    
  } else if(is.logical(db$target) || is.integer(db$target) || is.factor(db$target)){
    
    type <- "classification"
    
    if(is.logical(db$target)){
      y <- as.integer(db$target) + 1L
    } else if(is.factor(db$target)){
      y <- as.integer(db$target)
    } else {
      y <- as.integer(db$target)
    }
    
  } else {
    stop("target must be numeric (regression) or logical/integer/factor (classification)")
  }
  
  # ---------- FEATURES ----------
  
  feature_cols <- setdiff(names(db), c("Entry_ID", "target"))
  
  if(length(feature_cols) == 0){
    stop("No feature columns available")
  }
  
  X <- as.matrix(db[, feature_cols, drop = FALSE])
  
  # ---------- CART ----------
  
  cart <- CART(
    root = NULL,
    type = type,
    max_depth = max_depth,
    min_leaf_size = min_leaf_size
  )
  
  fit(cart, X, y)
  
  cart@ref$n_leaves <- count_leaves(cart)
  cart@ref$depth <- get_depth(cart)
  
  return(cart)
}

generate_pruned_cart_tree <- function(db, max_depth, min_leaf_size, alpha_index = 1){
  
  if(nrow(db) == 0) stop("Database empty")
  if(!"target" %in% names(db)) stop("target column missing")
  
  # ---------- FEATURES ----------
  feature_cols <- setdiff(names(db), c("Entry_ID","target"))
  X <- as.matrix(db[, feature_cols, drop = FALSE])
  
  # ---------- TARGET ----------
  if(is.numeric(db$target)){
    type <- "regression"
    y <- as.numeric(db$target)
  } else {
    type <- "classification"
    y <- as.integer(as.factor(db$target))
  }
  
  # ---------- TRAIN FULL TREE ----------
  cart <- CART(
    root = NULL,
    type = type,
    max_depth = max_depth,
    min_leaf_size = min_leaf_size
  )
  
  fit(cart, X, y)
  
  # ---------- PRUNE ----------
  pruned <- cost_complexity_prune(cart, X, y)
  
  trees <- pruned$trees
  
  # choose tree from pruning path
  alpha_index <- min(alpha_index, length(trees))
  
  tree <- trees[[alpha_index]]
  
  tree@ref$depth <- get_depth(tree)
  tree@ref$n_leaves <- count_leaves(tree)
  
  return(tree)
}

generate_random_forest <- function(db, B, max_depth, min_leaf_size){
  
  if(nrow(db) == 0){
    stop("Database empty")
  }
  
  if(!"target" %in% names(db)){
    stop("Column 'target' must exist")
  }
  
  # ---------- TYPE ----------
  if(is.numeric(db$target)){
    type <- "regression"
    y <- as.numeric(db$target)
  } else {
    type <- "classification"
    y <- as.integer(as.factor(db$target))
  }
  
  # ---------- FEATURES ----------
  feature_cols <- setdiff(names(db), c("Entry_ID","target"))
  
  if(length(feature_cols) == 0){
    stop("No feature columns available")
  }
  
  X <- as.matrix(db[, feature_cols, drop=FALSE])
  
  # ---------- FOREST ----------
  rf <- RandomForest(
    type = type,
    B = B,
    max_depth = max_depth,
    min_leaf_size = min_leaf_size
  )
  
  fit_rf(rf, X, y)
  
  return(rf)
}

generate_bagging_forest <- function(db, B, max_depth, min_leaf_size){
  
  if(nrow(db) == 0){
    stop("Database empty")
  }
  
  if(!"target" %in% names(db)){
    stop("Column 'target' must exist")
  }
  
  # ---------- TYPE ----------
  if(is.numeric(db$target)){
    type <- "regression"
    y <- as.numeric(db$target)
  } else {
    type <- "classification"
    y <- as.integer(as.factor(db$target))
  }
  
  # ---------- FEATURES ----------
  feature_cols <- setdiff(names(db), c("Entry_ID","target"))
  
  if(length(feature_cols) == 0){
    stop("No feature columns available")
  }
  
  X <- as.matrix(db[, feature_cols, drop = FALSE])
  
  # ---------- BAGGING ----------
  bag <- Bagging(
    type = type,
    B = B,
    max_depth = max_depth,
    min_leaf_size = min_leaf_size
  )
  
  fit_bag(bag, X, y)
  
  return(bag)
}