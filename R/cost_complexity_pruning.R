# cost_complexity_pruning.R

# TODO Check safety
# TODO Documentation

###     GENERICS      ###
find_weakest_link      <- S7::new_generic("find_weakest_link",      "cart")
cost_complexity_prune <- S7::new_generic("cost_complexity_prune", "cart")

###     HELPERS     ###

# Count leaves in subtree rooted at given node
count_subtree_leaves <- function(node) {
  if (is_leaf(node)) return(1L)
  return(count_subtree_leaves(get_left_child(node)) + count_subtree_leaves(get_right_child(node)))
}

# Build a leaf prediction from the target values for the requested task
compute_leaf_prediction <- function(values, type) {
  if (type == "classification") {
    return(as.integer(which.max(tabulate(values))))
  }

  mean(values)
}

# Loss for a leaf given its prediction
compute_leaf_loss <- function(values, prediction, type) {
  if (length(values) == 0) return(0)

  if (type == "classification") {
    return(sum(values != prediction))
  }

  sum((values - prediction)^2)
}

# Helper function for compute_collapsed_error
# Gets the indeces of the data rows that reach a specific node
# This is for calculating the mean value to collapse a subtree
get_node_indices <- function(root, node, X, indices = 1:nrow(X)) {
  # Start with all indexes and search for the node, in each iteration
  # we filter the indexes by the ones who actually go to the node
  if (identical(root, node)) {
    return(indices)
  }

  # If leaf but not the target node → stop
  if (is_leaf(root)) {
    return(integer(0))
  }

  feature <- root@ref$s_feature
  threshold <- root@ref$s_value

  # split indices according to node rule used during tree construction (< for left, >= for right)
  left_idx  <- indices[X[indices, feature] < threshold]
  right_idx <- indices[X[indices, feature] >= threshold]

  # search left subtree
  result <- get_node_indices(get_left_child(root), node, X, left_idx)
  if (length(result) > 0) {
    return(result)
  }

  # otherwise search right subtree
  return(get_node_indices(get_right_child(root), node, X, right_idx))
}

# Compute the error R(t) if subtree was collapsed to a single leaf
# Replace the subtree by a single node which has constant prediction for
# the subset of the data that would have reached the leaves, so we need
# to find the indexes of the rows in X which reach the node
compute_collapsed_error <- function(node, root, X, y, type) {
  # Get the rows
  indexes <- get_node_indices(root, node, X)

  if (length(indexes) == 0) return(0)
  # Mean prediction
  prediction <- compute_leaf_prediction(y[indexes], type)
  return(compute_leaf_loss(y[indexes], prediction, type))
}

# Compute the subtree error R(T_t)
# Sum of errors of leaves
compute_subtree_error <- function(node, root, X, y, type) {
  if (is_leaf(node)) return(compute_collapsed_error(node, root, X, y, type))
  return(
    compute_subtree_error(get_left_child(node),  root, X, y, type)
    + compute_subtree_error(get_right_child(node), root, X, y, type)
  )
}

# Get internal nodes as list (Helper for finding weakest link)
get_internal_nodes <- function (node, nodes = list()) {
    if (is_leaf(node)) return(nodes)

    nodes <- c(nodes, list(node))

    nodes <- get_internal_nodes(get_left_child(node), nodes)
    nodes <- get_internal_nodes(get_right_child(node), nodes)

    return(nodes)
}

# Compute the cost complexity ratio (R(t) - R(T_t))/(#T_t - 1) for a node, where
# - R(t) error if the subtree rooted at t gets replaced by leaf (make function for this)
# - R(T_t) total error in the node's subtree (make function for this)
# - #T_t number of leaves in the node's subtree (make function for this)
compute_cost_complexity_ratio <- function(node, root, X, y, type) {
  R_t   <- compute_collapsed_error(node, root, X, y, type)
  R_T_t <- compute_subtree_error(node,   root, X, y, type)
  leaves <- count_subtree_leaves(node)
  return((R_t - R_T_t) / (leaves - 1L)) # Possible zero division?
}

###     METHODS     ###

# Find the weakest link in the tree like in the Satz 6.19
# For each internal node t we want to compute the cost-complextiy ratio
# and find the one that minimizes it
S7::method(find_weakest_link, CART) <- function(cart, X, y) {
  nodes <- get_internal_nodes(cart@ref$root)
  min_cost_complexity_ratio <- Inf
  weakest_link <- NULL
  tree_type <- cart@ref$type
  if (is.null(tree_type)) tree_type <- "regression"

  # Iterate over each internal node
  for (current_node in nodes) {
    # Track minimum cost_complexity_ratio
    ratio <- compute_cost_complexity_ratio(current_node, cart@ref$root, X, y, tree_type)
    if (ratio < min_cost_complexity_ratio) {
      min_cost_complexity_ratio <- ratio
      weakest_link <- current_node
    }
  }
 
  return(list(
    node     = weakest_link,
    cc_ratio = min_cost_complexity_ratio
  ))
}
 

# Cost complexity pruning: the whole shabang
# Start with full tree, iteratively prune the weakest link until we have just the root.
# Important: we need to return the sequence WITH the ratios, so we need to store them as we go.
S7::method(cost_complexity_prune, CART) <- function(cart, X, y) {
  trees  <- list(unserialize(serialize(cart, NULL)))  # we use serialize and unserialize  as deep copy
  ratios <- c()
  tree_type <- cart@ref$type
  if (is.null(tree_type)) tree_type <- "regression"

  while (count_subtree_leaves(cart@ref$root) > 1L) {

    result <- find_weakest_link(cart, X, y)

    if (is.null(result$node)) break

    # store alpha
    ratios <- c(ratios, result$cc_ratio)

    # compute leaf value for collapsed node
    idx <- get_node_indices(cart@ref$root, result$node, X)
    if (length(idx) == 0) {
      leaf_value <- compute_leaf_prediction(y, tree_type)
    } else {
      leaf_value <- compute_leaf_prediction(y[idx], tree_type)
    }

    # prune subtree
    prune(cart, result$node, leaf_value)

    # store snapshot of pruned tree
    trees <- c(trees, list(unserialize(serialize(cart, NULL))))
  }

  return(list(trees  = trees, ratios = ratios))
}
