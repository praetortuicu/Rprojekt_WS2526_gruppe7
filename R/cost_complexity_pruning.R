# cost_complexity_pruning.R

# TODO Check safety
# TODO Documentation

###     GENERICS      ###
compute_subtree_error     <- S7::new_generic("compute_subtree_error",     "tree")
find_weakest_link      <- S7::new_generic("find_weakest_link",      "tree")
cost_complexity_prune <- S7::new_generic("cost_complexity_prune", "tree")

###     HELPERS     ###

# Count leaves in subtree rooted at given node
count_subtree_leaves <- function(node) {
  if (is_leaf(node)) return(1L)
  return(count_subtree_leaves(get_left_child(node)) + count_subtree_leaves(get_right_child(node)))
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

  # split indices according to node rule
  left_idx  <- indices[X[indices, feature] <= threshold]
  right_idx <- indices[X[indices, feature] > threshold]

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
compute_collapsed_error <- function(node, root, X, y) {
  # Get the rows
  indexes <- get_node_indices(root, node, X) # TODO Implement this function
  # Mean prediction
  prediction <- mean(y[indexes])
  # Return squared error of the true values minus the mean
  return(sum((y[indexes] - prediction)^2))
}

# Compute the subtree error R(T_t)
# Sum of errors of leaves
compute_subtree_error <- function(node, root, X, y) {
  if (is_leaf(node)) return(compute_collapsed_error(node, root, X, y))
  return(
    compute_subtree_error(get_left_child(node),  root, X, y)
    + compute_subtree_error(get_right_child(node), root, X, y)
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
compute_cost_complexity_ratio <- function(node, X, y) {
  # TODO 
}


###     METHODS     ###

# Find the weakest link in the tree like in the Satz 6.19
# For each internal node t we want to compute the cost-complextiy ratio
# and find the one that minimizes it
S7::method(find_weakest_link, BinaryTree) <- function(tree, X, y) {
  nodes <- get_internal_nodes(tree@ref$root)
  min_cost_complexity_ratio <- Inf
  weakest_link <- NULL
  # iterate over internal nodes
  for (current_node in nodes) {
    node_error <- compute_collapsed_error(current_node, X, y)
    subtree_error <- compute_subtree_error(current_node, X, y)
    num_leaves <- count_subtree_leaves(current_node)
    cost_complexity_ratio <- (node_error - subtree_error) / (num_leaves - 1L)
    # track the minimum and store the argmin
    if (cost_complexity_ratio < min_cost_complexity_ratio) {
      min_cost_complexity_ratio <- cost_complexity_ratio
      weakest_link <- current_node
    }
  }

  return(list(
    node = weakest_link,
    cc_ratio = min_cost_complexity_ratio
  ))
}

# Cost complexity pruning: the whole shabang
# TODO: 
#
# Start with full tree, iteratively prune the weakest link until we have just the root.
# Important: we need to return the sequence WITH the ratios, so we need to store them as we go.
S7::method(cost_complexity_prune, BinaryTree) <- function(tree, X, y) {
  # TODO
}
