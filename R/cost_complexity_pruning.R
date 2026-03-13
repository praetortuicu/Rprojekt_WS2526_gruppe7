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
  count_subtree_leaves(get_left_child(node)) + count_subtree_leaves(get_right_child(node))
}

# Compute the error R(t) if subtree was replaced by single leaf
compute_node_error <- function(node, X, y) {
  # TODO
}

# Compute the subtree error R(T_t)
compute_subtree_error <- function(node, X, y) {
  # TODO
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

# Compute total resubstitution error of a subtree
#
# Wrap compute_subtree_error() as an S7 method on BinaryTree
S7::method(compute_subtree_error, BinaryTree) <- function(tree, node, X, y) {
  # TODO
}

# Find the weakest link in the tree like in the Satz 6.19
# 
# For each internal node t we want to compute the cost-complextiy ratio
# and find the one that minimizes it
S7::method(find_weakest_link, BinaryTree) <- function(tree, X, y) {
  nodes <- get_internal_nodes(tree@ref$root)
  min_cost_complexity_ratio <- Inf
  weakest_link <- NULL
  # iterate over internal nodes
  for (current_node in nodes) {
    node_error <- compute_node_error(current_node, X, y)
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
