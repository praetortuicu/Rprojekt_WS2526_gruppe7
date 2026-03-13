# cost_complexity_pruning.R

###     GENERICS      ###
get_subtree_error     <- S7::new_generic("get_subtree_error",     "tree")
get_weakest_link      <- S7::new_generic("get_weakest_link",      "tree")
cost_complexity_prune <- S7::new_generic("cost_complexity_prune", "tree")


###     HELPERS     ###

# Count leaves in subtree rooted at given node
# TODO Check safety
# TODO Documentation
count_subtree_leaves <- function(node) {
  if (is_leaf(node)) return(1L)
  count_subtree_leaves(get_left_child(node)) + count_subtree_leaves(get_left_right(node))
}

# Compute the error R(t) if subtree was replaced by single leaf
compute_node_error <- function(node, root, X, y) {
  # TODO
}

# Compute the subtree error R(T_t)
sum_leaf_errors <- function(node, root, X, y) {
  # TODO
}

# Get internal nodes as list (Helper for finding weakest link)
get_internal_nodes <- function (node, nodes = list()) {
    if (is_leaf(node)) return(nodes)

    nodes <- c(nodes, list(node))

    nodes <- collect_internal_nodes(get_left_child(node), nodes)
    nodes <- collect_internal_nodes(get_right_child(node), nodes)

    return(nodes)
}

# Compute the cost complexity ratio (R(t) - R(T_t))/(#T_t - 1) for a node, where
# - R(t) error if the subtree rooted at t gets replaced by leaf (make function for this)
# - R(T_t) total error in the node's subtree (make function for this)
# - #T_t number of leaves in the node's subtree (make function for this)
compute_cost_complexity_ratio <- function(node, root, X, y) {
  # TODO 
}


###     METHODS     ###

# Compute total resubstitution error of a subtree
#
# Wrap sum_leaf_errors() as an S7 method on BinaryTree
S7::method(get_subtree_error, BinaryTree) <- function(tree, node, X, y) {
  # TODO
}

# Find the weakest link in the tree like in the Satz 6.19
# 
# For each internal node t we want to compute the cost-complextiy ratio
# and find the one that minimizes it
S7::method(find_weakest_link, BinaryTree) <- function(tree, X, y) {
  # TODO
  # iterate over internal nodes (list)
  # compute cost complexity ratio for each node and find the minimum
  # return that node
}

# Cost complexity pruning: the whole shabang
# TODO: 
#
# Start with full tree, iteratively prune the weakest link until we have just the root.
# Important: we need to return the sequence WITH the ratios, so we need to store them as we go.
S7::method(cost_complexity_prune, BinaryTree) <- function(tree, X, y) {
  # TODO
}
