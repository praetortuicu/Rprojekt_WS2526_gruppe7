# cost_complexity_pruning.R

###     GENERICS      ###
get_subtree_error     <- S7::new_generic("get_subtree_error",     "tree")
get_weakest_link      <- S7::new_generic("get_weakest_link",      "tree")
prune_node            <- S7::new_generic("prune_node",            "tree")
cost_complexity_prune <- S7::new_generic("cost_complexity_prune", "tree")


###     HELPERS     ###

# Count leaves in subtree rooted at given node
count_leaves <- function(node) {
  # TODO
}

# Compute the error R(t) if subtree was replaced by single leaf
compute_node_error <- function(node, root, X, y) {
  # TODO
}

# Compute the subtree error R(T_t)
sum_leaf_errors <- function(node, root, X, y) {
  # TODO
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
S7::method(get_weakest_link, BinaryTree) <- function(tree, X, y) {
  # TODO
}

# Cost complexity pruning: the whole shabang
# TODO: 
#
# Start with full tree, iteratively prune the weakest link until we have just the root.
# Important: we need to return the sequence WITH the ratios, so we need to store them as we go.
S7::method(cost_complexity_prune, BinaryTree) <- function(tree, X, y) {
  # TODO
}
