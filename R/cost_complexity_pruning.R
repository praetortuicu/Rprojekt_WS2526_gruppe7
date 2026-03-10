# cost_complexity_pruning.R
#
# TODO Documentation like Theo

# Error in subtree with root at given node in tree
get_subtree_error <- S7::new_generic("get_subtree_error", "tree")
# Find the weakest link vgl. Satz 6.19
get_weakest_link <- S7::new_generic("get_weakest_link", "tree")
# Collapse the subtree to a single leaf
prune_node <- S7::new_generic("prune_node", "tree")
# The whole shabang
cost_complexity_prune <- S7::new_generic("cost_complexity_prune", "tree")get_subtree_error <- S7::new_generic("get_subtree_error", "tree")

# Data: X matrix, y vector

S7::method(get_subtree_error, BinaryTree) <- function(tree, node, X, y) {
  # TODO
}

S7::method(get_weakest_link, BinaryTree) <- function(tree, X, y) {
  # TODO
}

S7::method(prune_node, BinaryTree) <- function(tree, node, y) {
  # TODO
}

S7::method(cost_complexity_prune, BinaryTree) <- function(tree, X, y) {
  # TODO
}
