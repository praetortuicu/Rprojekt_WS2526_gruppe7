# cost_complexity_pruning.R

# Error in subtree with root at given node in tree
get_subtree_error <- S7::new_generic("get_subtree_error", "tree")
# Find the weakest link vgl. Satz 6.19
get_weakest_link <- S7::new_generic("get_weakest_link", "tree")
# Collapse the subtree to a single leaf
prune_node <- S7::new_generic("prune_node", "tree")
# The whole shabang
cost_complexity_prune <- S7::new_generic("cost_complexity_prune", "tree")

# Data: X matrix, y vector

###   METHODS   ###

#' Compute the error of a subtree with root at a given node
#'
#' @name BinaryTree@get_subtree_error
#' @param tree A BinaryTree object
#' @param node A node object - the root of the subtree
#' @param y Numeric vector
#' @param X Numeric Matrix
#' @return Numeric
#' @export
S7::method(get_subtree_error, BinaryTree) <- function(tree, node, X, y) {
  # TODO
}

#' Find the weakest link in the tree
#'
#' Computes the cost-complexity factor (alpha)
#' for every internal node and returns the one with
#' the lowest value. vgl. Satz 6.19 but umformuliert.
#'
#' @name BinaryTree@get_weakest_link
#' @param tree  A BinaryTree object
#' @param X Matrix of predictor values
#' @param y Numeric vector of true response values
#' @return A Node object corresponding to the weakest link
#' @export
S7::method(get_weakest_link, BinaryTree) <- function(tree, X, y) {
  # TODO
}

# THIS HAS BEEN ADDED TO THEO'S CODEBASE
#' Prune a node by collapsing its subtree into a leaf
#'
#' @name BinaryTree@prune_node
#' @param tree  A BinaryTree object
#' @param node  A Node object to collapse
#' @param y Numeric vector of true response values for node's region
#' @return void
#' @export
S7::method(prune_node, BinaryTree) <- function(tree, node, y) {
  # TODO
}

#' Cost complexity pruning
#'
#' Constructs the full sequence T(0), T(1), ..., T(P) from Satz 6.19.
#' Returns all candidate trees with their associated alpha values.
#'
#' @name BinaryTree@cost_complexity_prune
#' @param tree  A BinaryTree object (fully grown)
#' @param X Matrix of predictor values
#' @param y Numeric vector of true response values
#' @return A list with elements: (trees, alphas)
#' @export
S7::method(cost_complexity_prune, BinaryTree) <- function(tree, X, y) {
  # TODO
}
