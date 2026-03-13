# cost_complexity_pruning.R

###     GENERICS      ###

find_weakest_link <- S7::new_generic("find_weakest_link", "cart")
cost_complexity_prune <- S7::new_generic("cost_complexity_prune", "cart")

###     HELPERS     ###

#' Count the number of leaves in a subtree rooted at given node
#'
#' @param node A Node object
#' @return Integer number of leaves
#' 
#' @examples
#' count_subtree_leaves(node)
count_subtree_leaves <- function(node) {
  if (is_leaf(node)) return(1L)
  return(count_subtree_leaves(get_left_child(node)) + count_subtree_leaves(get_right_child(node)))
}

#' Compute a leaf prediction from target values
#'
#' regression: mean
#' classification: majority class
#'
#' @param values Numeric vector of response values
#' @param type "regression" or "classification"
#'
#' @return Numeric predicted leaf value.
#' @examples
#' compute_leaf_prediction(y, "regression")
compute_leaf_prediction <- function(values, type) {
  if (type == "classification") {
    return(as.integer(which.max(tabulate(values))))
  }
  mean(values)
}

#' Compute loss for a leaf prediction
#'
#' regression: squared error loss
#' classification: misclassification count
#'
#' @param values True response values
#' @param prediction Leaf prediction value
#' @param type Tree type ("regression" or "classification")
#'
#' @return Numeric loss value
#' @eaxmples
#' compute_leaf_loss(y, mean(y), "regression")
compute_leaf_loss <- function(values, prediction, type) {
  if (length(values) == 0) return(0)

  if (type == "classification") {
    return(sum(values != prediction))
  }

  sum((values - prediction)^2)
}

#' Find indices of data rows that reach a node given
#'
#' Goes dwon the tree from the root and returns the indices of the
#' data rows that would reach the specified node
#'
#' @param root Root of the tree
#' @param node Target node
#' @param X Data matrix
#' @param indices Unfiltered row indices
#'
#' @return Integer vector of observation indices.
#'
#' @examples
#' get_node_indices(root, node, X)
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

#' Compute the error R(t) if a subtree was collapsed into a single leaf
#'
#' @param node A Node object representing the subtree root
#' @param root Root node of the tree
#' @param X Numeric feature matrix
#' @param y Numeric target vector
#' @param type Tree type ("regression" or "classification")
#'
#' @return Numeric error value
#'
#' @examples
#' compute_collapsed_error(node, root, X, y, "regression")
compute_collapsed_error <- function(node, root, X, y, type) {
  # Get the rows
  indexes <- get_node_indices(root, node, X)

  if (length(indexes) == 0) return(0)
  # Mean prediction
  prediction <- compute_leaf_prediction(y[indexes], type)
  return(compute_leaf_loss(y[indexes], prediction, type))
}

#' Compute the total error of a subtree
#'
#' Recursively computes the total error R(T_t) for the subtree rooted at node t
#' (the sum of losses across all leaf nodes in the subtree)
#'
#' @param node A Node object
#' @param root Root node of the tree
#' @param X Numeric feature matrix
#' @param y Numeric target vector
#' @param type Tree type ("regression" or "classification")
#'
#' @return Numeric error value
#' @eaxmples
#' compute_collapsed_error(node, root, X, y, type)
compute_subtree_error <- function(node, root, X, y, type) {
  if (is_leaf(node)) return(compute_collapsed_error(node, root, X, y, type))
  return(
    compute_subtree_error(get_left_child(node),  root, X, y, type)
    + compute_subtree_error(get_right_child(node), root, X, y, type)
  )
}

#' Get all internal nodes of a subtree (Helper function)
#'
#' Recursively collects all internal (non-leaf) nodes of a subtree
#' This helper is used when searching for the weakest link during
#' cost-complexity pruning
#'
#' @param node Root of the subtree
#' @param nodes List just used for recursion
#'
#' @return List of Node objects
#'
#' @examples
#' get_internal_nodes(cart@ref$root)
get_internal_nodes <- function (node, nodes = list()) {
    if (is_leaf(node)) return(nodes)

    nodes <- c(nodes, list(node))

    nodes <- get_internal_nodes(get_left_child(node), nodes)
    nodes <- get_internal_nodes(get_right_child(node), nodes)

    return(nodes)
}

#' Compute the cost-complexity ratio for a node
#'
#' Computes the factor  (R(t) - R(T_t))/(#T_t - 1) from Satz 6.18/6.19, where
#' - R(t) error if the subtree rooted at t gets replaced by leaf
#' - R(T_t) total error in the node's subtree
#' - #T_t number of leaves in the node's subtree
#'
#' @param node Node whose pruning cost is evaluated
#' @param root Root node of the tree
#' @param X Numeric feature matrix
#' @param y Numeric target vector
#' @param type Tree type ("regression" or "classification")
#'
#' @return Numeric cost-complexity ratio
#'
#' @examples
compute_cost_complexity_ratio <- function(node, root, X, y, type) {
  R_t   <- compute_collapsed_error(node, root, X, y, type)
  R_T_t <- compute_subtree_error(node,   root, X, y, type)
  leaves <- count_subtree_leaves(node)
  return((R_t - R_T_t) / (leaves - 1L)) # Possible zero division?
}

###     METHODS     ###

#' Find the weakest link in a CART tree
#'
#' Identifies the internal node whose removal results in the smallest cost-complexity ratio.
#' 
#' @name CART@find_weakest_link
#'
#' @param cart A CART object
#' @param X Numeric feature matrix used for training
#' @param y Numeric response vector
#'
#' @return A list containing
#' - A Node object: the weakest link
#' - Numeric: cc_ratio the minimum cost-complexity ratio
#'
#' @examples
#' find_weakest_link(cart, X, y)
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
 

#' Perform cost-complexity pruning on a CART tree
#'
#' Starts from a fully grown tree, repeatedly removes the weakest links.
#'
#' The result is a sequence of subtrees from Satz 6.19
#' together with their corresponding complexity ratios.
#'
#' Trees are deep-copied using serialization in order to store
#' intermediate pruning states, since the underlying data structures use environments.
#'
#' @name CART@cost_complexity_prune
#'
#' @param cart A CART object
#' @param X Numeric feature matrix
#' @param y Numeric response vector
#'
#' @return A list containing
#' - trees: List of pruned CART trees
#' - ratios: Numeric vector of cost-complexity parameters
#' }
#'
#' @examples
#' pruned <- cost_complexity_prune(cart, X, y)
#' pruned$trees
#' pruned$ratios
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
