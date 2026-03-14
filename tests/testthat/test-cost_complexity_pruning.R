# Helpers

make_fitted_regression_cart <- function() {
  cart <- CART(type = "regression", max_depth = 3L, min_leaf_size = 1L)
  X    <- matrix(c(0.1, 0.2, 0.3, 0.7, 0.8, 0.9), ncol = 1)
  y    <- c(1.0, 1.2, 0.9, 5.1, 4.8, 5.3)
  fit(cart, X, y)
  list(cart = cart, X = X, y = y)
}
make_fitted_classification_cart <- function() {
  cart <- CART(type = "classification", max_depth = 3L, min_leaf_size = 1L)
  X    <- matrix(c(0.1, 0.2, 0.3, 0.7, 0.8, 0.9), ncol = 1)
  y    <- c(1L, 1L, 1L, 2L, 2L, 2L)
  fit(cart, X, y)
  list(cart = cart, X = X, y = y)
}
make_manual_tree <- function() {
  # Small predicatble tree
  #   x <= 0.5
  #    | \
  #   1  x <= 0.8
  #      /   \
  #     4     6
  leaf_left <- Node(leaf_value = 1.0)
  leaf_rl   <- Node(leaf_value = 4.0)
  leaf_rr   <- Node(leaf_value = 6.0)
  right     <- Node(s_feature = 1L, s_value = 0.8)
  set_left_child(right,  leaf_rl)
  set_right_child(right, leaf_rr)
  root      <- Node(ROOT = TRUE, s_feature = 1L, s_value = 0.5)
  set_left_child(root,  leaf_left)
  set_right_child(root, right)
  cart      <- CART(type = "regression", max_depth = 3L, min_leaf_size = 1L)
  cart@ref$root <- root
  X <- matrix(c(0.1, 0.6, 0.9), ncol = 1)
  y <- c(1.0, 4.0, 6.0)
  list(cart = cart, X = X, y = y, root = root,
       leaf_left = leaf_left, right = right,
       leaf_rl = leaf_rl, leaf_rr = leaf_rr)
}

# count_subtree_leaves
test_that("count_subtree_leaves returns 1 for a leaf node", {
  leaf <- Node(leaf_value = 1.0)
  testthat::expect_equal(count_subtree_leaves(leaf), 1L)
})
test_that("count_subtree_leaves returns correct count for small subtree", {
  m <- make_manual_tree()
  testthat::expect_equal(count_subtree_leaves(m$root), 3L)  # full tree
  testthat::expect_equal(count_subtree_leaves(m$right), 2L)  # right subtree only
  testthat::expect_equal(count_subtree_leaves(m$leaf_left), 1L)  # single leaf
})

# compute_leaf_prediction
test_that("compute_leaf_prediction returns mean for regression tree", {
  testthat::expect_equal(compute_leaf_prediction(c(1.0, 2.0, 3.0), "regression"), 2.0)
  testthat::expect_equal(compute_leaf_prediction(c(4.0, 4.0),      "regression"), 4.0)
})
test_that("compute_leaf_prediction returns majority class for classification tree", {
  testthat::expect_equal(compute_leaf_prediction(c(1L, 1L, 2L), "classification"), 1L)
  testthat::expect_equal(compute_leaf_prediction(c(2L, 2L, 1L), "classification"), 2L)
})

# compute_leaf_loss
test_that("compute_leaf_loss returns correct value for regression tree", {
  # (1-2)^2 + (2-2)^2 + (3-2)^2 = 2
  testthat::expect_equal(compute_leaf_loss(c(1.0, 2.0, 3.0), 2.0, "regression"), 2.0)
  testthat::expect_equal(compute_leaf_loss(c(5.0, 5.0), 5.0, "regression"), 0.0)
})
test_that("compute_leaf_loss returns correct value for classification tree", {
  # 1 misclassification out of 3
  testthat::expect_equal(compute_leaf_loss(c(1L, 1L, 2L), 1L, "classification"), 1L)
  testthat::expect_equal(compute_leaf_loss(c(1L, 1L, 1L), 1L, "classification"), 0L)
})

# get_node_indices
test_that("get_node_indices returns all row indices for the root node", {
  m       <- make_manual_tree()
  root    <- m$root
  indices <- get_node_indices(root, root, m$X)
  testthat::expect_equal(sort(indices), 1:nrow(m$X))
})
test_that("get_node_indices returns non-empty indices for all internal nodes", {
  m    <- make_fitted_regression_cart()
  root <- m$cart@ref$root

  left_idx  <- get_node_indices(root, get_left_child(root),  m$X)
  right_idx <- get_node_indices(root, get_right_child(root), m$X)

  # both sides get some observations
  testthat::expect_gt(length(left_idx),  0)
  testthat::expect_gt(length(right_idx), 0)

  # disjoint — no row goes both ways
  testthat::expect_equal(length(intersect(left_idx, right_idx)), 0)

  # together they cover everything
  testthat::expect_equal(sort(c(left_idx, right_idx)), 1:nrow(m$X))
})

# compute_collapsed_error
test_that("compute_collapsed_error returns 0 for a subtree rooted at a leaf (single node so nothing to collapse)", {
  cart <- CART(type = "regression", max_depth = 1L, min_leaf_size = 1L)
  X    <- matrix(c(0.5), ncol = 1)
  y    <- c(3.0)
  fit(cart, X, y)
  root  <- cart@ref$root
  error <- compute_collapsed_error(root, root, X, y, "regression")
  testthat::expect_equal(error, 0.0)
})
test_that("compute_collapsed_error is >= compute_subtree_error", {
  m         <- make_fitted_regression_cart()
  root      <- m$cart@ref$root
  collapsed <- compute_collapsed_error(root, root, m$X, m$y, "regression")
  subtree   <- compute_subtree_error(root,   root, m$X, m$y, "regression")
  # splitting can only reduce error, never increase it
  testthat::expect_gte(collapsed, subtree)
})
test_that("compute_collapsed_error equals subtree error for a leaf", {
  m    <- make_manual_tree()
  leaf <- m$leaf_left
  root <- m$root
  testthat::expect_equal(
    compute_collapsed_error(leaf, root, m$X, m$y, "regression"),
    compute_subtree_error(leaf,   root, m$X, m$y, "regression")
  )
})
 
# compute_subtree_error
test_that("compute_subtree_error returns 0 for a fully fitted tree (min_leaf_size=1)", {
  cart <- CART(type = "regression", max_depth = 10L, min_leaf_size = 1L)
  X    <- matrix(c(0.1, 0.2, 0.8, 0.9), ncol = 1)
  y    <- c(1.0, 1.0, 5.0, 5.0)   # two identical pairs => each leaf gets 2 identical y => RSS = 0
  fit(cart, X, y)
  root  <- cart@ref$root
  error <- compute_subtree_error(root, root, X, y, "regression")
  testthat::expect_lt(error, 1e-10)
})
test_that("compute_subtree_error decreases as tree grows", {
  X       <- matrix(seq(0.1, 0.9, by = 0.1), ncol = 1)
  y       <- as.double(seq_len(9))
  shallow <- CART(type = "regression", max_depth = 1L, min_leaf_size = 1L)
  deep    <- CART(type = "regression", max_depth = 4L, min_leaf_size = 1L)
  fit(shallow, X, y)
  fit(deep,    X, y)
  err_shallow <- compute_subtree_error(shallow@ref$root, shallow@ref$root, X, y, "regression")
  err_deep    <- compute_subtree_error(deep@ref$root,    deep@ref$root,    X, y, "regression")
  testthat::expect_gte(err_shallow, err_deep)
})

# get_internal_nodes
test_that("get_internal_nodes returns empty list for leaf", {
  leaf   <- Node(leaf_value = 1.0)
  result <- get_internal_nodes(leaf)
  testthat::expect_equal(length(result), 0)
})
test_that("get_internal_nodes returns internal nodes of a small subtree", {
  m      <- make_manual_tree()
  result <- get_internal_nodes(m$root)
  # manual tree has exactly 2 internal nodes: root and right
  testthat::expect_equal(length(result), 2)
  # none of the returned nodes should be leaves
  for (node in result) {
    testthat::expect_false(is_leaf(node))
  }
})

# compute_cost_complexity_ratio
test_that("compute_cost_complexity_ratio is non-negative", {
  m     <- make_fitted_regression_cart()
  root  <- m$cart@ref$root
  ratio <- compute_cost_complexity_ratio(root, root, m$X, m$y, "regression")
  testthat::expect_gte(ratio, 0)
})

# find_weakest_link
test_that("find_weakest_link returns a list with node and cc_ratio", {
  m      <- make_fitted_regression_cart()
  result <- find_weakest_link(m$cart, m$X, m$y)
  testthat::expect_type(result, "list")
  testthat::expect_true(all(c("node", "cc_ratio") %in% names(result)))
})
test_that("find_weakest_link returns NULL node for a single-leaf tree", {
  # max_depth=0 forces a single root leaf
  cart <- CART(type = "regression", max_depth = 0L, min_leaf_size = 1L)
  X    <- matrix(c(0.1, 0.9), ncol = 1)
  y    <- c(1.0, 5.0)
  fit(cart, X, y)
  result <- find_weakest_link(cart, X, y)
  testthat::expect_null(result$node)
})
test_that("find_weakest_link returns not NULL node for a tree which is not a single leaf", {
  m      <- make_fitted_regression_cart()
  result <- find_weakest_link(m$cart, m$X, m$y)
  testthat::expect_false(is.null(result$node))
  testthat::expect_true(S7::S7_inherits(result$node, Node))
})
test_that("find_weakest_link works for classification", {
  m      <- make_fitted_classification_cart()
  result <- find_weakest_link(m$cart, m$X, m$y)
  testthat::expect_false(is.null(result$node))
  testthat::expect_gte(result$cc_ratio, 0)
})

# cost_complexity_prune
test_that("cost_complexity_prune returns a list with trees and ratios", {
  m      <- make_fitted_regression_cart()
  result <- cost_complexity_prune(m$cart, m$X, m$y)
  testthat::expect_type(result, "list")
  testthat::expect_true(all(c("trees", "ratios") %in% names(result)))
})
test_that("cost_complexity_prune sequence has one more tree than ratios", {
  # T(0) stored before any pruning => always one extra tree
  m      <- make_fitted_regression_cart()
  result <- cost_complexity_prune(m$cart, m$X, m$y)
  testthat::expect_equal(length(result$trees), length(result$ratios) + 1)
})
test_that("cost_complexity_prune pruning sequence reduces number of leaves", {
  m           <- make_fitted_regression_cart()
  result      <- cost_complexity_prune(m$cart, m$X, m$y)
  leaf_counts <- sapply(result$trees, function(t) count_subtree_leaves(t@ref$root))
  # every step must reduce leaf count
  testthat::expect_true(all(diff(leaf_counts) < 0))
})
test_that("cost_complexity_prune last tree in sequence has only one leaf", {
  m      <- make_fitted_regression_cart()
  result <- cost_complexity_prune(m$cart, m$X, m$y)
  last   <- result$trees[[length(result$trees)]]
  testthat::expect_equal(count_subtree_leaves(last@ref$root), 1L)
})
test_that("cost_complexity_prune sequence has proper deep copies", {
  # if serialize/unserialize failed, all snapshots would share the same
  # environment and point to the same object
  m      <- make_fitted_regression_cart()
  result <- cost_complexity_prune(m$cart, m$X, m$y)
  counts <- sapply(result$trees, function(t) count_subtree_leaves(t@ref$root))
  testthat::expect_gt(length(unique(counts)), 1)
})
