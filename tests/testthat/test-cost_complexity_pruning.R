# count_subtree_leaves
test_that("count_subtree_leaves returns 1 for a leaf node", {
  # TODO 
})
test_that("count_subtree_leaves returns correct count for small subtree", {
  # TODO 
})

# compute_leaf_prediction
test_that("compute_leaf_prediction returns mean for regression tree", {
  # TODO 
})
test_that("compute_leaf_prediction returns majority class for classification tree", {
  # TODO 
})

# compute_leaf_loss
test_that("compute_leaf_loss returns correct value for regression tree", {
  # TODO 
})
test_that("compute_leaf_loss returns correct value for classification tree", {
  # TODO 
})

# get_node_indices
test_that("get_node_indices returns all row indices for the root node", {
  # TODO
})
test_that("get_node_indices returns non-empty indices for all internal nodes", {
  # TODO
})

# compute_collapsed_error
test_that("compute_collapsed_error returns 0 for a subtree rooted at a leaf (single node so nothing to collapse)", {
  # TODO
})
test_that("compute_collapsed_error is >= compute_subtree_error", {
  # TODO
})
test_that("compute_collapsed_error equals subtree error for a leaf", {
  # TODO
})
 
# compute_subtree_error
test_that("compute_subtree_error returns 0 for a fully fitted tree (min_leaf_size=1)", {
  # TODO
})
test_that("compute_subtree_error decreases as tree grows", {
  # TODO
})

# get_internal_niodes
test_that("get_internal_nodes returns empty list for leaf", {
  # TODO 
})
test_that("get_internal_nodes returns internal nodes of a small subtree", {
  # TODO 
})

# compute_cost_complexity_ratio
test_that("compute_cost_complexity_ratio is non-negative", {
  # TODO
})

# find_weakest_link
test_that("find_weakest_link returns a list with node and cc_ratio", {
  # TODO
})
test_that("find_weakest_link returns NULL node for a single-leaf tree", {
  # TODO
})
test_that("find_weakest_link returns not NULL node for a tree which is not a single leaf", {
  # TODO
})
test_that("find_weakest_link works for classification", {
  # TODO
})

# cost_complexity_prune
test_that("cost_complexity_prune returns a list with trees and ratios", {
  # TODO
})
test_that("cost_complexity_prune sequence has one more tree than ratios", {
  # because T(0) is stored before any pruning, then one tree per pruning step
  # TODO
})
test_that("cost_complexity_prune pruning sequence reduces number of leaves", {
  # TODO
})
test_that("cost_complexity_prune last tree in sequence has only one leaf", {
  # TODO
})
test_that("cost_complexity_prune sequence has proper deep copies", {
  # Verify they don't point to the same object
  # TODO
})
